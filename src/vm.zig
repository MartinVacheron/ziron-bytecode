const std = @import("std");
const Allocator = std.mem.Allocator;
const print = std.debug.print;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("values.zig").Value;
const ByteCodeGen = @import("compiler.zig").ByteCodeGen;
const Compiler = @import("compiler.zig").Compiler;
const Obj = @import("obj.zig").Obj;
const ObjFunction = @import("obj.zig").ObjFunction;
const ObjIter = @import("obj.zig").ObjIter;
const ObjString = @import("obj.zig").ObjString;
const ObjNativeFn = @import("obj.zig").ObjNativeFn;
const NativeFn = @import("obj.zig").NativeFn;
const Table = @import("table.zig").Table;
const config = @import("config");

fn clock_native(_: []const Value) Value {
    return .{ .Float = @as(f64, @floatFromInt(std.time.milliTimestamp())) / 1000.0 };
}

const CallFrame = struct {
    function: *ObjFunction,
    ip: [*]u8,
    slots: [*]Value,

    const Self = @This();

    pub fn read_byte(self: *Self) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    pub fn read_constant(self: *Self) Value {
        return self.function.chunk.constants.items[self.read_byte()];
    }

    pub fn read_string(self: *Self) *ObjString {
        return self.read_constant().as_obj().?.as(ObjString);
    }

    pub fn read_short(self: *Self) u16 {
        const part1 = self.read_byte();
        const part2 = self.read_byte();

        return (@as(u16, part1) << 8) | part2;
    }
};

const FrameStack = struct {
    frames: [FRAMES_MAX]CallFrame,
    count: usize,

    const Self = @This();
    const FRAMES_MAX: u8 = 64;

    pub fn new() Self {
        return .{
            .frames = undefined, // NOTE: if working, investigate perf (compare to value stack)
            .count = 0,
        };
    }
};

// PERF: check BoundedArray?
const Stack = struct {
    values: [STACK_SIZE]Value,
    top: [*]Value,

    const STACK_SIZE: u16 = @as(u16, FrameStack.FRAMES_MAX) * @as(u16, std.math.maxInt(u8));
    const Self = @This();

    pub fn new() Self {
        return .{
            .values = [_]Value{.{ .Null = undefined }} ** STACK_SIZE,
            .top = undefined,
        };
    }

    pub fn init(self: *Self) void {
        self.top = self.values[0..].ptr;
    }

    fn push(self: *Self, value: Value) void {
        self.top[0] = value;
        self.top += 1;
    }

    fn pop(self: *Self) Value {
        self.top -= 1;
        return self.top[0];
    }

    fn peek(self: *const Self, distance: usize) Value {
        // Pointer arithmetic
        return (self.top - 1 - distance)[0];
    }
};

pub const Vm = struct {
    stack: Stack,
    bytecode_gen: ByteCodeGen,
    allocator: Allocator,
    objects: ?*Obj,
    strings: Table,
    globals: Table,
    frame_stack: FrameStack,

    const Self = @This();

    const VmErr = error{
        RuntimeErr,
    } || Compiler.CompileErr || std.fmt.BufPrintError;

    pub fn new(allocator: Allocator) Self {
        return .{
            .stack = Stack.new(),
            .bytecode_gen = ByteCodeGen.new(),
            .allocator = allocator,
            .objects = null,
            .strings = Table.init(allocator),
            .globals = Table.init(allocator),
            .frame_stack = FrameStack.new(),
        };
    }

    pub fn init(self: *Self) Allocator.Error!void {
        self.stack.init();
        try self.define_native("clock", clock_native);
    }

    pub fn deinit(self: *Self) void {
        self.free_objects();
        self.strings.deinit();
        self.globals.deinit();
    }

    fn free_objects(self: *Self) void {
        var object = self.objects;
        while (object) |obj| {
            const next = obj.next;
            self.free_object(obj);
            object = next;
        }
    }

    fn free_object(self: *Self, object: *Obj) void {
        switch (object.kind) {
            .Fn => {
                const function = object.as(ObjFunction);
                function.deinit(self.allocator);
            },
            // NOTE: does iter really needs to be an Obj?
            // check memory footprint on the tagged union if
            // we put it with the others Value
            .Iter => {
                const iter = object.as(ObjIter);
                self.allocator.destroy(iter);
            },
            .NativeFn => {
                const function = object.as(ObjNativeFn);
                function.deinit(self.allocator);
            },
            .String => {
                const string = object.as(ObjString);
                string.deinit(self.allocator);
            },
        }
    }

    pub fn interpret(self: *Self, source: []const u8) VmErr!void {
        const function = self.bytecode_gen.compile(self, source) catch |e| {
            print("{}\n", .{e});
            return;
        };
        self.stack.push(Value.obj(function.as_obj()));

        try self.call(function, 0);
        try self.run();
    }

    pub fn run(self: *Self) VmErr!void {
        while (true) {
            if (config.PRINT_STACK) {
                print("          ", .{});

                var value = self.stack.values[0..].ptr;
                while (value != self.stack.top) : (value += 1) {
                    print("[", .{});
                    value[0].print(std.debug);
                    print("] ", .{});
                }
                print("\n", .{});
            }

            // PERF: investigate to use a pointer to top frame (as value stack)
            var frame = &self.frame_stack.frames[self.frame_stack.count - 1];

            // as it is known as comptime (const via build.zig), not compiled
            // if not needed (equivalent of #ifdef or #[cfg(feature...)])
            if (config.TRACING) {
                const Dis = @import("disassembler.zig").Disassembler;
                const dis = Dis.init(&frame.function.chunk);
                _ = dis.dis_instruction(self.instruction_nb());
            }

            const instruction = frame.read_byte();
            const op_code: OpCode = @enumFromInt(instruction);

            switch (op_code) {
                .Add => self.stack.push(try self.binop('+')),
                .Call => {
                    // Number of args
                    const arg_count = frame.read_byte();
                    // peek(arg_count) gives back the function obj, which is before args
                    try self.call_value(self.stack.peek(arg_count), arg_count);

                    frame = &self.frame_stack.frames[self.frame_stack.count - 1];
                },
                .Constant => {
                    const value = frame.read_constant();
                    self.stack.push(value);
                },
                .CreateIter => {
                    // No runtime check of type
                    const iter_end = self.stack.pop().Int;

                    // Placeholder value (same as local idx)
                    self.stack.push(Value.int(0));

                    // Compiler checked if it was an int
                    const iter = try ObjIter.create(self, iter_end);
                    self.stack.push(Value.obj(iter.as_obj()));
                },
                .DefineGlobal => {
                    const name = frame.read_string();
                    // NOTE: we don't check if field exists already, we can redefine same global
                    _ = try self.globals.set(name, self.stack.pop());
                },
                .Divide => self.stack.push(try self.binop('/')),
                .Equal => {
                    const v1 = self.stack.pop();
                    const v2 = self.stack.pop();
                    self.stack.push(Value.bool_(v1.equals(v2)));
                },
                .False => self.stack.push(Value.bool_(false)),
                .ForIter => {
                    const jump = frame.read_short();
                    const iter_index = frame.read_byte();

                    // const iter = self.stack.values[iter_index].as_obj().?.as(ObjIter);
                    const iter = frame.slots[iter_index].as_obj().?.as(ObjIter);

                    if (iter.next()) |i| {
                        // self.stack.values[iter_index - 1].Int = i;
                        frame.slots[iter_index - 1].Int = i;
                    } else {
                        frame.ip += jump;
                    }
                },
                // PERF: lookup differently the globals, like locals. Much more faster
                // https://craftinginterpreters.com/global-variables.html#challenges [2]
                .GetGlobal => {
                    const name = frame.read_string();
                    const value = self.globals.get(name) orelse {
                        var buf: [250]u8 = undefined;
                        _ = try std.fmt.bufPrint(&buf, "undeclared variable: {s}\n", .{name.chars});
                        self.runtime_err(&buf);
                        return error.RuntimeErr;
                    };

                    self.stack.push(value);
                },
                .GetLocal => {
                    const index = frame.read_byte();
                    self.stack.push(frame.slots[index]);
                },
                .Greater => self.stack.push(try self.binop('>')),
                .Jump => {
                    const offset = frame.read_short();
                    frame.ip += offset;
                },
                .JumpIfFalse => {
                    const jump = frame.read_short();
                    const condition = self.stack.peek(0).as_bool() orelse {
                        self.runtime_err("condition must be a bool");
                        return error.RuntimeErr;
                    };

                    if (!condition) frame.ip += jump;
                },
                .Less => self.stack.push(try self.binop('<')),
                .Loop => {
                    // Must be in two part as read_short modifies ip
                    const offset = frame.read_short();
                    frame.ip -= offset;
                },
                .Multiply => self.stack.push(try self.binop('*')),
                .Negate => {
                    // PERF: https://craftinginterpreters.com/a-virtual-machine.html#challenges [4]
                    const value = self.stack.pop();
                    switch (value) {
                        .Int => |v| self.stack.push(Value.int(-v)),
                        .Float => |v| self.stack.push(Value.float(-v)),
                        else => self.runtime_err("operand must be a number"),
                    }
                },
                .Null => self.stack.push(Value.null_()),
                .Not => {
                    const value = self.stack.pop().as_bool() orelse {
                        self.runtime_err("operator '!' can only be used with bool operand");
                        return error.RuntimeErr;
                    };
                    self.stack.push(Value.bool_(!value));
                },
                .Print => {
                    const value = self.stack.pop();
                    value.print(std.debug);
                    print("\n", .{});
                },
                .Pop => _ = self.stack.pop(),
                .Return => {
                    const result = self.stack.pop();
                    self.frame_stack.count -= 1;

                    if (self.frame_stack.count == 0) {
                        // Main script object function
                        _ = self.stack.pop();
                        return;
                    }

                    // We set back the top at the beginning of the function
                    // stack window, discarding all the parameters
                    self.stack.top = frame.slots;
                    self.stack.push(result);
                    frame = &self.frame_stack.frames[self.frame_stack.count - 1];
                },
                // NOTE: we don't check if global already exists...
                .SetGlobal => {
                    const name = frame.read_string();

                    // Peek here because assignemnt is an expression, we leave it on the stack
                    if (try self.globals.set(name, self.stack.peek(0))) {
                        _ = self.globals.delete(name);

                        var buf: [250]u8 = undefined;
                        _ = try std.fmt.bufPrint(&buf, "undeclared variable: {s}\n", .{name.chars});
                        self.runtime_err(&buf);
                        return error.RuntimeErr;
                    }
                },
                .SetLocal => {
                    const index = frame.read_byte();
                    frame.slots[index] = self.stack.peek(0);
                },
                .Subtract => self.stack.push(try self.binop('-')),
                .True => self.stack.push(Value.bool_(true)),
            }
        }
    }

    fn call_value(self: *Self, callee: Value, arg_count: u8) VmErr!void {
        // NOTE: type check forced because dynamically typed
        if (callee.as_obj()) |obj| {
            switch (obj.kind) {
                .Fn => return self.call(obj.as(ObjFunction), arg_count),
                .NativeFn => {
                    const function = obj.as(ObjNativeFn).function;
                    const args = (self.stack.top - arg_count)[0..arg_count];
                    const result = function(args);
                    self.stack.top -= arg_count + 1;
                    self.stack.push(result);
                    return;
                },
                else => {},
            }
        }

        self.runtime_err("can only call function and structures");
        return VmErr.RuntimeErr;
    }

    fn call(self: *Self, function: *ObjFunction, arg_count: u8) VmErr!void {
        // NOTE: check necessary because one pass compiler
        if (arg_count != function.arity) {
            var buf: [250]u8 = undefined;
            _ = try std.fmt.bufPrint(&buf, "expected {} arguments but got {}\n", .{ function.arity, arg_count });
            self.runtime_err(&buf);
            return error.RuntimeErr;
        }

        if (self.frame_stack.count == FrameStack.FRAMES_MAX) {
            self.runtime_err("stack overflow");
            return VmErr.RuntimeErr;
        }

        const frame = &self.frame_stack.frames[self.frame_stack.count];
        frame.function = function;
        frame.ip = function.chunk.code.items.ptr;
        // Pointer arithmetic, -1 to go before function itself
        frame.slots = self.stack.top - arg_count - 1;
        self.frame_stack.count += 1;
    }

    fn binop(self: *Self, op: u8) Allocator.Error!Value {
        const v2 = self.stack.pop();
        const v1 = self.stack.pop();

        if (v1 == .Obj and v2 == .Obj) {
            return try self.concatenate(v1, v2);
        }

        if ((v1 != .Int and v1 != .Float) or (v2 != .Int and v2 != .Float)) {
            self.runtime_err("binary operation only allowed between ints or floats");
            return Value.null_();
        }

        if ((v1 == .Int and v2 != .Int) or (v1 == .Float and v2 != .Float)) {
            self.runtime_err("binary operation must be on two ints or two floats");
            return Value.null_();
        }

        if (v1 == .Int and v2 == .Int) {
            return switch (op) {
                '+' => Value.int(v1.Int + v2.Int),
                '-' => .{ .Int = v1.Int - v2.Int },
                '*' => .{ .Int = v1.Int * v2.Int },
                '/' => .{ .Int = @divTrunc(v1.Int, v2.Int) },
                '<' => Value.bool_(v1.Int < v2.Int),
                '>' => Value.bool_(v1.Int > v2.Int),
                else => unreachable,
            };
        }

        if (v1 == .Float and v2 == .Float) {
            return switch (op) {
                '+' => .{ .Float = v1.Float + v2.Float },
                '-' => .{ .Float = v1.Float - v2.Float },
                '*' => .{ .Float = v1.Float * v2.Float },
                '/' => .{ .Float = v1.Float / v2.Float },
                '<' => Value.bool_(v1.Float < v2.Float),
                '>' => Value.bool_(v1.Float > v2.Float),
                else => unreachable,
            };
        }

        unreachable;
    }

    fn concatenate(self: *Self, str1: Value, str2: Value) Allocator.Error!Value {
        const obj1 = str1.as_obj().?.as(ObjString);
        const obj2 = str2.as_obj().?.as(ObjString);

        const res = try self.allocator.alloc(u8, obj1.chars.len + obj2.chars.len);
        @memcpy(res[0..obj1.chars.len], obj1.chars);
        @memcpy(res[obj1.chars.len..], obj2.chars);

        return Value.obj((try ObjString.take(self, res)).as_obj());
    }

    // Here, we put them in the stack because a GC could be triggered, so putting them
    // in the stack ensure that the GC dosen't free them before use
    fn define_native(self: *Self, name: []const u8, function: NativeFn) Allocator.Error!void {
        self.stack.push(Value.obj((try ObjString.copy(self, name)).as_obj()));
        self.stack.push(Value.obj((try ObjNativeFn.create(self, function)).as_obj()));
        _ = try self.globals.set(self.stack.values[0].as_obj().?.as(ObjString), self.stack.values[1]);
        _ = self.stack.pop();
        _ = self.stack.pop();
    }

    fn runtime_err(self: *const Self, msg: []const u8) void {
        var frame = &self.frame_stack.frames[self.frame_stack.count - 1];
        var line = frame.function.chunk.lines.items[self.instruction_nb()];

        print("[line {}] in script: {s}\n", .{ line, msg });

        for (0..self.frame_stack.count - 1) |i| {
            frame = &self.frame_stack.frames[self.frame_stack.count - i];
            const function = frame.function;
            // -1 because we are already on the next instruction at this point
            // and we want the one who failed
            const addr1 = @intFromPtr(frame.ip - 1);
            const addr2 = @intFromPtr(function.chunk.code.items.ptr);
            const instruction = addr1 - addr2;
            line = function.chunk.lines.items[instruction];
            print("[line {}] in ", .{line});

            if (function.name) |n| {
                print("{}()\n", .{n});
            } else {
                print("script\n", .{});
            }
        }
    }

    // Only used in debug or runtime error, no performance required
    fn instruction_nb(self: *const Self) usize {
        const frame = &self.frame_stack.frames[self.frame_stack.count - 1];
        const addr1 = @intFromPtr(frame.ip);
        const addr2 = @intFromPtr(frame.function.chunk.code.items.ptr);
        return addr1 - addr2;
    }
};

test "interning" {
    const allocator = std.testing.allocator;

    var vm = Vm.new(allocator);
    vm.init();
    defer vm.deinit();

    const str1 = try vm.allocator.alloc(u8, 4);
    @memcpy(str1, "mars");
    const str2 = try vm.allocator.alloc(u8, 4);
    @memcpy(str2, "mars");

    _ = try ObjString.take(&vm, str1);
    _ = try ObjString.take(&vm, str2);

    try std.testing.expectEqual(vm.strings.count, 1);

    const str3 = try vm.allocator.alloc(u8, 4);
    @memcpy(str3, "moon");
    _ = try ObjString.take(&vm, str3);

    try std.testing.expectEqual(vm.strings.count, 2);

    _ = try ObjString.copy(&vm, "moon");
    try std.testing.expectEqual(vm.strings.count, 2);

    _ = try ObjString.copy(&vm, "jupyter");
    try std.testing.expectEqual(vm.strings.count, 3);
}
