const std = @import("std");
const Allocator = std.mem.Allocator;
const print = std.debug.print;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("values.zig").Value;
const Compiler = @import("compiler.zig").Compiler;
const Obj = @import("obj.zig").Obj;
const ObjString = @import("obj.zig").ObjString;
const ObjIter = @import("obj.zig").ObjIter;
const Table = @import("table.zig").Table;
const config = @import("config");

const Stack = struct {
    values: [STACK_SIZE]Value,
    top: [*]Value,

    const STACK_SIZE: u8 = std.math.maxInt(u8);
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
    chunk: Chunk,
    ip: [*]u8,
    stack: Stack,
    compiler: Compiler,
    allocator: Allocator,
    objects: ?*Obj,
    strings: Table,
    globals: Table,

    const Self = @This();

    const VmErr = error{
        RuntimeErr,
    } || Compiler.CompileErr || std.fmt.BufPrintError;

    pub fn new(allocator: Allocator) Self {
        return .{
            .chunk = Chunk.init(allocator),
            .ip = undefined,
            .stack = Stack.new(),
            .compiler = Compiler.new(),
            .allocator = allocator,
            .objects = null,
            .strings = Table.init(allocator),
            .globals = Table.init(allocator),
        };
    }

    pub fn init(self: *Self) void {
        self.stack.init();
        self.compiler.init(self);
    }

    pub fn deinit(self: *Self) void {
        self.chunk.deinit();
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
            // NOTE: does iter really needs to be an Obj?
            // check memory footprint on the tagged union if
            // we put it with the others Value
            .Iter => {
                const iter = object.as(ObjIter);
                self.allocator.destroy(iter);
            },
            .String => {
                const string = object.as(ObjString);
                self.allocator.free(string.chars);
                self.allocator.destroy(string);
            },
        }
    }

    fn read_byte(self: *Self) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    fn read_constant(self: *Self) Value {
        return self.chunk.constants.items[self.read_byte()];
    }

    fn read_string(self: *Self) *ObjString {
        return self.read_constant().as_obj().?.as(ObjString);
    }

    fn read_short(self: *Self) u16 {
        const part1 = self.read_byte();
        const part2 = self.read_byte();

        return (@as(u16, part1) << 8) | part2;
    }

    pub fn interpret(self: *Self, source: []const u8) VmErr!void {
        self.chunk.code.clearRetainingCapacity();
        try self.compiler.compile(source, &self.chunk);
        self.ip = self.chunk.code.items.ptr;

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

            // as it is known as comptime (const via build.zig), not compiled
            // if not needed (equivalent of #ifdef or #[cfg(feature...)])
            if (config.TRACING) {
                const Dis = @import("disassembler.zig").Disassembler;
                const dis = Dis.init(&self.chunk);
                _ = dis.dis_instruction(self.instruction_nb());
            }

            const instruction = self.read_byte();
            const op_code: OpCode = @enumFromInt(instruction);

            switch (op_code) {
                .Add => self.stack.push(try self.binop('+')),
                .Constant => {
                    const value = self.read_constant();
                    self.stack.push(value);
                },
                .CreateIter => {
                    const iter_end = self.stack.pop().Int;

                    // Placeholder value (same as local idx)
                    self.stack.push(Value.int(0));

                    // Compiler checked if it was an int
                    const iter = try ObjIter.create(self, iter_end);
                    self.stack.push(Value.obj(iter.as_obj()));
                },
                .DefineGlobal => {
                    const name = self.read_string();
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
                    const jump = self.read_short();
                    const iter_index = self.read_byte();

                    const iter = self.stack.values[iter_index].as_obj().?.as(ObjIter);

                    if (iter.next()) |i| {
                        self.stack.values[iter_index - 1].Int = i;
                    } else {
                        self.ip += jump;
                    }
                },
                // PERF: lookup differently the globals, like locals. Much more faster
                // https://craftinginterpreters.com/global-variables.html#challenges [2]
                .GetGlobal => {
                    const name = self.read_string();
                    const value = self.globals.get(name) orelse {
                        var buf: [250]u8 = undefined;
                        _ = try std.fmt.bufPrint(&buf, "undeclared variable: {s}\n", .{name.chars});
                        self.runtime_err(&buf);
                        return error.RuntimeErr;
                    };

                    self.stack.push(value);
                },
                .GetLocal => {
                    const index = self.read_byte();
                    self.stack.push(self.stack.values[index]);
                },
                .Greater => self.stack.push(try self.binop('>')),
                .Jump => {
                    const offset = self.read_short();
                    self.ip += offset;
                },
                .JumpIfFalse => {
                    const jump = self.read_short();
                    const condition = self.stack.peek(0).as_bool() orelse {
                        self.runtime_err("condition must be a bool");
                        return error.RuntimeErr;
                    };

                    if (!condition) self.ip += jump;
                },
                .Less => self.stack.push(try self.binop('<')),
                .Loop => {
                    // Must be in two part as read_short modifies ip
                    const offset = self.read_short();
                    self.ip -= offset;
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
                .Return => return,
                .SetGlobal => {
                    const name = self.read_string();

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
                    const index = self.read_byte();
                    self.stack.values[index] = self.stack.peek(0);
                },
                .Subtract => self.stack.push(try self.binop('-')),
                .True => self.stack.push(Value.bool_(true)),
            }
        }
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

    fn runtime_err(self: *const Self, msg: []const u8) void {
        const line = self.chunk.lines.items[self.instruction_nb()];
        print("[line {}] in script: {s}\n", .{ line, msg });
        // TODO: in repl mode, the stack is corrupted if we don't stop execution
    }

    fn instruction_nb(self: *const Self) usize {
        const addr1 = @intFromPtr(self.ip);
        const addr2 = @intFromPtr(self.chunk.code.items.ptr);
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
