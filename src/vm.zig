const std = @import("std");
const Allocator = std.mem.Allocator;
const print = std.debug.print;
const config = @import("config");
const Gc = @import("gc.zig").Gc;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("values.zig").Value;
const Compiler = @import("compiler.zig").Compiler;
const compile = @import("compiler.zig").compile;
const Obj = @import("obj.zig").Obj;
const ObjFunction = @import("obj.zig").ObjFunction;
const ObjIter = @import("obj.zig").ObjIter;
const ObjString = @import("obj.zig").ObjString;
const ObjNativeFn = @import("obj.zig").ObjNativeFn;
const ObjClosure = @import("obj.zig").ObjClosure;
const ObjStruct = @import("obj.zig").ObjStruct;
const ObjInstance = @import("obj.zig").ObjInstance;
const ObjUpValue = @import("obj.zig").ObjUpValue;
const ObjBoundMethod = @import("obj.zig").ObjBoundMethod;
const NativeFn = @import("obj.zig").NativeFn;
const Table = @import("table.zig").Table;

fn clock_native(_: []const Value) Value {
    return .{ .Float = @as(f64, @floatFromInt(std.time.milliTimestamp())) / 1000.0 };
}

const CallFrame = struct {
    closure: *ObjClosure,
    ip: [*]u8,
    slots: [*]Value,

    const Self = @This();

    pub fn read_byte(self: *Self) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    pub fn read_constant(self: *Self) Value {
        return self.closure.function.chunk.constants.items[self.read_byte()];
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

    pub fn push(self: *Self, value: Value) void {
        self.top[0] = value;
        self.top += 1;
    }

    pub fn pop(self: *Self) Value {
        self.top -= 1;
        return self.top[0];
    }

    fn peek(self: *const Self, distance: usize) Value {
        // Pointer arithmetic
        return (self.top - 1 - distance)[0];
    }
};

pub const Vm = struct {
    gc: Gc,
    allocator: Allocator,
    stack: Stack,
    compiler: *Compiler,
    objects: ?*Obj,
    strings: Table,
    globals: Table,
    frame_stack: FrameStack,
    open_upvalues: ?*ObjUpValue,
    stdout: std.fs.File.Writer,
    init_string: *ObjString,

    const Self = @This();

    const VmErr = error{
        RuntimeErr,
    } || Compiler.CompileErr || std.fmt.BufPrintError || std.posix.WriteError;

    pub fn new(allocator: Allocator) Self {
        return .{
            .gc = Gc.init(allocator),
            .allocator = undefined,
            .stack = Stack.new(),
            .compiler = undefined,
            .objects = null,
            .strings = undefined,
            .globals = undefined,
            .frame_stack = FrameStack.new(),
            .open_upvalues = null,
            .stdout = std.io.getStdOut().writer(),
            .init_string = undefined,
        };
    }

    pub fn init(self: *Self) Allocator.Error!void {
        self.gc.link(self);
        self.allocator = self.gc.allocator();

        self.stack.init();
        self.strings = Table.init(self.gc.allocator());
        self.globals = Table.init(self.gc.allocator());
        self.init_string = try ObjString.copy(self, "init");

        try self.define_native("clock", clock_native);
    }

    pub fn deinit(self: *Self) void {
        self.strings.deinit();
        self.globals.deinit();
        self.free_objects();
        self.gc.deinit();
    }

    fn free_objects(self: *Self) void {
        var object = self.objects;
        while (object) |obj| {
            const next = obj.next;
            obj.destroy(self);
            object = next;
        }
    }

    pub fn interpret(self: *Self, source: []const u8) VmErr!void {
        const function = compile(self, source) catch |e| {
            print("{}\n", .{e});
            return;
        };

        const closure = try ObjClosure.create(self, function);
        self.stack.push(Value.obj(closure.as_obj()));

        try self.call(closure, 0);

        self.gc.active = true;

        try self.run();
    }

    pub fn run(self: *Self) VmErr!void {
        while (true) {
            if (config.PRINT_STACK) {
                print("          ", .{});

                var value = self.stack.values[0..].ptr;
                while (value != self.stack.top) : (value += 1) {
                    print("[", .{});
                    value[0].log();
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
                const dis = Dis.init(&frame.closure.function.chunk);
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
                .CloseUpValue => {
                    self.close_upvalues(self.stack.top - 1);
                    // Discard the stack slot, old local stack variable lives on the heap now
                    _ = self.stack.pop();
                },
                .Closure => {
                    const function = frame.read_constant().as_obj().?.as(ObjFunction);
                    self.stack.push(Value.obj(function.as_obj()));
                    const closure = try ObjClosure.create(self, function);
                    _ = self.stack.pop();
                    self.stack.push(Value.obj(closure.as_obj()));

                    for (0..closure.upvalue_count) |i| {
                        const is_local = frame.read_byte();
                        const index = frame.read_byte();

                        // When compiled, we walk the entire compiler linked list until we find
                        // then one where the upvalue is a local var. If it is local, it's on
                        // the stack, so we just give a ptr to it. Other with, the index is the
                        // index in the enclosing compiler's upvalue array
                        if (is_local == 1) {
                            closure.upvalues[i] = try self.capture_upvalue(&(frame.slots + index)[0]);
                        } else {
                            closure.upvalues[i] = frame.closure.upvalues[index];
                        }
                    }
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
                    // We peek() for GC
                    _ = try self.globals.set(name, self.stack.peek(0));
                    _ = self.stack.pop();
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

                    const iter = frame.slots[iter_index].as_obj().?.as(ObjIter);

                    if (iter.next()) |i| {
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
                        const written = try std.fmt.bufPrint(&buf, "undeclared variable '{s}'\n", .{name.chars});
                        self.runtime_err(written);
                        return error.RuntimeErr;
                    };

                    self.stack.push(value);
                },
                .GetLocal => {
                    const index = frame.read_byte();
                    self.stack.push(frame.slots[index]);
                },
                .GetProperty => {
                    const instance = self.stack.peek(0).as_obj().?.as(ObjInstance);
                    const name = frame.read_string();

                    if (self.stack.peek(0).as_obj()) |obj| {
                        if (obj.kind != .Instance) {
                            self.runtime_err("only instances have properties");
                            return error.RuntimeErr;
                        }

                        if (instance.fields.get(name)) |value| {
                            _ = self.stack.pop(); // Instance
                            self.stack.push(value);
                        } else if (instance.parent.methods.get(name)) |method| {
                            try self.bind_method(method.as_obj().?.as(ObjClosure));
                        } else {
                            var buf: [128]u8 = undefined;
                            const written = try std.fmt.bufPrint(&buf, "undeclared property '{s}'\n", .{name.chars});
                            self.runtime_err(written);
                            return error.RuntimeErr;
                        }
                    } else {
                        self.runtime_err("only instances have properties");
                        return error.RuntimeErr;
                    }
                },
                .GetUpvalue => {
                    const slot = frame.read_byte();
                    print("Get upvalue slot: {}\n", .{slot});
                    // NOTE: do we really need nullable?
                    self.stack.push(frame.closure.upvalues[slot].?.location.*);
                },
                .Greater => self.stack.push(try self.binop('>')),
                .Invoke => {
                    const method_name = frame.read_string();
                    const arg_count = frame.read_byte();

                    try self.invoke(method_name, arg_count);

                    // If invoke succeeded, there is a new CallFrame on the stack.
                    // We update the cached pointer to top frame
                    frame = &self.frame_stack.frames[self.frame_stack.count - 1];
                },
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
                .Method => try self.define_method(frame.read_string()),
                .Multiply => self.stack.push(try self.binop('*')),
                .Negate => {
                    // PERF: https://craftinginterpreters.com/a-virtual-machine.html#challenges 4
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
                    try value.print(self.stdout);
                    try self.stdout.print("\n", .{});
                },
                .Pop => _ = self.stack.pop(),
                .Return => {
                    const result = self.stack.pop();
                    // Close every remaining open upvalues owned by the returning function.
                    self.close_upvalues(frame.slots);
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
                        const written = try std.fmt.bufPrint(&buf, "undeclared variable '{s}'\n", .{name.chars});
                        self.runtime_err(written);
                        return error.RuntimeErr;
                    }
                },
                .SetLocal => {
                    const index = frame.read_byte();
                    frame.slots[index] = self.stack.peek(0);
                },
                .SetProperty => {
                    if (self.stack.peek(1).as_obj()) |obj| {
                        if (obj.kind != .Instance) {
                            self.runtime_err("only instances have fields");
                            return error.RuntimeErr;
                        }

                        const instance = obj.as(ObjInstance);
                        // We can add new fields like that, we don't check if it exists
                        _ = try instance.fields.set(frame.read_string(), self.stack.peek(0));

                        // Here, we let the assigned value on top of the stack to chain
                        // assignemnts
                        const value = self.stack.pop();
                        _ = self.stack.pop();
                        self.stack.push(value);
                    } else {
                        self.runtime_err("only instances have fields");
                        return error.RuntimeErr;
                    }
                },
                .SetUpvalue => {
                    const index = frame.read_byte();
                    frame.closure.upvalues[index].?.location.* = self.stack.peek(0);
                },
                .Struct => {
                    const structure = try ObjStruct.create(self, frame.read_string());
                    self.stack.push(Value.obj(structure.as_obj()));
                },
                .Subtract,
                => self.stack.push(try self.binop('-')),
                .True => self.stack.push(Value.bool_(true)),
            }
        }
    }

    fn call_value(self: *Self, callee: Value, arg_count: u8) VmErr!void {
        // NOTE: type check forced because dynamically typed
        if (callee.as_obj()) |obj| {
            switch (obj.kind) {
                .BoundMethod => {
                    const bound = obj.as(ObjBoundMethod);
                    // We replace the closure Object at the beginning of the frame window
                    // by the receiver, matching the 'self' index 0 in the locals of each
                    // function
                    const val = &(self.stack.top - arg_count - 1)[0];
                    val.* = bound.receiver;
                    return self.call(bound.method, arg_count);
                },
                .Closure => return self.call(callee.as_obj().?.as(ObjClosure), arg_count),
                .NativeFn => {
                    const function = obj.as(ObjNativeFn).function;
                    const args = (self.stack.top - arg_count)[0..arg_count];
                    const result = function(args);
                    self.stack.top -= arg_count + 1;
                    self.stack.push(result);
                    return;
                },
                .Struct => {
                    const structure = obj.as(ObjStruct);
                    const instance = try ObjInstance.create(self, structure);

                    // Replaces the structure by the instance before args for constructor
                    const val = &(self.stack.top - arg_count - 1)[0];
                    val.* = Value.obj(instance.as_obj());

                    // Look for constructor. Tranfers all arguments to the init function
                    if (structure.methods.get(self.init_string)) |constructor| {
                        return self.call(constructor.as_obj().?.as(ObjClosure), arg_count);
                    } else if (arg_count != 0) {
                        // If no init function but args were given
                        var buf: [250]u8 = undefined;
                        const written = try std.fmt.bufPrint(&buf, "expected 0 argument but got {}\n", .{arg_count});
                        self.runtime_err(written);
                        return error.RuntimeErr;
                    }

                    return;
                },
                else => {},
            }
        }

        self.runtime_err("can only call functions and structures");
        return VmErr.RuntimeErr;
    }

    fn call(self: *Self, closure: *ObjClosure, arg_count: u8) VmErr!void {
        // NOTE: check necessary because one pass compiler
        if (arg_count != closure.function.arity) {
            var buf: [250]u8 = undefined;
            const written = try std.fmt.bufPrint(&buf, "expected {} arguments but got {}\n", .{ closure.function.arity, arg_count });
            self.runtime_err(written);
            return error.RuntimeErr;
        }

        if (self.frame_stack.count == FrameStack.FRAMES_MAX) {
            self.runtime_err("stack overflow");
            return VmErr.RuntimeErr;
        }

        const frame = &self.frame_stack.frames[self.frame_stack.count];
        frame.closure = closure;
        frame.ip = closure.function.chunk.code.items.ptr;
        // Pointer arithmetic, -1 to go before function itself
        frame.slots = self.stack.top - arg_count - 1;
        self.frame_stack.count += 1;
    }

    fn invoke(self: *Self, method_name: *ObjString, arg_count: u8) VmErr!void {
        if (self.stack.peek(arg_count).as_obj()) |obj| {
            const instance = obj.as(ObjInstance);

            // It can be a field containing a function, as in GetProperty we check
            if (instance.fields.get(method_name)) |field| {
                const val = &(self.stack.top - arg_count - 1)[0];
                val.* = field;
                return self.call_value(field, arg_count);
            } else if (instance.parent.methods.get(method_name)) |method| {
                return self.call(method.as_obj().?.as(ObjClosure), arg_count);
            } else {
                var buf: [250]u8 = undefined;
                const written = try std.fmt.bufPrint(&buf, "undefined property '{s}'", .{method_name.chars});
                self.runtime_err(written);
                return error.RuntimeErr;
            }
        } else {
            self.runtime_err("only instances have methods");
            return error.RuntimeErr;
        }
    }

    // Open upvalues are an intrusive linked list, the first element is always the
    // last created. Usefull because the close up values are near the closure itself,
    // so near the values on top of stack
    fn capture_upvalue(self: *Self, local: *Value) Allocator.Error!*ObjUpValue {
        var prev_upval: ?*ObjUpValue = null;
        var upval = self.open_upvalues;

        // We look if the value has already been closed over. We check until
        // we point to a location lower than local we want to close over. It
        // means that we passed its point on the stack so it's impossible that is
        // has already been closed over. This is because the list is sorted
        while (upval) |uv| {
            if (@intFromPtr(uv.location) > @intFromPtr(local)) {
                prev_upval = upval;
                upval = uv.next;
            } else {
                break;
            }
        }

        // We found an UpValue already closed over the variable, we reuse it
        if (upval) |uv| {
            if (uv.location == local) return uv;
        }

        const created_upvalue = try ObjUpValue.create(self, local);
        created_upvalue.next = upval;

        // Insertion in-between
        if (prev_upval) |prev_uv| {
            prev_uv.next = created_upvalue;
        } else {
            self.open_upvalues = created_upvalue;
        }

        return created_upvalue;
    }

    fn close_upvalues(self: *Self, last: [*]Value) void {
        while (self.open_upvalues) |open_upval| {
            if (@intFromPtr(open_upval) >= @intFromPtr(last)) {
                open_upval.closed = open_upval.location.*;
                open_upval.location = &open_upval.closed;
                self.open_upvalues = open_upval.next;
            } else {
                break;
            }
        }
    }

    fn define_method(self: *Self, name: *ObjString) Allocator.Error!void {
        const method = self.stack.peek(0);
        const structure = self.stack.peek(1).as_obj().?.as(ObjStruct);
        _ = try structure.methods.set(name, method);
        // Pops the closure for the method
        _ = self.stack.pop();
    }

    /// Called when there is a field access that is method. Methods are first class.
    /// Wraps 'self' with the method
    fn bind_method(self: *Self, method: *ObjClosure) Allocator.Error!void {
        // Top of stack is instance (lhs of dot syntaxe)
        const bound = try ObjBoundMethod.create(self, self.stack.peek(0), method);
        // Pops instance
        _ = self.stack.pop();
        self.stack.push(Value.obj(bound.as_obj()));
    }

    fn binop(self: *Self, op: u8) Allocator.Error!Value {
        // We peek first because if we pop and then the concatenation triggers
        // a GC, we could loose the two strings poped from the stack as they aren't
        // roots anymore
        const v2 = self.stack.peek(0);
        const v1 = self.stack.peek(1);

        if (v1 == .Obj and v2 == .Obj) {
            return self.concatenate(v1.as_obj().?.as(ObjString), v2.as_obj().?.as(ObjString));
        }

        _ = self.stack.pop();
        _ = self.stack.pop();

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

    fn concatenate(self: *Self, s1: *ObjString, s2: *ObjString) Allocator.Error!Value {
        const res = try self.allocator.alloc(u8, s1.chars.len + s2.chars.len);
        @memcpy(res[0..s1.chars.len], s1.chars);
        @memcpy(res[s1.chars.len..], s2.chars);

        // Pop after alloc in case of GC trigger
        _ = self.stack.pop();
        _ = self.stack.pop();

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
        var line = frame.closure.function.chunk.lines.items[self.instruction_nb()];

        print("[line {}] Error in script: {s}\n", .{ line, msg });

        for (0..self.frame_stack.count - 1) |i| {
            frame = &self.frame_stack.frames[self.frame_stack.count - i];
            const function = frame.closure.function;
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
        const addr2 = @intFromPtr(frame.closure.function.chunk.code.items.ptr);
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
