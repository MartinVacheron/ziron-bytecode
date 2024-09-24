const std = @import("std");
const print = std.debug.print;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("values.zig").Value;
const Compiler = @import("compiler.zig").Compiler;
const config = @import("config");

const VmErr = error{
    Compile,
    Runtime,
};

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
};

pub const Vm = struct {
    chunk: *const Chunk,
    ip: [*]u8,
    stack: Stack,
    compiler: Compiler,

    const Self = @This();

    pub fn new(chunk: *const Chunk) Self {
        return .{
            .chunk = chunk,
            .ip = chunk.code.items.ptr,
            .stack = Stack.new(),
            .compiler = Compiler.init(),
        };
    }

    pub fn init(self: *Self) void {
        self.stack.init();
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    fn read_byte(self: *Self) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    pub fn interpret(self: *Self, source: []const u8) void {
        self.compiler.compile(source);
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
                const dis = Dis.init(self.chunk);
                const addr1 = @intFromPtr(self.ip);
                const addr2 = @intFromPtr(self.chunk.code.items.ptr);
                _ = dis.dis_instruction(addr1 - addr2);
            }

            const instruction = self.read_byte();
            const op_code: OpCode = @enumFromInt(instruction);

            switch (op_code) {
                .Constant => {
                    const value = self.chunk.read_constant(self.read_byte());
                    self.stack.push(value);
                },
                .Add => self.stack.push(self.binop('+')),
                .Subtract => self.stack.push(self.binop('-')),
                .Multiply => self.stack.push(self.binop('*')),
                .Divide => self.stack.push(self.binop('/')),
                .Negate => {
                    // PERF: https://craftinginterpreters.com/a-virtual-machine.html#challenges [4]
                    const value = self.stack.pop();
                    switch (value) {
                        .Int => |v| self.stack.push(.{ .Int = -v }),
                        .Float => |v| self.stack.push(.{ .Float = -v }),
                        else => @panic("Can't negate anything else than a number"),
                    }
                },
                .Return => {
                    const value = self.stack.pop();
                    value.print(std.debug);
                    print("\n", .{});
                    return;
                },
            }
        }
    }

    fn binop(self: *Self, op: u8) Value {
        const v2 = self.stack.pop();
        const v1 = self.stack.pop();

        if (v1 == .Int and v2 != .Int or v1 == .Float and v2 != .Float) {
            @panic("Binary operation only allowed between ints or floats");
        }

        if (v1 == .Int and v2 == .Int) {
            return switch (op) {
                '+' => .{ .Int = v1.Int + v2.Int },
                '-' => .{ .Int = v1.Int - v2.Int },
                '*' => .{ .Int = v1.Int * v2.Int },
                '/' => .{ .Int = @divTrunc(v1.Int, v2.Int) },
                else => unreachable,
            };
        }

        if (v1 == .Float and v2 == .Float) {
            return switch (op) {
                '+' => .{ .Float = v1.Float + v2.Float },
                '-' => .{ .Float = v1.Float - v2.Float },
                '*' => .{ .Float = v1.Float * v2.Float },
                '/' => .{ .Float = v1.Float / v2.Float },
                else => unreachable,
            };
        }

        unreachable;
    }
};
