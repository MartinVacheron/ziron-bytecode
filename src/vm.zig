const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("values.zig").Value;
const config = @import("config");

const VmErr = error{
    Compile,
    Runtime,
};

pub const Vm = struct {
    chunk: *Chunk,
    ip: [*]u8,
    stack: [STACK_SIZE]Value,
    stack_top: *Value,

    const STACK_SIZE: u8 = 255;
    const Self = @This();

    pub fn init(chunk: *Chunk) Self {
        return .{
            .chunk = chunk,
            .ip = chunk.code.items.ptr,
            .stack = undefined,
            .stack_top = undefined,
        };
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    pub fn interpret(self: *Self) VmErr!void {
        while (true) {
            const instruction = self.read_byte();
            const op_code: OpCode = @enumFromInt(instruction);

            // as it is known as comptime (const via build.zig), not compiled
            // if not needed (equivalent of #ifdef or #[cfg(feature...)])
            if (config.TRACING) {
                std.debug.print("Instruction: {s}\n", .{@tagName(op_code)});
            }

            switch (op_code) {
                .Return => {
                    return;
                },
                .Constant => {
                    const cte = self.chunk.read_constant(self.read_byte());
                    cte.print(std.debug);
                    std.debug.print("\n", .{});
                },
            }
        }
    }

    fn read_byte(self: *Self) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }
};
