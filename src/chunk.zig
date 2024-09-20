const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("values.zig").Value;

pub const OpCode = enum(u8) {
    Constant,
    Return,
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    lines: std.ArrayList(u8),
    constants: std.ArrayList(Value),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .code = std.ArrayList(u8).init(allocator),
            .lines = std.ArrayList(u8).init(allocator),
            .constants = std.ArrayList(Value).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit();
        self.lines.deinit();
        self.constants.deinit();
    }

    pub fn write_op(self: *Self, op: OpCode, line: u8) Allocator.Error!void {
        try self.code.append(@intFromEnum(op));
        try self.lines.append(line);
    }

    pub fn write_byte(self: *Self, byte: u8, line: u8) Allocator.Error!void {
        try self.code.append(byte);
        try self.lines.append(line);
    }

    pub fn write_constant(self: *Self, constant: Value) Allocator.Error!usize {
        try self.constants.append(constant);
        return self.constants.items.len - 1;
    }

    pub fn read_constant(self: Self, index: u8) Value {
        return self.constants.items[index];
    }
};
