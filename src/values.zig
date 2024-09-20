pub const Value = union(enum) {
    Int: i64,
    Float: f64,
    Bool: bool,
    Null: void,

    pub fn print(self: Value, writer: anytype) void {
        switch (self) {
            .Int => |v| writer.print("{}", .{v}),
            .Float => |v| writer.print("{d}", .{v}),
            .Bool => |v| writer.print("{}", .{v}),
            .Null => writer.print("null", .{}),
        }
    }
};
