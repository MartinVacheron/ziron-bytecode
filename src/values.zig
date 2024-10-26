const std = @import("std");
const Obj = @import("obj.zig").Obj;
const ObjString = @import("obj.zig").ObjString;
const ObjKind = Obj.ObjKind;

pub const Value = union(enum) {
    Bool: bool,
    Float: f64,
    Int: i64,
    Null: void,
    Obj: *Obj,

    pub fn bool_(value: bool) Value {
        return .{ .Bool = value };
    }

    pub fn float(value: f64) Value {
        return .{ .Float = value };
    }

    pub fn int(value: i64) Value {
        return .{ .Int = value };
    }

    pub fn null_() Value {
        return .{ .Null = undefined };
    }

    pub fn obj(object: *Obj) Value {
        return .{ .Obj = object };
    }

    pub fn as_int(self: *const Value) ?i64 {
        return switch (self.*) {
            .Int => |v| v,
            else => null,
        };
    }

    pub fn as_float(self: *const Value) ?f64 {
        return switch (self.*) {
            .Float => |v| v,
            else => null,
        };
    }

    pub fn as_bool(self: *const Value) ?bool {
        return switch (self.*) {
            .Bool => |v| v,
            else => null,
        };
    }

    // PERF: else => unreachable and return a non-null to avoid unwrapping
    // it in the call site?
    pub fn as_obj(self: *const Value) ?*Obj {
        return switch (self.*) {
            .Obj => |v| v,
            else => null,
        };
    }

    pub fn equals(self: Value, other: Value) bool {
        if (std.meta.activeTag(self) != std.meta.activeTag(other)) return false;

        return switch (self) {
            .Int => self.Int == other.Int,
            .Float => self.Float == other.Float,
            .Bool => self.Bool == other.Bool,
            .Null => true,
            .Obj => {
                const str1 = self.as_obj().?.as(ObjString);
                const str2 = other.as_obj().?.as(ObjString);
                // Pointer comparison since we intern strings
                return str1 == str2;
            },
        };
    }

    pub fn print(self: *const Value, writer: anytype) std.fs.File.WriteError!void {
        try switch (self.*) {
            .Int => |v| writer.print("{}", .{v}),
            .Float => |v| writer.print("{d}", .{v}),
            .Bool => |v| writer.print("{}", .{v}),
            .Null => writer.print("null", .{}),
            .Obj => |v| v.print(writer),
        };
    }

    pub fn log(self: *const Value) void {
        switch (self.*) {
            .Int => |v| std.debug.print("{}", .{v}),
            .Float => |v| std.debug.print("{d}", .{v}),
            .Bool => |v| std.debug.print("{}", .{v}),
            .Null => std.debug.print("null", .{}),
            .Obj => |v| v.log(),
        }
    }
};
