const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Vm = @import("vm.zig").Vm;
const Value = @import("values.zig").Value;

pub const Obj = struct {
    kind: ObjKind,
    next: ?*Obj,

    const ObjKind = enum {
        Iter,
        String,
    };

    pub fn allocate(vm: *Vm, comptime T: type, kind: ObjKind) Allocator.Error!*T {
        comptime assert(@hasField(T, "obj"));

        const ptr = try vm.allocator.create(T);
        ptr.obj = Obj{
            .kind = kind,
            .next = vm.objects,
        };

        vm.objects = &ptr.obj;
        return ptr;
    }

    pub fn as(self: *Obj, comptime T: type) *T {
        comptime assert(@hasField(T, "obj"));

        // Obj is aligned on 1 byte, *T on 8 (i beleve this is why its mandatory)
        // NOTE: Why is it aligned 1 byte?
        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub fn print(self: *Obj, writer: anytype) void {
        switch (self.kind) {
            .Iter => {
                const iter = self.as(ObjIter);
                writer.print("iter: {} -> {}", .{ iter.current, iter.end });
            },
            .String => {
                const str = self.as(ObjString);
                writer.print("{s}", .{str.chars});
            },
        }
    }
};

pub const ObjString = struct {
    obj: Obj,
    chars: []const u8,
    hash: u32,

    // PERF: flexible array member: https://craftinginterpreters.com/strings.html#challenges
    pub fn create(vm: *Vm, str: []const u8, hash: u32) Allocator.Error!*ObjString {
        var obj = try Obj.allocate(vm, ObjString, .String);
        obj.chars = str;
        obj.hash = hash;

        _ = try vm.strings.set(obj, Value.null_());

        return obj;
    }

    pub fn copy(vm: *Vm, str: []const u8) Allocator.Error!*ObjString {
        const hash = ObjString.hash_string(str);
        const interned = vm.strings.find_string(str, hash);

        if (interned) |i| return i;

        const chars = try vm.allocator.alloc(u8, str.len);
        @memcpy(chars, str);

        return ObjString.create(vm, chars, hash);
    }

    // Take a string allocated by calling Vm. If interned already, free
    // the memory and return the interned one
    pub fn take(vm: *Vm, str: []const u8) Allocator.Error!*ObjString {
        const hash = ObjString.hash_string(str);
        const interned = vm.strings.find_string(str, hash);

        if (interned) |i| {
            vm.allocator.free(str);
            return i;
        }

        return ObjString.create(vm, str, hash);
    }

    pub fn as_obj(self: *ObjString) *Obj {
        return &self.obj;
    }

    fn hash_string(chars: []const u8) u32 {
        var hash: u32 = 2166136261;
        for (chars) |c| {
            hash ^= c;
            hash *%= 16777619;
        }

        return hash;
    }
};

pub const ObjIter = struct {
    obj: Obj,
    end: i64,
    current: i64,

    const Self = @This();

    pub fn create(vm: *Vm, end: i64) Allocator.Error!*ObjIter {
        const iter = try Obj.allocate(vm, Self, .Iter);

        iter.end = end;
        iter.current = 0;

        return iter;
    }

    pub fn next(self: *Self) ?i64 {
        if (self.current < self.end) {
            self.current += 1;
            return self.current - 1;
        }

        return null;
    }

    pub fn as_obj(self: *Self) *Obj {
        return &self.obj;
    }
};
