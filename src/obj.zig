const std = @import("std");
const Allocator = std.mem.Allocator;
const Vm = @import("vm.zig").Vm;

pub const Obj = struct {
    kind: ObjKind,
    next: ?*Obj,

    const ObjKind = enum {
        String,
    };

    pub fn allocate(vm: *Vm, comptime T: type, kind: ObjKind) Allocator.Error!*T {
        const ptr = try vm.allocator.create(T);
        ptr.obj = Obj{
            .kind = kind,
            .next = vm.objects,
        };

        vm.objects = &ptr.obj;
        return ptr;
    }

    pub fn as(self: *Obj, comptime T: type) *T {
        // Obj is aligned on 1 byte, *T on 8 (i beleve this is why its mandatory)
        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub fn print(self: *Obj, writer: anytype) void {
        switch (self.kind) {
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

    // PERF: lexible array member: https://craftinginterpreters.com/strings.html#challenges
    pub fn create(vm: *Vm, str: []const u8) Allocator.Error!*ObjString {
        var obj = try Obj.allocate(vm, ObjString, .String);
        obj.chars = str;
        obj.hash = ObjString.hash_string(str);
        return obj;
    }

    pub fn copy(vm: *Vm, str: []const u8) Allocator.Error!*ObjString {
        const chars = try vm.allocator.alloc(u8, str.len);
        @memcpy(chars, str);
        try ObjString.create(vm, chars);
    }

    pub fn as_obj(self: *ObjString) *Obj {
        return &self.obj;
    }

    pub fn eq(self: *const ObjString, other: *const ObjString) bool {
        return std.mem.eql(u8, self.chars, other.chars);
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
