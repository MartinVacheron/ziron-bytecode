const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const config = @import("config");
const Vm = @import("vm.zig").Vm;
const Table = @import("table.zig").Table;
const Chunk = @import("chunk.zig").Chunk;
const Value = @import("values.zig").Value;

pub const Obj = struct {
    kind: ObjKind,
    next: ?*Obj,
    is_marked: bool,

    const ObjKind = enum {
        Closure,
        Fn,
        Instance,
        Iter,
        NativeFn,
        String,
        Struct,
        UpValue,
    };

    pub fn allocate(vm: *Vm, comptime T: type, kind: ObjKind) Allocator.Error!*T {
        comptime assert(@hasField(T, "obj"));
        comptime assert(@hasDecl(T, "as_obj"));

        const ptr = try vm.allocator.create(T);
        ptr.obj = Obj{
            .kind = kind,
            .next = vm.objects,
            .is_marked = false,
        };

        vm.objects = &ptr.obj;

        if (config.LOG_GC) {
            std.debug.print("{*} allocate {} bytes  ", .{ ptr, @sizeOf(T) });
        }

        return ptr;
    }

    // NOTE: lots of repetition...
    pub fn destroy(self: *Obj, vm: *Vm) void {
        switch (self.kind) {
            .Closure => self.as(ObjClosure).deinit(vm.allocator),
            .Fn => {
                const function = self.as(ObjFunction);
                function.deinit(vm.allocator);
            },
            .Instance => {
                const instance = self.as(ObjInstance);
                instance.deinit(vm.allocator);
            },
            .Iter => {
                const iter = self.as(ObjIter);
                vm.allocator.destroy(iter);
            },
            .NativeFn => {
                const function = self.as(ObjNativeFn);
                function.deinit(vm.allocator);
            },
            .String => {
                const string = self.as(ObjString);
                string.deinit(vm.allocator);
            },
            .Struct => {
                const structure = self.as(ObjStruct);
                structure.deinit(vm.allocator);
            },
            .UpValue => {
                const upval = self.as(ObjUpValue);
                upval.deinit(vm.allocator);
            },
        }
    }

    pub fn as(self: *Obj, comptime T: type) *T {
        comptime assert(@hasField(T, "obj"));

        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub fn print(self: *Obj, writer: anytype) void {
        switch (self.kind) {
            .Closure => self.as(ObjClosure).function.print(writer),
            .Fn => self.as(ObjFunction).print(writer),
            .Instance => writer.print("<instance of {s}>", .{self.as(ObjInstance).parent.name.chars}),
            .Iter => {
                const iter = self.as(ObjIter);
                writer.print("iter: {} -> {}", .{ iter.current, iter.end });
            },
            .NativeFn => writer.print("<native fn>", .{}),
            .String => writer.print("\"{s}\"", .{self.as(ObjString).chars}),
            .Struct => writer.print("<struct {s}>", .{self.as(ObjStruct).name.chars}),
            .UpValue => writer.print("upvalue", .{}),
        }
    }
};

pub const ObjString = struct {
    obj: Obj,
    chars: []const u8,
    hash: u32,

    const Self = @This();

    // PERF: flexible array member: https://craftinginterpreters.com/strings.html#challenges
    fn create(vm: *Vm, str: []const u8, hash: u32) Allocator.Error!*ObjString {
        var obj = try Obj.allocate(vm, Self, .String);
        obj.chars = str;
        obj.hash = hash;

        // The set method can trigger a GC to grow hashmap before
        // inserting. We put the value on the stack so that it is marked
        // as a root
        vm.stack.push(Value.obj(obj.as_obj()));
        _ = try vm.strings.set(obj, Value.null_());
        _ = vm.stack.pop();

        if (config.LOG_GC) {
            std.debug.print("{s}\n", .{str});
        }

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

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.free(self.chars);
        allocator.destroy(self);
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

        if (config.LOG_GC) {
            std.debug.print("0 -> {}\n", .{end});
        }

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

pub const ObjFunction = struct {
    obj: Obj,
    arity: u8,
    chunk: Chunk,
    name: ?*ObjString,
    upvalue_count: u8,

    const Self = @This();

    pub fn create(vm: *Vm, name: ?*ObjString) Allocator.Error!*Self {
        const obj = try Obj.allocate(vm, Self, .Fn);

        obj.arity = 0;
        obj.chunk = Chunk.init(vm.allocator);
        obj.name = name;
        obj.upvalue_count = 0;

        if (config.LOG_GC) {
            const display_name = if (name) |n| n.chars else "";
            std.debug.print("{s}\n", .{display_name});
        }

        return obj;
    }

    pub fn as_obj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn print(self: *const Self, writer: anytype) void {
        if (self.name) |n| {
            writer.print("<fn {s}>", .{n.chars});
        } else {
            writer.print("<script>", .{});
        }
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.chunk.deinit();

        // Name already in the linked list, don't free manually
        allocator.destroy(self);
    }
};

// Many item pointer for args?
pub const NativeFn = *const fn ([]const Value) Value;

pub const ObjNativeFn = struct {
    obj: Obj,
    function: NativeFn,

    const Self = @This();

    pub fn create(vm: *Vm, function: NativeFn) Allocator.Error!*Self {
        const obj = try Obj.allocate(vm, Self, .NativeFn);
        obj.function = function;

        if (config.LOG_GC) std.debug.print("\n", .{});

        return obj;
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.destroy(self);
    }

    pub fn as_obj(self: *ObjNativeFn) *Obj {
        return &self.obj;
    }
};

pub const ObjClosure = struct {
    obj: Obj,
    function: *ObjFunction,
    upvalues: []?*ObjUpValue, // NOTE: do we really need nullable?
    upvalue_count: u8,

    const Self = @This();

    pub fn create(vm: *Vm, function: *ObjFunction) Allocator.Error!*Self {
        // GC can't be triggered by direct call to alloc function (see body)
        const upvalues = try vm.allocator.alloc(?*ObjUpValue, function.upvalue_count);

        // Need to null this out rather than leaving it
        // uninitialized becaue the GC might try to look at it
        // before it gets filled in with values
        for (upvalues) |*uv| {
            uv.* = null;
        }

        const obj = try Obj.allocate(vm, Self, .Closure);

        obj.function = function;
        obj.upvalues = upvalues;
        obj.upvalue_count = function.upvalue_count;

        if (config.LOG_GC) {
            const display_name = if (function.name) |n| n.chars else "main";
            std.debug.print("{s}\n", .{display_name});
        }

        return obj;
    }

    pub fn as_obj(self: *Self) *Obj {
        return &self.obj;
    }

    // We don't destroy the function, closure don't own them. Multiple
    // closure can refer to the same function
    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.free(self.upvalues);
        allocator.destroy(self);
    }
};

pub const ObjUpValue = struct {
    obj: Obj,
    location: *Value,
    next: ?*ObjUpValue,
    closed: Value,

    const Self = @This();

    pub fn create(vm: *Vm, slot: *Value) Allocator.Error!*Self {
        const obj = try Obj.allocate(vm, Self, .UpValue);
        obj.location = slot;
        obj.next = null;
        obj.closed = Value.null_();

        if (config.LOG_GC) {
            slot.print(std.debug);
            std.debug.print("\n", .{});
        }

        return obj;
    }

    pub fn as_obj(self: *Self) *Obj {
        return &self.obj;
    }

    // It dosen't own the variable
    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.destroy(self);
    }
};

pub const ObjStruct = struct {
    obj: Obj,
    name: *ObjString,

    const Self = @This();

    pub fn create(vm: *Vm, name: *ObjString) Allocator.Error!*Self {
        const obj = try Obj.allocate(vm, Self, .Struct);
        obj.name = name;

        if (config.LOG_GC) std.debug.print("<struct {s}>\n", .{name.chars});

        return obj;
    }

    pub fn as_obj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.destroy(self);
    }
};

pub const ObjInstance = struct {
    obj: Obj,
    parent: *ObjStruct,
    fields: Table,

    const Self = @This();

    pub fn create(vm: *Vm, parent: *ObjStruct) Allocator.Error!*Self {
        const obj = try Obj.allocate(vm, Self, .Instance);
        obj.parent = parent;
        obj.fields = Table.init(vm.allocator);

        if (config.LOG_GC) std.debug.print("<instance of {s}>\n", .{parent.name.chars});

        return obj;
    }

    pub fn as_obj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.fields.deinit();
        allocator.destroy(self);
    }
};
