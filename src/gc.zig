const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const config = @import("config");
const Vm = @import("vm.zig").Vm;
const Value = @import("values.zig").Value;
const Obj = @import("obj.zig").Obj;
const ObjClosure = @import("obj.zig").ObjClosure;
const ObjFunction = @import("obj.zig").ObjFunction;
const ObjUpValue = @import("obj.zig").ObjUpValue;
const Table = @import("table.zig").Table;
const Compiler = @import("compiler.zig").Compiler;

pub const Gc = struct {
    vm: *Vm,
    parent_allocator: Allocator,
    grays: std.ArrayList(*Obj),
    bytes_allocated: usize,
    next_gc: usize,
    active: bool,

    const Self = @This();
    const GROW_FACTOR = 2;

    pub fn init(parent_allocator: Allocator) Self {
        return .{
            .vm = undefined,
            // .compiler = undefined,
            .parent_allocator = parent_allocator,
            .grays = std.ArrayList(*Obj).init(parent_allocator),
            .bytes_allocated = 0,
            .next_gc = 1024 * 1024,
            .active = false,
        };
    }

    pub fn deinit(self: *Self) void {
        self.grays.deinit();
    }

    pub fn link(self: *Self, vm: *Vm) void {
        self.vm = vm;
    }

    pub fn collect_garbage(self: *Self) Allocator.Error!void {
        if (config.LOG_GC) {
            print("\n-- GC begin\n", .{});
        }

        const bytes_before = self.bytes_allocated;

        try self.mark_roots();
        try self.trace_reference();
        Gc.table_remove_white(&self.vm.strings);
        self.sweep();

        self.next_gc = self.bytes_allocated * Gc.GROW_FACTOR;

        if (config.LOG_GC) {
            print("-- GC end\n", .{});
            print("   collected {} bytes (from {} to {}), next at {}\n\n", .{ bytes_before - self.bytes_allocated, bytes_before, self.bytes_allocated, self.next_gc });
        }
    }

    fn mark_roots(self: *Self) Allocator.Error!void {
        for (&self.vm.stack.values) |*value| {
            try self.mark_value(value);
        }

        try self.mark_table(&self.vm.globals);

        for (self.vm.frame_stack.frames[0..self.vm.frame_stack.count]) |*frame| {
            try self.mark_object(frame.closure.as_obj());
        }

        var current_upval = self.vm.open_upvalues;
        while (current_upval) |open_upval| : (current_upval = open_upval.next) {
            try self.mark_object(open_upval.as_obj());
        }
    }

    fn trace_reference(self: *Self) Allocator.Error!void {
        while (self.grays.items.len > 0) {
            const obj = self.grays.pop();
            try self.blacken_object(obj);
        }
    }

    fn blacken_object(self: *Self, obj: *Obj) Allocator.Error!void {
        if (config.LOG_GC) {
            print("{*} blacken ", .{obj});
            obj.print(std.debug);
            print("\n", .{});
        }

        switch (obj.kind) {
            .Closure => {
                const closure = obj.as(ObjClosure);
                try self.mark_object(closure.function.as_obj());

                for (closure.upvalues) |upval| {
                    // NOTE: can't we just .?
                    if (upval) |up| {
                        try self.mark_object(up.as_obj());
                    }
                }
            },
            .Fn => {
                const function = obj.as(ObjFunction);
                if (function.name) |name| {
                    try self.mark_object(name.as_obj());
                }
                try self.mark_array(&function.chunk.constants);
            },
            .UpValue => try self.mark_value(&obj.as(ObjUpValue).closed),
            .NativeFn, .String, .Iter => {},
        }
    }

    /// We don't consider interned strings as root, otherwise no strings could
    /// ever be freed. We count them as weak reference. Only if after the
    /// marker phase they are white, we free them from the table.
    fn table_remove_white(table: *Table) void {
        for (table.entries) |*entry| {
            if (entry.key) |k| {
                if (k.as_obj().is_marked) _ = table.delete(k);
            }
        }
    }

    fn sweep(self: *Self) void {
        var previous: ?*Obj = null;
        var object: ?*Obj = self.vm.objects;

        while (object) |obj| {
            if (obj.is_marked) {
                obj.is_marked = false;
                previous = object;
                object = obj.next;
            } else {
                const unreached = obj;
                object = obj.next;

                if (previous) |prev| {
                    prev.next = object;
                } else {
                    self.vm.objects = object;
                }

                if (config.LOG_GC) {
                    print("{*} sweep\n", .{unreached});
                }

                unreached.destroy(self.vm);
            }
        }
    }

    fn mark_value(self: *Self, value: *Value) Allocator.Error!void {
        if (value.as_obj()) |obj| try self.mark_object(obj);
    }

    // pub because called once by the compiler
    pub fn mark_object(self: *Self, obj: ?*Obj) Allocator.Error!void {
        if (obj) |o| {
            if (o.is_marked) return;

            if (config.LOG_GC) {
                print("{*} mark ", .{o});
                o.print(std.debug);
                print("\n", .{});
            }

            o.is_marked = true;

            try self.grays.append(o);
        }
    }

    fn mark_table(self: *Self, table: *Table) Allocator.Error!void {
        for (table.entries) |*entry| {
            if (entry.key) |key| {
                try self.mark_object(key.as_obj());
                try self.mark_value(&entry.value);
            }
        }
    }

    fn mark_array(self: *Self, array: *std.ArrayList(Value)) Allocator.Error!void {
        for (array.items) |*value| try self.mark_value(value);
    }

    /// Calling alloc triggers the GC before allocating
    pub fn alloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
        const self: *Self = @ptrCast(@alignCast(ctx));

        self.bytes_allocated += len;
        if (self.active and (self.bytes_allocated > self.next_gc or config.STRESS_GC)) {
            self.collect_garbage() catch return null;
        }

        return self.parent_allocator.rawAlloc(len, ptr_align, ret_addr);
    }

    pub fn free(ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
        const self: *Self = @ptrCast(@alignCast(ctx));

        self.bytes_allocated -= buf.len;

        self.parent_allocator.rawFree(buf, buf_align, ret_addr);
    }

    pub fn resize(ctx: *anyopaque, buf: []u8, buf_align: u8, new_len: usize, ret_addr: usize) bool {
        const self: *Self = @ptrCast(@alignCast(ctx));

        self.bytes_allocated += new_len - buf.len;

        if (self.active and (self.bytes_allocated > self.next_gc or config.STRESS_GC)) {
            self.collect_garbage() catch return false;
        }

        return self.parent_allocator.rawResize(buf, buf_align, new_len, ret_addr);
    }

    /// Returns the Gc as an allocator
    pub fn allocator(self: *Self) Allocator {
        return .{ .ptr = self, .vtable = &.{
            .alloc = alloc,
            .resize = resize,
            .free = free,
        } };
    }
};
