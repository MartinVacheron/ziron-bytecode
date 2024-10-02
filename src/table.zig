const std = @import("std");
const Allocator = std.mem.Allocator;
const ObjString = @import("obj.zig").ObjString;
const Value = @import("values.zig").Value;

pub const Entry = struct {
    key: ?*ObjString,
    value: Value,
};

pub const Table = struct {
    count: usize,
    entries: []Entry,
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        // Type coercion tuple to arry: https://ziglang.org/documentation/master/#Type-Coercion-Tuples-to-Arrays
        return .{
            .count = 0,
            .entries = &.{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.entries);
    }

    pub fn set(self: *Self, key: *ObjString, value: Value) Allocator.Error!bool {
        // Encodes a 75%
        if (4 * (self.count + 1) > 3 * self.entries.len) {
            try self.adjust_capacity();
        }

        const entry = Table.find_entry(self.entries, key);
        const is_new = entry.key == null;

        entry.key = key;
        entry.value = value;
        self.count += 1;

        return is_new;
    }

    fn find_entry(entries: []Entry, key: *const ObjString) *Entry {
        var index = key.hash % entries.len;

        while (true) : (index = (index + 1) % entries.len) {
            const entry = &entries[index];

            if (entry.key == key or entry.key == null) {
                return entry;
            }
        }
    }

    fn adjust_capacity(self: *Self) Allocator.Error!void {
        const new_capa = self.grow();
        const entries_grown = try self.allocator.alloc(Entry, new_capa);

        // PERF: is it mandatory? At least maybe only the key?
        for (entries_grown) |*e| {
            e.key = null;
            e.value = Value.null_();
        }

        for (self.entries) |*e| {
            if (e.key) |k| {
                const dest = Table.find_entry(entries_grown, k);
                dest.key = k;
                dest.value = e.value;
            }
        }

        self.allocator.free(self.entries);
        self.entries = entries_grown;
    }

    fn grow(self: *Self) usize {
        if (self.entries.len < 8) {
            return 8;
        } else {
            return self.entries.len * 2;
        }
    }
};

test "set" {
    const Vm = @import("vm.zig").Vm;
    const allocator = std.testing.allocator;

    var vm = Vm.new(allocator);
    vm.init();
    defer vm.deinit();

    var table = Table.init(allocator);
    defer table.deinit();

    // Alloc with Vm's allocator to mimic real case
    // otherwise segfault while we free it at Vm's deinit
    const str = try vm.allocator.alloc(u8, 4);
    @memcpy(str, "mars");

    const key = try ObjString.create(&vm, str);
    const val = Value.int(42);

    try std.testing.expect(try table.set(key, val));
    try std.testing.expect(!try table.set(key, val));
}

test "grow" {
    const Vm = @import("vm.zig").Vm;
    const allocator = std.testing.allocator;

    var vm = Vm.new(allocator);
    vm.init();
    defer vm.deinit();

    var table = Table.init(allocator);
    defer table.deinit();

    const val = Value.int(42);

    for (0..9) |i| {
        const str = try vm.allocator.alloc(u8, i);
        @memset(str, 'a');
        const key = try ObjString.create(&vm, str);
        // We check that each new entry is unique
        try std.testing.expect(try table.set(key, val));
    }

    try std.testing.expect(table.count == 9);
    try std.testing.expect(table.entries.len == 16);
}
