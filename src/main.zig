const std = @import("std");
const print = std.debug.print;
const Lexer = @import("lexer.zig").Lexer;
const Chunk = @import("chunk.zig").Chunk;
const Disassembler = @import("disassembler.zig").Disassembler;
const Vm = @import("vm.zig").Vm;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var args = try std.process.ArgIterator.initWithAllocator(allocator);
    defer args.deinit();

    _ = args.next(); // file name

    if (args.next()) |filename| {
        try run_file(filename);
    } else {
        try repl(allocator);
    }
}

fn run_file(filename: []const u8) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const status = gpa.deinit();
        std.debug.assert(status == .ok);
    }

    const allocator = gpa.allocator();

    const file = std.fs.cwd().openFile(filename, .{ .mode = .read_only }) catch |err| {
        var buf: [500]u8 = undefined;
        _ = try std.fmt.bufPrint(&buf, "Error: {}, unable to open file at: {s}\n", .{ err, filename });
        print("{s}", .{buf});
        return err;
    };
    defer file.close();

    const size = try file.getEndPos();

    const buf = try allocator.alloc(u8, size);
    defer allocator.free(buf);

    _ = try file.readAll(buf);

    var vm = Vm.new(allocator);
    try vm.init();
    defer vm.deinit();

    try vm.interpret(buf);
}

fn repl(allocator: std.mem.Allocator) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var input = std.ArrayList(u8).init(allocator);
    defer input.deinit();

    var vm = Vm.new(allocator);
    try vm.init();
    defer vm.deinit();

    _ = try stdout.write("\t\tZiron language REPL\n");

    while (true) {
        _ = try stdout.write("\n> ");

        input.clearRetainingCapacity();
        try stdin.streamUntilDelimiter(input.writer(), '\n', null);
        // const trimmed = std.mem.trimRight(u8, input.items, "\r");

        try input.append('\n');

        // try vm.interpret(trimmed);
        try vm.interpret(input.items);
    }
}
