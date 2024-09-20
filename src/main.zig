const std = @import("std");
const print = std.debug.print;
const Lexer = @import("lexer.zig").Lexer;
const Chunk = @import("chunk.zig").Chunk;
const Disassembler = @import("disassembler.zig").Disassembler;
const Vm = @import("vm.zig").Vm;

const Opts = struct {
    debug_trace: bool = false,
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    // TMP
    const constant_idx = try chunk.write_constant(.{ .Float = 1.2 });
    try chunk.write_op(.Constant, 123);
    try chunk.write_byte(@truncate(constant_idx), 123);
    try chunk.write_op(.Return, 123);

    var disassembler = Disassembler.init(&chunk);
    try disassembler.dis_chunk("test chunk");

    var vm = Vm.init(&chunk);
    defer vm.deinit();

    try vm.interpret();

    try repl(allocator);
}

fn repl(allocator: std.mem.Allocator) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var input = try std.ArrayList(u8).initCapacity(allocator, 1024);
    defer input.deinit();

    var lexer = Lexer.init(allocator);
    defer lexer.deinit();

    var buffer: [500]u8 = undefined;

    _ = try stdout.write("\t\tRizon language REPL\n");

    while (true) {
        _ = try stdout.write("\n> ");

        input.clearRetainingCapacity();
        lexer.reinit();

        try stdin.streamUntilDelimiter(input.writer(), '\n', null);

        const trimmed = std.mem.trimRight(u8, input.items, "\r");

        const tokens = lexer.lex(trimmed) catch |err| switch (err) {
            error.UnterminatedString => {
                const tmp = try std.fmt.bufPrint(&buffer, "Error: unterminated string at {}\n", .{lexer.current});
                _ = try stdout.write(tmp);
                continue;
            },
            else => continue,
        };

        for (tokens) |tk| {
            print("Kind: {s}, ", .{@tagName(tk.kind)});

            if (tk.value) |val| {
                print("value: {s}\n", .{val});
            } else {
                print("\n", .{});
            }
        }
    }
}
