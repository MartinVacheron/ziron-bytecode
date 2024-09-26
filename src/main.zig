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
    _ = args.next(); // file name

    if (args.next()) |filename| {
        run_file(filename, allocator) catch |err| {
            var buf: [500]u8 = undefined;
            _ = try std.fmt.bufPrint(&buf, "Error: {}, unable to open file at: {s}\n", .{ err, filename });
            @panic(&buf);
        };
    } else {
        // try repl(allocator);
    }
}

fn run_file(filename: []const u8, allocator: std.mem.Allocator) !void {
    const file = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
    defer file.close();

    const size = try file.getEndPos();

    const buf = try allocator.alloc(u8, size);
    defer allocator.free(buf);

    _ = try file.readAll(buf);

    var vm = Vm.new(allocator);
    vm.init();
    defer vm.deinit();

    try vm.interpret(buf);
}

fn repl(allocator: std.mem.Allocator) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var input = std.ArrayList(u8).init(allocator);
    defer input.deinit();

    var lexer = Lexer.init(allocator);
    defer lexer.deinit();

    var buffer: [500]u8 = undefined;

    _ = try stdout.write("\t\tZiron language REPL\n");

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
