const std = @import("std");
const print = std.debug.print;
const Lexer = @import("lexer.zig").Lexer;

pub const Compiler = struct {
    lexer: Lexer,

    const Self = @This();

    pub fn init() Self {
        return .{
            .lexer = Lexer.new(),
        };
    }

    pub fn compile(self: *Self, source: []const u8) void {
        self.lexer.init(source);

        while (true) {
            const token = self.lexer.lex();

            print("Kind: {s}, ", .{@tagName(token.kind)});

            if (token.lexeme) |val| {
                print("value: {s}\n", .{val});
            } else {
                print("\n", .{});
            }

            if (token.kind == .Eof) {
                break;
            }
        }
    }
};
