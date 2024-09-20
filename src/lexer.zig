const std = @import("std");
const Allocator = std.mem.Allocator;

pub const LexerErr = error{
    UnterminatedString,
};

pub const TokenKind = enum {
    RightParen,
    LeftParen,
    RightBrace,
    LeftBrace,
    Colon,
    Comma,
    Dot,

    Plus,
    Minus,
    Star,
    Slash,
    Less,
    Greater,
    Bang,
    Equal,
    LessEqual,
    GreaterEqual,
    EqualEqual,
    BangEqual,

    Identifier,
    String,
    Float,
    Int,

    NewLine,
    Eof,
};

pub const Span = struct {
    start: usize,
    end: usize,
};

pub const Token = struct {
    kind: TokenKind,
    value: ?[]const u8,
    span: Span,
};

pub const Lexer = struct {
    code: []const u8,
    current: usize,
    start: usize,
    tokens: std.ArrayList(Token),

    pub fn init(allocator: Allocator) Lexer {
        return .{
            .code = undefined,
            .current = 0,
            .start = 0,
            .tokens = std.ArrayList(Token).init(allocator),
        };
    }

    pub fn reinit(self: *Lexer) void {
        self.code = undefined;
        self.tokens.clearAndFree();
        self.current = 0;
        self.start = 0;
    }

    pub fn deinit(self: Lexer) void {
        self.tokens.deinit();
    }

    pub fn lex(self: *Lexer, code: []const u8) ![]Token {
        self.code = code;

        while (!self.eof()) {
            self.skip_space();
            self.start = self.current;

            if (std.ascii.isAlphabetic(self.peek())) {
                try self.identifier();
            } else if (std.ascii.isDigit(self.peek())) {
                try self.number();
            } else {
                try switch (self.advance()) {
                    '(' => self.add_token(.LeftParen),
                    ')' => self.add_token(.RightParen),
                    '{' => self.add_token(.LeftBrace),
                    '}' => self.add_token(.RightBrace),
                    ':' => self.add_token(.Colon),
                    ',' => self.add_token(.Comma),
                    '.' => self.add_token(.Dot),
                    '+' => self.add_token(.Plus),
                    '-' => self.add_token(.Minus),
                    '*' => self.add_token(.Star),
                    '/' => self.add_token(.Slash),
                    '<' => self.add_if_equal_tk_else(.LessEqual, .Less),
                    '>' => self.add_if_equal_tk_else(.GreaterEqual, .Greater),
                    '!' => self.add_if_equal_tk_else(.BangEqual, .Bang),
                    '=' => self.add_if_equal_tk_else(.EqualEqual, .Equal),
                    '"' => self.string(),
                    '\n' => self.add_token(.NewLine),
                    else => @panic("unsupported token"),
                };
            }
        }

        return self.tokens.items;
    }

    fn string(self: *Lexer) !void {
        self.start += 1;

        while (!self.eof() and self.peek() != '"') {
            _ = self.advance();
        }

        if (self.eof()) {
            return error.UnterminatedString;
        }

        try self.add_token_value(.String);
        _ = self.advance();
    }

    fn identifier(self: *Lexer) !void {
        while (!self.eof() and std.ascii.isAlphabetic(self.peek())) {
            _ = self.advance();
        }

        try self.add_token_value(.Identifier);
    }

    fn number(self: *Lexer) !void {
        while (!self.eof() and std.ascii.isDigit(self.peek())) {
            _ = self.advance();
        }

        try self.add_token_value(.Int);
    }

    fn add_token(self: *Lexer, kind: TokenKind) !void {
        try self.tokens.append(.{
            .kind = kind,
            .value = null,
            .span = .{ .start = self.start, .end = self.current },
        });
    }

    fn add_token_value(self: *Lexer, kind: TokenKind) !void {
        try self.tokens.append(.{
            .kind = kind,
            .value = self.code[self.start..self.current],
            .span = .{ .start = self.start, .end = self.current },
        });
    }

    fn add_if_equal_tk_else(self: *Lexer, if_equal: TokenKind, else_: TokenKind) !void {
        if (!self.eof() and self.peek() == '=') {
            _ = self.advance();
            try self.add_token(if_equal);
        } else {
            try self.add_token(else_);
        }
    }

    fn advance(self: *Lexer) u8 {
        self.current += 1;
        return self.code[self.current - 1];
    }

    fn peek(self: Lexer) u8 {
        return self.code[self.current];
    }

    fn skip_space(self: *Lexer) void {
        while (!self.eof() and self.peek() == ' ') {
            _ = self.advance();
        }
    }

    fn eof(self: Lexer) bool {
        return self.current >= self.code.len;
    }
};
