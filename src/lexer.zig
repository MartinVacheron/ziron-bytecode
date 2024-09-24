const std = @import("std");
const Allocator = std.mem.Allocator;

pub const LexerErr = error{
    UnterminatedString,
};

pub const TokenKind = enum {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Colon,
    Comma,
    Dot,

    Plus,
    Minus,
    Star,
    Slash,

    Bang,
    Equal,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    EqualEqual,
    BangEqual,

    Identifier,
    String,
    Float,
    Int,

    And,
    Struct,
    Else,
    False,
    For,
    Fn,
    If,
    Null,
    Or,
    Print,
    Return,
    Self,
    True,
    Var,
    While,

    NewLine,
    Error,
    Eof,
};

pub const Token = struct {
    kind: TokenKind,
    lexeme: ?[]const u8,
    line: u32,
};

pub const Lexer = struct {
    start: []const u8,
    current: usize,
    line: u32,

    const Self = @This();

    pub fn new() Self {
        return .{
            .start = undefined,
            .current = 0,
            .line = 1,
        };
    }

    pub fn init(self: *Self, source: []const u8) void {
        self.start = source;
        self.current = 0;
        self.line = 1;
    }

    pub fn lex(self: *Self) Token {
        self.skip_space();
        self.start = self.start[self.current..];
        self.current = 0;

        if (self.eof()) {
            return self.make_token(.Eof);
        }

        return switch (self.advance()) {
            '(' => self.make_token(.LeftParen),
            ')' => self.make_token(.RightParen),
            '{' => self.make_token(.LeftBrace),
            '}' => self.make_token(.RightBrace),
            ':' => self.make_token(.Colon),
            ',' => self.make_token(.Comma),
            '.' => self.make_token(.Dot),
            '+' => self.make_token(.Plus),
            '-' => self.make_token(.Minus),
            '*' => self.make_token(.Star),
            '/' => self.make_token(.Slash),
            '<' => self.make_if_equal_or_else(.LessEqual, .Less),
            '>' => self.make_if_equal_or_else(.GreaterEqual, .Greater),
            '!' => self.make_if_equal_or_else(.BangEqual, .Bang),
            '=' => self.make_if_equal_or_else(.EqualEqual, .Equal),
            '"' => self.string(),
            '\n' => blk: {
                self.line += 1;
                break :blk self.make_token(.NewLine);
            },
            else => self.error_token("Unexpected character"),
        };
    }

    fn string(self: *Self) Token {
        while (!self.eof() and self.peek() != '"') {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }

        if (self.eof()) {
            return self.error_token("unterminated string");
        }

        _ = self.advance();
        return self.make_token(.String);
    }

    fn identifier(self: *Self) Token {
        while (!self.eof() and std.ascii.isAlphabetic(self.peek())) {
            _ = self.advance();
        }

        return self.add_token(.Identifier);
    }

    fn number(self: *Self) Token {
        while (!self.eof() and std.ascii.isDigit(self.peek())) {
            _ = self.advance();
        }

        return self.add_token(.Int);
    }

    fn peek(self: Self) u8 {
        return self.start[self.current];
    }

    fn peek_next(self: Self) u8 {
        if (self.current + 1 > self.start.len) {
            return 0;
        }

        return self.start[self.current + 1];
    }

    fn advance(self: *Self) u8 {
        self.current += 1;
        return self.start[self.current - 1];
    }

    fn make_token(self: Self, kind: TokenKind) Token {
        return .{
            .kind = kind,
            .lexeme = self.start[0..self.current],
            .line = self.line,
        };
    }
    fn make_if_equal_or_else(self: *Self, if_equal: TokenKind, else_: TokenKind) Token {
        if (self.match('=')) {
            return self.make_token(if_equal);
        } else {
            return self.make_token(else_);
        }
    }

    fn error_token(self: Self, msg: []const u8) Token {
        return .{
            .kind = .Error,
            .lexeme = msg,
            .line = self.line,
        };
    }

    fn match(self: *Self, char: u8) bool {
        if (self.eof()) return false;
        if (self.start[self.current] != char) return false;

        self.current += 1;
        return true;
    }

    fn skip_space(self: *Self) void {
        while (!self.eof()) {
            const c = self.peek();

            switch (c) {
                ' ', '\r' => _ = self.advance(),
                '/' => {
                    if (self.peek_next() == '/') {
                        while (!self.eof() and self.peek() != '\n') {
                            _ = self.advance();
                        }
                    }
                },
                else => break,
            }
        }
    }

    fn eof(self: Self) bool {
        return self.current >= self.start.len;
    }
};
