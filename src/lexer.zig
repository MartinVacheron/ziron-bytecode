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
    Int,
    Float,
    String,

    And,
    Else,
    False,
    For,
    Fn,
    If,
    In,
    Null,
    Or,
    Print,
    Return,
    Self,
    Struct,
    True,
    Var,
    While,

    NewLine,
    Error,
    Eof,
};

pub const Token = struct {
    kind: TokenKind,
    lexeme: []const u8,
    line: u32,

    pub fn empty() Token {
        return .{ .kind = .Null, .lexeme = undefined, .line = 0 };
    }
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

        const c = self.advance();

        if (is_alpha(c)) return self.identifier();
        if (std.ascii.isDigit(c)) return self.number();

        return switch (c) {
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

    fn number(self: *Self) Token {
        while (!self.eof() and std.ascii.isDigit(self.peek())) {
            _ = self.advance();
        }

        if (self.match('.')) {
            while (!self.eof() and std.ascii.isDigit(self.peek())) {
                _ = self.advance();
            }

            return self.make_token(.Float);
        }

        return self.make_token(.Int);
    }

    fn identifier(self: *Self) Token {
        while (!self.eof() and (is_alpha(self.peek()) or std.ascii.isDigit(self.peek()))) {
            _ = self.advance();
        }

        return self.make_token(self.identifier_type());
    }

    fn identifier_type(self: *Self) TokenKind {
        return switch (self.start[0]) {
            'a' => self.check_keyword(1, 2, "nd", .And),
            'e' => self.check_keyword(1, 3, "lse", .Else),
            'f' => blk: {
                if (self.current > 1) {
                    break :blk switch (self.start[1]) {
                        'a' => self.check_keyword(2, 3, "lse", .False),
                        'o' => self.check_keyword(2, 1, "r", .For),
                        'n' => self.check_keyword(2, 0, "", .Fn),
                        else => .Identifier,
                    };
                } else {
                    break :blk .Identifier;
                }
            },
            'i' => blk: {
                if (self.current > 1) {
                    break :blk switch (self.start[1]) {
                        'f' => .If,
                        'n' => .In,
                        else => .Identifier,
                    };
                } else {
                    break :blk .Identifier;
                }
            },
            'n' => self.check_keyword(1, 3, "ull", .Null),
            'o' => self.check_keyword(1, 1, "r", .Or),
            'p' => self.check_keyword(1, 4, "rint", .Print),
            'r' => self.check_keyword(1, 5, "eturn", .Return),
            's' => blk: {
                if (self.current > 1) {
                    break :blk switch (self.start[1]) {
                        't' => self.check_keyword(2, 4, "ruct", .Struct),
                        'e' => self.check_keyword(2, 2, "lf", .Self),
                        else => .Identifier,
                    };
                } else {
                    break :blk .Identifier;
                }
            },
            't' => self.check_keyword(1, 3, "rue", .True),
            'v' => self.check_keyword(1, 2, "ar", .Var),
            'w' => self.check_keyword(1, 4, "hile", .While),
            else => .Identifier,
        };
    }

    fn check_keyword(self: *const Self, start: u8, len: u8, rest: []const u8, kind: TokenKind) TokenKind {
        if (self.current - start == len and std.mem.eql(u8, self.start[start .. start + len], rest)) {
            return kind;
        }

        return .Identifier;
    }

    fn peek(self: *const Self) u8 {
        return self.start[self.current];
    }

    fn peek_next(self: *const Self) u8 {
        if (self.current + 1 >= self.start.len) {
            return 0;
        }

        return self.start[self.current + 1];
    }

    fn advance(self: *Self) u8 {
        self.current += 1;
        return self.start[self.current - 1];
    }

    fn make_token(self: *const Self, kind: TokenKind) Token {
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

    fn error_token(self: *const Self, msg: []const u8) Token {
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
                    } else break;
                },
                else => break,
            }
        }
    }

    fn eof(self: *const Self) bool {
        return self.current >= self.start.len;
    }
};

fn is_alpha(c: u8) bool {
    return std.ascii.isAlphabetic(c) or c == '_';
}

// ------------
//  Tests
// ------------
const expect = std.testing.expect;

test "ident and strings" {
    var lexer = Lexer.new();
    lexer.init("foo bar variable truth");

    const res = [_]Token{
        .{ .kind = .Identifier, .lexeme = "foo", .line = 0 },
        .{ .kind = .Identifier, .lexeme = "bar", .line = 0 },
    };

    for (0..res.len) |i| {
        const tk = lexer.lex();
        try expect(tk.kind == res[i].kind);
        try expect(std.mem.eql(u8, tk.lexeme, res[i].lexeme));
    }
}

test "numbers" {
    var lexer = Lexer.new();
    lexer.init("123 45.6 7.");

    const res = [3]Token{
        .{ .kind = .Int, .lexeme = "123", .line = 0 },
        .{ .kind = .Float, .lexeme = "45.6", .line = 0 },
        .{ .kind = .Float, .lexeme = "7.", .line = 0 },
    };

    for (0..res.len) |i| {
        const tk = lexer.lex();
        try expect(tk.kind == res[i].kind);
        try expect(std.mem.eql(u8, tk.lexeme, res[i].lexeme));
    }
}

test "tokens" {
    var lexer = Lexer.new();
    lexer.init("(){}.:,=!< ><= >= !=+-*/");

    const res = [_]TokenKind{
        .LeftParen,    .RightParen, .LeftBrace, .RightBrace, .Dot,     .Colon,
        .Comma,        .Equal,      .Bang,      .Less,       .Greater, .LessEqual,
        .GreaterEqual, .BangEqual,  .Plus,      .Minus,      .Star,    .Slash,
    };

    for (0..res.len) |i| {
        const tk = lexer.lex();
        try expect(tk.kind == res[i]);
    }
}

test "keywords" {
    var lexer = Lexer.new();
    lexer.init("and else false for fn if in null or print return self struct true var while");

    const res = [_]TokenKind{ .And, .Else, .False, .For, .Fn, .If, .In, .Null, .Or, .Print, .Return, .Self, .Struct, .True, .Var, .While, .Eof };

    for (0..res.len) |i| {
        const tk = lexer.lex();
        try expect(tk.kind == res[i]);
    }
}
