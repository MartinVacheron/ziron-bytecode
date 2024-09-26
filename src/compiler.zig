const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const Lexer = @import("lexer.zig").Lexer;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Token = @import("lexer.zig").Token;
const TokenKind = @import("lexer.zig").TokenKind;
const Value = @import("values.zig").Value;

const Precedence = enum {
    None,
    Assignment, // =
    Or, // or
    And, // and
    Equality, // == !=
    Comparison, // < > <= >=
    Term, // + -
    Factor, // * /
    Unary, // ! -
    Call, // . ()
    Primary,
};

const Parser = struct {
    previous: Token,
    current: Token,
    had_error: bool,
    panic_mode: bool,

    const Self = @This();

    pub fn init() Self {
        return .{
            .previous = Token.empty(),
            .current = Token.empty(),
            .had_error = false,
            .panic_mode = false,
        };
    }

    fn advance(self: *Self) void {
        self.previous = self.current;

        while (true) {
            self.current = self.lexer.lex();

            if (self.current.kind != .Error) break;

            self.error_at_current(self.current.lexeme);
        }
    }

    fn consume(self: *Self, kind: TokenKind, msg: []const u8) void {
        if (self.current.kind == kind) {
            self.advance();
            return;
        }

        self.error_at_current(msg);
    }

    fn error_at_current(self: *Self, msg: []const u8) void {
        self.error_at(&self.current, msg);
    }

    fn err(self: *Self, msg: []const u8) void {
        self.error_at(&self.previous, msg);
    }

    fn error_at(self: *Self, token: *const Token, msg: []const u8) void {
        if (self.panic_mode) return;
        self.panic_mode = true;

        print("[line {}] Error", .{token.line});

        if (token.kind == .Eof) {
            print(" at end", .{});
        } else if (token.kind == .Error) {} else {
            print(" at '{s}'", .{token.lexeme});
        }

        print(": {s}\n", .{msg});

        self.had_error = true;
    }
};

pub const Compiler = struct {
    lexer: Lexer,
    parser: Parser,
    chunk: *Chunk,

    const Self = @This();

    pub const CompileErr = error{
        CompileErr,
        ParserErr,
    };

    const ParseRule = struct {
        prefix: ?*const fn (*Compiler) CompileErr!void = null,
        infix: ?*const fn (*Compiler) CompileErr!void = null,
        precedence: Precedence = .None,
    };

    const parser_rule_table = std.EnumArray(TokenKind, ParseRule).init(.{
        .LeftParen = ParseRule{ .prefix = Self.grouping },
        .RightParen = ParseRule{},
        .LeftBrace = ParseRule{},
        .RightBrace = ParseRule{},
        .Comma = ParseRule{},
        .Dot = ParseRule{},
        .Minus = ParseRule{ .prefix = Self.unary, .infix = Self.binary, .precedence = .Term },
        .Plus = ParseRule{ .infix = Self.binary, .precedence = .Term },
        .Slash = ParseRule{ .infix = Self.binary, .precedence = .Factor },
        .Star = ParseRule{ .infix = Self.binary, .precedence = .Factor },
        .Bang = ParseRule{},
        .BangEqual = ParseRule{},
        .Equal = ParseRule{},
        .EqualEqual = ParseRule{},
        .Greater = ParseRule{},
        .GreaterEqual = ParseRule{},
        .Less = ParseRule{},
        .LessEqual = ParseRule{},
        .Identifier = ParseRule{},
        .String = ParseRule{},
        .Number = ParseRule{ .prefix = Self.number },
        .And = ParseRule{},
        .Struct = ParseRule{},
        .Else = ParseRule{},
        .For = ParseRule{},
        .Fn = ParseRule{},
        .If = ParseRule{},
        .Null = ParseRule{},
        .Or = ParseRule{},
        .Print = ParseRule{},
        .Return = ParseRule{},
        .Self = ParseRule{},
        .True = ParseRule{},
        .Var = ParseRule{},
        .While = ParseRule{},
        .Error = ParseRule{},
        .Eof = ParseRule{},
    });

    pub fn init() Self {
        return .{
            .lexer = Lexer.new(),
            .parser = Parser.init(),
        };
    }

    pub fn compile(self: *Self, source: []const u8, chunk: *Chunk) CompileErr!void {
        self.chunk = chunk;
        self.lexer.init(source);
        self.advance();
        self.expression();
        self.parser.consume(.Eof, "expected end of expression");

        self.end_compiler();

        if (self.parser.had_error) return error.ParserErr;
    }

    fn expression(self: *Self) CompileErr!void {
        self.parse_precedence(.Assignment);
    }

    fn parse_precedence(self: Self, precedence: Precedence) void {
        _ = self;
        _ = precedence;
    }

    fn get_rule(kind: TokenKind) *const ParseRule {
        return parser_rule_table.getPtrConst(kind);
    }

    fn grouping(self: *Self) CompileErr!void {
        self.expression();
        self.parser.consume(.RightParen, "expect ')' after expression");
    }

    fn binary(self: *Self) CompileErr!void {
        const operator = self.parser.previous.kind;
        const rule = self.get_rule(operator);

        // +1 allow left associative like: ((1 + 2) + 3) + 4
        //  otherwise it would do: a = (b = (c = d)) -- assignment is right associative
        self.parse_precedence(rule.precedence + 1);

        switch (operator) {
            .Plus => self.emit_byte(.Add),
            .Minus => self.emit_byte(.Subtract),
            .Slash => self.emit_byte(.Multiply),
            .Slash => self.emit_byte(.Divide),
            else => unreachable,
        }
    }

    fn unary(self: *Self) CompileErr!void {
        const operator = self.parser.previous.kind;
        self.parse_precedence(.Unary);

        switch (operator) {
            .Minus => self.emit_byte(.Negate),
            else => unreachable,
        }
    }

    fn number(self: *Self) CompileErr!void {
        const prev = self.parser.previous.lexeme;
        const value = std.fmt.parseFloat(f64, prev) catch unreachable;
        self.emit_contant(.{ .Float = value });
    }

    fn emit_byte(self: *Self, op: OpCode) void {
        self.chunk.write_op(op, self.parser.previous.line);
    }

    fn emit_bytes(self: *Self, op1: OpCode, op2: OpCode) void {
        self.emit_byte(op1);
        self.emit_byte(op2);
    }

    fn end_compiler(self: *Self) void {
        self.emit_return();
    }

    fn emit_return(self: *Self) void {
        self.emit_byte(.Return);
    }

    fn emit_constant(self: *Self, value: Value) void {
        self.emit_bytes(.Constant, self.make_constant(value));
    }

    fn make_constant(self: *Self, value: Value) Allocator.Error!u8 {
        const constant = try self.chunk.write_constant(value);
        if (constant > std.math.maxInt(u8)) {
            self.parser.err("too many constants in one chunk");
            return 0;
        }

        return @truncate(constant);
    }
};
