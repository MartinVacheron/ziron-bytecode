const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const config = @import("config");
const Lexer = @import("lexer.zig").Lexer;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Token = @import("lexer.zig").Token;
const TokenKind = @import("lexer.zig").TokenKind;
const Value = @import("values.zig").Value;
const ObjString = @import("obj.zig").ObjString;
const Vm = @import("vm.zig").Vm;

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
    lexer: Lexer,
    previous: Token,
    current: Token,
    had_error: bool,
    panic_mode: bool,

    const Self = @This();

    pub fn init() Self {
        return .{
            .lexer = Lexer.new(),
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

    fn skip_new_lines(self: *Self) void {
        while (self.current.kind == .NewLine) {
            self.advance();
        }
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
    parser: Parser,
    chunk: *Chunk,
    vm: *Vm,

    const Self = @This();

    pub const CompileErr = error{
        CompileErr,
    } || Allocator.Error;

    const ParseRule = struct {
        prefix: ?*const fn (*Compiler) Allocator.Error!void = null,
        infix: ?*const fn (*Compiler) Allocator.Error!void = null,
        precedence: Precedence = .None,
    };

    const parser_rule_table = std.EnumArray(TokenKind, ParseRule).init(.{
        .And = ParseRule{},
        .Bang = ParseRule{ .prefix = Self.unary },
        .BangEqual = ParseRule{ .infix = Self.binary, .precedence = .Equality },
        .Colon = ParseRule{},
        .Comma = ParseRule{},
        .Dot = ParseRule{},
        .Else = ParseRule{},
        .Eof = ParseRule{},
        .Equal = ParseRule{},
        .EqualEqual = ParseRule{ .infix = Self.binary, .precedence = .Equality },
        .Error = ParseRule{},
        .False = ParseRule{ .prefix = Self.literal },
        .Float = ParseRule{ .prefix = Self.float },
        .Fn = ParseRule{},
        .For = ParseRule{},
        .Greater = ParseRule{ .infix = Self.binary, .precedence = .Comparison },
        .GreaterEqual = ParseRule{ .infix = Self.binary, .precedence = .Comparison },
        .Identifier = ParseRule{},
        .If = ParseRule{},
        .In = ParseRule{},
        .Int = ParseRule{ .prefix = Self.int },
        .LeftBrace = ParseRule{},
        .LeftParen = ParseRule{ .prefix = Self.grouping },
        .Less = ParseRule{ .infix = Self.binary, .precedence = .Comparison },
        .LessEqual = ParseRule{ .infix = Self.binary, .precedence = .Comparison },
        .Minus = ParseRule{ .prefix = Self.unary, .infix = Self.binary, .precedence = .Term },
        .NewLine = ParseRule{},
        .Null = ParseRule{ .prefix = Self.literal },
        .Or = ParseRule{},
        .Plus = ParseRule{ .infix = Self.binary, .precedence = .Term },
        .Print = ParseRule{},
        .Return = ParseRule{},
        .RightParen = ParseRule{},
        .RightBrace = ParseRule{},
        .Slash = ParseRule{ .infix = Self.binary, .precedence = .Factor },
        .Star = ParseRule{ .infix = Self.binary, .precedence = .Factor },
        .String = ParseRule{ .prefix = Self.string },
        .Struct = ParseRule{},
        .Self = ParseRule{},
        .True = ParseRule{ .prefix = Self.literal },
        .Var = ParseRule{},
        .While = ParseRule{},
    });

    pub fn new() Self {
        return .{
            .parser = Parser.init(),
            .chunk = undefined,
            .vm = undefined,
        };
    }

    pub fn init(self: *Self, vm: *Vm) void {
        self.vm = vm;
    }

    pub fn compile(self: *Self, source: []const u8, chunk: *Chunk) CompileErr!void {
        self.chunk = chunk;
        self.parser.lexer.init(source);
        self.parser.advance();
        try self.expression();

        self.parser.consume(.Eof, "expected end of expression");

        try self.end_compiler();

        if (self.parser.had_error) return error.CompileErr;
    }

    // NOTE: for ternary https://chidiwilliams.com/posts/on-recursive-descent-and-pratt-parsing

    fn expression(self: *Self) Allocator.Error!void {
        self.parser.skip_new_lines();
        try self.parse_precedence(.Assignment);
        self.parser.skip_new_lines();
    }

    fn parse_precedence(self: *Self, precedence: Precedence) Allocator.Error!void {
        self.parser.advance();
        const prefix_rule = get_rule(self.parser.previous.kind).prefix orelse {
            self.parser.err("expect expression");
            return;
        };

        try prefix_rule(self);

        while (@intFromEnum(precedence) <= @intFromEnum(get_rule(self.parser.current.kind).precedence)) {
            self.parser.advance();

            const infix_rule = get_rule(self.parser.previous.kind).infix orelse {
                self.parser.err("internal error, not in book: https://craftinginterpreters.com/compiling-expressions.html#parsing-with-precedence");
                return;
            };

            try infix_rule(self);
        }
    }

    fn get_rule(kind: TokenKind) *const ParseRule {
        return parser_rule_table.getPtrConst(kind);
    }

    fn grouping(self: *Self) Allocator.Error!void {
        try self.expression();
        self.parser.consume(.RightParen, "expect ')' after expression");
    }

    fn binary(self: *Self) Allocator.Error!void {
        const operator = self.parser.previous.kind;
        const rule = get_rule(operator);

        // +1 allow left associative like: ((1 + 2) + 3) + 4
        //  otherwise it would do: a = (b = (c = d)) -- assignment is right associative
        try self.parse_precedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

        try switch (operator) {
            .BangEqual => self.emit_bytes(.Equal, .Not),
            .EqualEqual => self.emit_byte(.Equal),
            .Greater => self.emit_byte(.Greater),
            .GreaterEqual => self.emit_bytes(.Less, .Not),
            .Less => self.emit_byte(.Less),
            .LessEqual => self.emit_bytes(.Greater, .Not),
            .Minus => self.emit_byte(.Subtract),
            .Plus => self.emit_byte(.Add),
            .Star => self.emit_byte(.Multiply),
            .Slash => self.emit_byte(.Divide),
            else => unreachable,
        };
    }

    fn unary(self: *Self) Allocator.Error!void {
        const operator = self.parser.previous.kind;
        try self.parse_precedence(.Unary);

        try switch (operator) {
            .Minus => self.emit_byte(.Negate),
            .Bang => self.emit_byte(.Not),
            else => unreachable,
        };
    }

    fn literal(self: *Self) Allocator.Error!void {
        try switch (self.parser.previous.kind) {
            .False => self.emit_byte(.False),
            .Null => self.emit_byte(.Null),
            .True => self.emit_byte(.True),
            else => unreachable,
        };
    }

    fn int(self: *Self) Allocator.Error!void {
        const prev = self.parser.previous.lexeme;
        const value = std.fmt.parseInt(i64, prev, 10) catch unreachable;
        try self.emit_constant(Value.int(value));
    }

    fn float(self: *Self) Allocator.Error!void {
        const prev = self.parser.previous.lexeme;
        const value = std.fmt.parseFloat(f64, prev) catch unreachable;
        try self.emit_constant(Value.float(value));
    }

    fn string(self: *Self) Allocator.Error!void {
        const lexeme = self.parser.previous.lexeme;
        // Skip quotes
        const object = try ObjString.create(self.vm, lexeme[1 .. lexeme.len - 1]);
        const value = Value.obj(object.as_obj());

        try self.emit_constant(value);
    }

    fn emit_byte(self: *Self, op: OpCode) Allocator.Error!void {
        try self.chunk.write_op(op, @truncate(self.parser.previous.line));
    }

    fn emit_byte_u8(self: *Self, byte: u8) Allocator.Error!void {
        try self.chunk.write_byte(byte, @truncate(self.parser.previous.line));
    }

    fn emit_bytes(self: *Self, op1: OpCode, op2: OpCode) Allocator.Error!void {
        try self.emit_byte(op1);
        try self.emit_byte(op2);
    }

    fn emit_bytes_u8(self: *Self, op1: OpCode, byte: u8) Allocator.Error!void {
        try self.emit_byte(op1);
        try self.emit_byte_u8(byte);
    }

    fn end_compiler(self: *Self) Allocator.Error!void {
        if (config.PRINT_CODE) {
            if (!self.parser.had_error) {
                const Disassembler = @import("disassembler.zig").Disassembler;
                const dis = Disassembler.init(&self.chunk);
                dis.dis_chunk("code");
            }
        }

        try self.emit_return();
    }

    fn emit_return(self: *Self) Allocator.Error!void {
        try self.emit_byte(.Return);
    }

    fn emit_constant(self: *Self, value: Value) Allocator.Error!void {
        try self.emit_bytes_u8(.Constant, try self.make_constant(value));
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
