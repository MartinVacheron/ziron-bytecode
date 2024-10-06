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

            if (!self.check(.Error)) break;

            self.error_at_current(self.current.lexeme);
        }
    }

    fn match(self: *Self, kind: TokenKind) bool {
        if (!self.check(kind)) return false;

        self.advance();
        return true;
    }

    fn check(self: *Self, kind: TokenKind) bool {
        return self.current.kind == kind;
    }

    fn consume(self: *Self, kind: TokenKind, msg: []const u8) void {
        if (self.check(kind)) {
            self.advance();
            return;
        }

        self.error_at_current(msg);
    }

    fn consume_skip(self: *Self, kind: TokenKind, msg: []const u8) void {
        self.consume(kind, msg);
        self.skip_new_lines();
    }

    fn skip_new_lines(self: *Self) void {
        while (self.check(.NewLine)) {
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

    fn synchronize(self: *Self) void {
        self.panic_mode = false;

        while (!self.check(.Eof)) {
            switch (self.current.kind) {
                .Fn, .For, .If, .Print, .Return, .Struct, .Var, .While => return,
                else => self.advance(),
            }
        }
    }
};

const Local = struct {
    name: []const u8,
    depth: isize,
};

pub const Compiler = struct {
    parser: Parser,
    chunk: *Chunk,
    vm: *Vm,
    locals: [std.math.maxInt(u8) + 1]Local,
    local_count: u8,
    scope_depth: usize,

    const Self = @This();

    pub const CompileErr = error{
        CompileErr,
    } || Allocator.Error;

    const ParseRule = struct {
        prefix: ?*const fn (*Compiler, bool) Allocator.Error!void = null,
        infix: ?*const fn (*Compiler, bool) Allocator.Error!void = null,
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
        .Identifier = ParseRule{ .prefix = Self.variable },
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
            .locals = [_]Local{.{ .name = undefined, .depth = 0 }} ** 256,
            .local_count = 0,
            .scope_depth = 0,
        };
    }

    pub fn init(self: *Self, vm: *Vm) void {
        self.vm = vm;
    }

    // NOTE: for ternary https://chidiwilliams.com/posts/on-recursive-descent-and-pratt-parsing
    pub fn compile(self: *Self, source: []const u8, chunk: *Chunk) CompileErr!void {
        self.chunk = chunk;
        self.parser.lexer.init(source);
        self.parser.advance();

        while (!self.parser.match(.Eof)) {
            self.parser.skip_new_lines();
            try self.declaration();
            self.parser.skip_new_lines();

            if (self.parser.panic_mode) self.parser.synchronize();
        }

        try self.end_compiler();

        if (self.parser.had_error) return error.CompileErr;
    }

    fn parse_precedence(self: *Self, precedence: Precedence) Allocator.Error!void {
        self.parser.advance();
        const prefix_rule = get_rule(self.parser.previous.kind).prefix orelse {
            self.parser.err("expect expression");
            return;
        };

        // We can assign only if already in assignment or expr_stmt
        const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.Assignment);
        try prefix_rule(self, can_assign);

        while (@intFromEnum(precedence) <= @intFromEnum(get_rule(self.parser.current.kind).precedence)) {
            self.parser.advance();

            const infix_rule = get_rule(self.parser.previous.kind).infix orelse {
                self.parser.err("internal error, not in book: https://craftinginterpreters.com/compiling-expressions.html#parsing-with-precedence");
                return;
            };

            try infix_rule(self, can_assign);
        }

        // If we went through all the loop and that we tried to assign with
        // a wrong target, nobody will have consumed the '='. It means the
        // assignment target was invalid
        if (can_assign and self.parser.match(.Equal)) {
            self.parser.err("invalid assignment target");
        }
    }

    fn get_rule(kind: TokenKind) *const ParseRule {
        return parser_rule_table.getPtrConst(kind);
    }

    fn begin_scope(self: *Self) void {
        self.scope_depth += 1;
    }

    fn end_scope(self: *Self) Allocator.Error!void {
        self.scope_depth -= 1;

        while (self.local_count > 0 and self.locals[self.local_count - 1].depth > self.scope_depth) {
            try self.emit_byte(.Pop);
            self.local_count -= 1;
        }
    }

    /// Consume variable lexeme and add it to the chunk's constants
    /// Returns its index
    fn parse_variable(self: *Self, msg: []const u8) Allocator.Error!u8 {
        self.parser.consume(.Identifier, msg);

        self.declare_variable();
        if (self.scope_depth > 0) return 0;

        return self.identifier_constant(&self.parser.previous);
    }

    fn declare_variable(self: *Self) void {
        if (self.scope_depth == 0) return;

        const token = &self.parser.previous;

        var i: u8 = self.local_count;
        while (i >= 0) : (i -= 1) {
            const local = self.locals[i];

            if (local.depth != -1 and local.depth < self.scope_depth) break;

            if (Compiler.identifier_equal(token.lexeme, local.name)) {
                self.parser.err("already a variable with this name in this scope");
            }
        }

        self.add_local(token.lexeme);
    }

    /// Creates a ObjString value and store it in the chunk's constants
    /// Returns its index
    fn identifier_constant(self: *Self, name: *const Token) Allocator.Error!u8 {
        const str = try ObjString.copy(self.vm, name.lexeme);
        return self.make_constant(Value.obj(str.as_obj()));
    }

    fn identifier_equal(name1: []const u8, name2: []const u8) bool {
        if (name1.len != name2.len) return false;
        return std.mem.eql(u8, name1, name2);
    }

    fn add_local(self: *Self, name: []const u8) void {
        if (self.local_count == std.math.maxInt(u8) + 1) {
            self.parser.err("too many local variables in function");
            return;
        }

        var local = &self.locals[self.local_count];
        local.name = name;
        local.depth = -1; // Sentinel value to mark it uninitialized
        self.local_count += 1;
    }

    // i16 because we can return a -1 and otherwise it's all values for u8
    fn resolve_local(self: *Self, token: *const Token) i16 {
        print("Start resolving local...\n", .{});
        var i: u8 = self.local_count;
        print("local count: {}\n", .{i});

        // NOTE: in book, >= 0, but if it is 0, there is no local
        while (i > 0) : (i -= 1) {
            const local = &self.locals[i - 1];
            print("local: {s}\n", .{local.name});

            if (Compiler.identifier_equal(local.name, token.lexeme)) {
                if (local.depth == -1) {
                    self.parser.err("can't read local variable in its own initializer");
                }

                return @intCast(i - 1);
            }
        }

        return -1;
    }

    // PERF: define multiple time the same one potentially
    // https://craftinginterpreters.com/global-variables.html#challenges [1]
    fn define_variable(self: *Self, global_id: u8) Allocator.Error!void {
        // Local variable are automatically implicitly created when
        // we parse its initializer. It's right on top of the stack
        if (self.scope_depth > 0) {
            self.mark_initialized();
            return;
        }

        try self.emit_bytes_u8(.DefineGlobal, global_id);
    }

    fn mark_initialized(self: *Self) void {
        self.locals[self.local_count - 1].depth = @intCast(self.scope_depth);
    }

    fn declaration(self: *Self) Allocator.Error!void {
        self.parser.skip_new_lines();

        if (self.parser.match(.Var)) {
            try self.var_declaration();
        } else {
            try self.statement();
        }
    }

    fn var_declaration(self: *Self) Allocator.Error!void {
        const global_id = try self.parse_variable("expect variable name");

        if (self.parser.match(.Equal)) {
            try self.expression();
        } else {
            try self.emit_byte(.Null);
        }

        self.parser.consume(.NewLine, "expect new line after variable declaration");
        try self.define_variable(global_id);
    }

    fn statement(self: *Self) Allocator.Error!void {
        if (self.parser.match(.Print)) {
            try self.print_statement();
        } else if (self.parser.match(.LeftBrace)) {
            self.begin_scope();
            try self.block();
            try self.end_scope();
        } else if (self.parser.match(.If)) {
            try self.if_statement();
        } else {
            try self.expression_statement();
        }
    }

    fn print_statement(self: *Self) Allocator.Error!void {
        try self.expression();
        try self.emit_byte(.Print);
    }

    fn block(self: *Self) Allocator.Error!void {
        self.parser.skip_new_lines();

        while (!self.parser.check(.RightBrace) and !self.parser.check(.Eof)) {
            try self.declaration();
            self.parser.skip_new_lines();
        }

        self.parser.consume(.RightBrace, "expect '}' after block");
    }

    fn if_statement(self: *Self) Allocator.Error!void {
        try self.expression();
        self.parser.skip_new_lines();

        const then_jump = try self.emit_jump(.JumpIfFalse);
        // Pop the condition inside then branch if the condition is true
        try self.emit_byte(.Pop);
        try self.statement();

        const else_jump = try self.emit_jump(.Jump);
        self.patch_jump(then_jump);

        // Pop the condition inside else branch if the condition is false
        try self.emit_byte(.Pop);

        if (self.parser.match(.Else)) try self.statement();
        self.patch_jump(else_jump);
    }

    // In the book, expr_stmt is: expr + ;
    // For us, no difference. The Pop is because the result of expr
    // is not used, because it's a statement like: brunch = "quiche"
    fn expression_statement(self: *Self) Allocator.Error!void {
        try self.expression();
        try self.emit_byte(.Pop);
    }

    fn expression(self: *Self) Allocator.Error!void {
        try self.parse_precedence(.Assignment);
    }

    fn grouping(self: *Self, _: bool) Allocator.Error!void {
        try self.expression();
        self.parser.consume(.RightParen, "expect ')' after expression");
    }

    fn binary(self: *Self, _: bool) Allocator.Error!void {
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

    fn unary(self: *Self, _: bool) Allocator.Error!void {
        const operator = self.parser.previous.kind;
        try self.parse_precedence(.Unary);

        try switch (operator) {
            .Minus => self.emit_byte(.Negate),
            .Bang => self.emit_byte(.Not),
            else => unreachable,
        };
    }

    fn literal(self: *Self, _: bool) Allocator.Error!void {
        try switch (self.parser.previous.kind) {
            .False => self.emit_byte(.False),
            .Null => self.emit_byte(.Null),
            .True => self.emit_byte(.True),
            else => unreachable,
        };
    }

    fn int(self: *Self, _: bool) Allocator.Error!void {
        const prev = self.parser.previous.lexeme;
        const value = std.fmt.parseInt(i64, prev, 10) catch unreachable;
        try self.emit_constant(Value.int(value));
    }

    fn float(self: *Self, _: bool) Allocator.Error!void {
        const prev = self.parser.previous.lexeme;
        const value = std.fmt.parseFloat(f64, prev) catch unreachable;
        try self.emit_constant(Value.float(value));
    }

    fn string(self: *Self, _: bool) Allocator.Error!void {
        const lexeme = self.parser.previous.lexeme;
        // Skip quotes
        const object = try ObjString.copy(self.vm, lexeme[1 .. lexeme.len - 1]);
        const value = Value.obj(object.as_obj());

        try self.emit_constant(value);
    }

    fn variable(self: *Self, can_assign: bool) Allocator.Error!void {
        try self.named_variable(&self.parser.previous, can_assign);
    }

    /// Creates a local or global variable and a Set/Get OpCode
    fn named_variable(self: *Self, name: *const Token, can_assign: bool) Allocator.Error!void {
        var var_id = self.resolve_local(name);
        // NOTE: initialize with local and just (if (.. == -1)) ?
        var set_op: OpCode = undefined;
        var get_op: OpCode = undefined;

        print("local id for var '{s}': {}\n\n", .{ name.lexeme, var_id });

        if (var_id != -1) {
            set_op = .SetLocal;
            get_op = .GetLocal;
        } else {
            var_id = try self.identifier_constant(name);
            set_op = .SetGlobal;
            get_op = .GetGlobal;
        }

        if (can_assign and self.parser.match(.Equal)) {
            try self.expression();
            try self.emit_bytes_u8(set_op, @intCast(var_id));
        } else {
            try self.emit_bytes_u8(get_op, @intCast(var_id));
        }
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

    fn emit_jump(self: *Self, op: OpCode) Allocator.Error!usize {
        try self.emit_byte(op);
        try self.emit_byte_u8(0xff);
        try self.emit_byte_u8(0xff);

        return self.chunk.code.items.len - 2;
    }

    fn patch_jump(self: *Self, offset: usize) void {
        // -2 is the jump offset itself
        const jump: u16 = @intCast(self.chunk.code.items.len - offset - 2);

        if (jump > std.math.maxInt(u16)) {
            self.parser.err("too much code to jump over");
        }

        self.chunk.code.items[offset] = @as(u8, @intCast(jump >> 8)) & 0xff;
        self.chunk.code.items[offset + 1] = @intCast(jump & 0xff);
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

    /// Writes a constant to the chunk. Returns its index
    fn make_constant(self: *Self, value: Value) Allocator.Error!u8 {
        const constant = try self.chunk.write_constant(value);

        // NOTE: for now, unnecessary check because it writes to an ArrayList
        if (constant > std.math.maxInt(u8)) {
            self.parser.err("too many constants in one chunk");
            return 0;
        }

        return @truncate(constant);
    }
};
