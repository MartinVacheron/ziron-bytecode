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
const ObjFunction = @import("obj.zig").ObjFunction;
const Vm = @import("vm.zig").Vm;

pub fn compile(vm: *Vm, source: []const u8) Compiler.CompileErr!*ObjFunction {
    var parser = Parser.init();
    parser.lexer.init(source);
    parser.advance();

    var compiler = Compiler.new();
    try compiler.init(vm, &parser, .Script, null);

    while (!parser.match(.Eof)) {
        parser.skip_new_lines();
        try compiler.declaration();
        parser.skip_new_lines();

        if (parser.panic_mode) parser.synchronize();
    }

    const func = try compiler.end_compiler();

    return if (parser.had_error) error.CompileErr else func;
}

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

const FnKind = enum {
    Fn,
    Initializer,
    Method,
    Script,
};

const Local = struct {
    name: []const u8,
    depth: isize,
    is_captured: bool,
};

const UpValue = struct {
    index: u8,
    is_local: bool,
};

// NOTE: avoided the enclosing compiler for now
pub const Compiler = struct {
    vm: *Vm,
    parser: *Parser,
    enclosing: ?*Compiler,
    function: *ObjFunction,
    kind: FnKind,
    locals: [std.math.maxInt(u8) + 1]Local,
    local_count: u8,
    scope_depth: usize,
    upvalues: [std.math.maxInt(u8) + 1]UpValue,
    in_struct: bool,

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
        .And = ParseRule{ .infix = Self.and_, .precedence = .And },
        .Bang = ParseRule{ .prefix = Self.unary },
        .BangEqual = ParseRule{ .infix = Self.binary, .precedence = .Equality },
        .Colon = ParseRule{},
        .Comma = ParseRule{},
        .Dot = ParseRule{ .infix = Self.dot, .precedence = .Call },
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
        .LeftParen = ParseRule{ .prefix = Self.grouping, .infix = Self.call, .precedence = .Call },
        .Less = ParseRule{ .infix = Self.binary, .precedence = .Comparison },
        .LessEqual = ParseRule{ .infix = Self.binary, .precedence = .Comparison },
        .Minus = ParseRule{ .prefix = Self.unary, .infix = Self.binary, .precedence = .Term },
        .NewLine = ParseRule{},
        .Null = ParseRule{ .prefix = Self.literal },
        .Or = ParseRule{ .infix = Self.or_, .precedence = .Or },
        .Plus = ParseRule{ .infix = Self.binary, .precedence = .Term },
        .Print = ParseRule{},
        .Return = ParseRule{},
        .RightParen = ParseRule{},
        .RightBrace = ParseRule{},
        .Slash = ParseRule{ .infix = Self.binary, .precedence = .Factor },
        .Star = ParseRule{ .infix = Self.binary, .precedence = .Factor },
        .String = ParseRule{ .prefix = Self.string },
        .Struct = ParseRule{},
        .Self = ParseRule{ .prefix = Self.self_ },
        .True = ParseRule{ .prefix = Self.literal },
        .Var = ParseRule{},
        .While = ParseRule{},
    });

    pub fn new() Self {
        return .{
            .vm = undefined,
            .parser = undefined,
            .enclosing = null,
            .function = undefined,
            .kind = .Script,
            .locals = [_]Local{.{ .name = undefined, .depth = 0, .is_captured = false }} ** 256,
            .local_count = 0,
            .scope_depth = 0,
            .upvalues = [_]UpValue{.{ .index = 0, .is_local = false }} ** 256,
            .in_struct = false,
        };
    }

    pub fn init(self: *Self, vm: *Vm, parser: *Parser, kind: FnKind, enclosing: ?*Self) Allocator.Error!void {
        self.vm = vm;
        self.parser = parser;
        self.kind = kind;
        self.enclosing = enclosing;

        var name: ?*ObjString = null;
        if (kind != .Script) {
            name = try ObjString.copy(self.vm, self.parser.previous.lexeme);
        }

        self.function = try ObjFunction.create(vm, name);

        if (kind == .Initializer or kind == .Method) {
            self.in_struct = true;
        } else {
            self.in_struct = false;
        }

        // Because the first value in the stack will be the function object
        // itself. We keep the first slot empty to be aligned to runtime stack
        const first_local = &self.locals[0];
        // Depth and is_captured already init in array declaration
        first_local.name = if (kind != .Fn) "self" else "";

        self.local_count += 1;
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
            if (self.locals[self.local_count - 1].is_captured) {
                try self.emit_byte(.CloseUpValue);
            } else {
                try self.emit_byte(.Pop);
            }

            self.local_count -= 1;
        }
    }

    /// Consume variable lexeme and add it either to locals and return 0
    /// or to the chunk's constants and return its index
    fn parse_variable(self: *Self, msg: []const u8) Allocator.Error!u8 {
        self.parser.consume(.Identifier, msg);

        self.declare_variable();
        if (self.scope_depth > 0) return 0;

        return self.identifier_constant(&self.parser.previous);
    }

    /// Take the previous token and add it as a local if scope_depth > 0
    fn declare_variable(self: *Self) void {
        if (self.scope_depth == 0) return;

        const token = &self.parser.previous;

        var i: u8 = self.local_count;
        while (i > 0) : (i -= 1) {
            const local = self.locals[i - 1];

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
        var i: u8 = self.local_count;

        while (i > 0) : (i -= 1) {
            const local = &self.locals[i - 1];

            if (Compiler.identifier_equal(local.name, token.lexeme)) {
                if (local.depth == -1) {
                    self.parser.err("can't read local variable in its own initializer");
                }

                return @intCast(i - 1);
            }
        }

        return -1;
    }

    /// Resolve upvalues in direct enclosing compiler if there is one, otherwise
    /// returns -1.
    /// We add the upavlue to the entire chain of enclosing compilers
    // See book https://craftinginterpreters.com/closures.html#flattening-upvalues
    fn resolve_upvalue(self: *Self, token: *const Token) i16 {
        if (self.enclosing) |enclosing| {
            const local_id = enclosing.resolve_local(token);

            if (local_id != -1) {
                enclosing.locals[@intCast(local_id)].is_captured = true;
                return self.add_upvalue(@intCast(local_id), true);
            }

            // We are forced to inform the enclosing like this because when
            // we'll return to the enclosing to finish its compilation,
            // the current compiler will be ended, we have no way anymore
            // to know if a local was captured. We do it now.
            const upvalue = enclosing.resolve_upvalue(token);

            if (upvalue != -1) {
                return self.add_upvalue(@intCast(upvalue), false);
            }
        }

        return -1;
    }

    fn add_upvalue(self: *Self, index: u8, is_local: bool) i16 {
        const upval_count = self.function.upvalue_count;

        // Checks if the upvalue isn't already created
        for (0..upval_count) |i| {
            const upval = &self.upvalues[i];

            if (upval.index == index and upval.is_local == is_local) {
                return @intCast(i);
            }
        }

        if (upval_count == std.math.maxInt(u8) + 1) {
            self.parser.err("too many closure variables in function");
            return 0;
        }

        self.upvalues[upval_count].is_local = is_local;
        self.upvalues[upval_count].index = index;
        self.function.upvalue_count += 1;
        return upval_count;
    }

    // PERF: define multiple time the same one potentially
    // https://craftinginterpreters.com/global-variables.html#challenges [1]

    /// If scope_depth > 0, just mark the local initialized, otherwise
    /// emits DefineGlobal with its index
    fn define_variable(self: *Self, global_id: u8) Allocator.Error!void {
        // Local variable are automatically implicitly created when
        // we parse its initializer. It's right on top of the stack
        if (self.scope_depth > 0) {
            self.mark_initialized();
            return;
        }

        try self.emit_bytes_u8(.DefineGlobal, global_id);
    }

    /// If scope_depth > 0, update the local's depth with current compiler's one
    fn mark_initialized(self: *Self) void {
        // Because fn definition calls this function and can be at top level
        // we add one check
        if (self.scope_depth == 0) return;

        self.locals[self.local_count - 1].depth = @intCast(self.scope_depth);
    }

    /// Each compiled argument is gonna be on top of the stack.
    /// Returns the number of arguments parsed
    fn argument_list(self: *Self) Allocator.Error!u8 {
        var args_count: u8 = 0;

        if (!self.parser.check(.RightParen)) {
            while (true) {
                try self.expression();

                if (args_count == 255) {
                    self.parser.error_at_current("can't have more than 255 arguments");
                }

                args_count += 1;

                if (!self.parser.match(.Comma)) break;
            }
        }

        self.parser.consume(.RightParen, "expect ')' after arguments");
        return args_count;
    }

    fn declaration(self: *Self) Allocator.Error!void {
        self.parser.skip_new_lines();

        if (self.parser.match(.Var)) {
            try self.var_declaration();
        } else if (self.parser.match(.Fn)) {
            try self.fn_declaration();
        } else if (self.parser.match(.Struct)) {
            try self.struct_declaration();
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

    fn fn_declaration(self: *Self) Allocator.Error!void {
        const global_id = try self.parse_variable("expect function name");
        // Mark initialized to be able to refer to itself in its body
        self.mark_initialized();
        try self.parse_function(.Fn);
        // Will bind to the object fn on top of stack created by function()
        try self.define_variable(global_id);
    }

    fn parse_function(self: *Self, kind: FnKind) Allocator.Error!void {
        var compiler = Compiler.new();
        try compiler.init(self.vm, self.parser, kind, self);

        compiler.begin_scope();

        compiler.parser.consume_skip(.LeftParen, "expect '(' after function name");

        if (!self.parser.check(.RightParen)) {
            while (true) {
                compiler.function.arity += 1;

                if (compiler.function.arity > 255) {
                    self.parser.error_at_current("can't have more than 255 parameters");
                }

                const constant_id = try compiler.parse_variable("expect parameter name");
                try compiler.define_variable(constant_id);

                if (!self.parser.match(.Comma)) break;
            }
        }

        self.parser.consume_skip(.RightParen, "expect ')' after parameters");
        self.parser.consume_skip(.LeftBrace, "expect '{' before function body");

        try compiler.block();
        const obj_fn = try compiler.end_compiler();
        try self.emit_bytes_u8(.Closure, try self.make_constant(Value.obj(obj_fn.as_obj())));

        for (0..obj_fn.upvalue_count) |i| {
            try self.emit_byte_u8(if (compiler.upvalues[i].is_local) 1 else 0);
            try self.emit_byte_u8(compiler.upvalues[i].index);
        }
    }

    fn struct_declaration(self: *Self) Allocator.Error!void {
        self.parser.consume(.Identifier, "expect structure name");
        const struct_name = &self.parser.previous;
        const name_id = try self.identifier_constant(&self.parser.previous);
        self.declare_variable();

        try self.emit_bytes_u8(.Struct, name_id);
        // Define before body, the we can refer to the class own method's body
        try self.define_variable(name_id);

        // Allow to put the structure back on top of stack. It could be a local
        // or a global, the function handles that
        try self.named_variable(struct_name, false);

        self.parser.consume_skip(.LeftBrace, "expect '{' before structure body");

        if (self.parser.check(.Fn)) {
            while (!self.parser.check(.RightBrace) and !self.parser.check(.Eof)) {
                self.parser.consume(.Fn, "expect 'fn' keyword to declare methods");
                try self.method();
                self.parser.skip_new_lines();
            }
        }

        self.parser.consume(.RightBrace, "expect '}' after structure body");

        // We pop the structure that was put by the `named variable` method
        try self.emit_byte(.Pop);
    }

    fn method(self: *Self) Allocator.Error!void {
        self.parser.consume(.Identifier, "expect method name");
        const method_id = try self.identifier_constant(&self.parser.previous);

        var fn_kind: FnKind = .Method;

        if (std.mem.eql(u8, self.parser.previous.lexeme, "init")) {
            fn_kind = .Initializer;
        }

        // Leaves the closure on top of the stack at runtime
        try self.parse_function(fn_kind);

        try self.emit_bytes_u8(.Method, method_id);
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
        } else if (self.parser.match(.For)) {
            try self.for_statement();
        } else if (self.parser.match(.While)) {
            try self.while_statement();
        } else if (self.parser.match(.Return)) {
            try self.return_statement();
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

        if (!self.parser.check(.LeftBrace)) {
            self.parser.error_at_current("expect '{' after 'if' condition");
            return;
        }

        const then_jump = try self.emit_jump(.JumpIfFalse);
        // Pop the condition inside then branch if the condition is true
        try self.emit_byte(.Pop);
        try self.statement();

        const else_jump = try self.emit_jump(.Jump);
        self.patch_jump(then_jump, 0);

        // Pop the condition inside else branch if the condition is false
        try self.emit_byte(.Pop);

        if (self.parser.match(.Else)) try self.statement();
        self.patch_jump(else_jump, 0);
    }

    fn while_statement(self: *Self) Allocator.Error!void {
        const loop_start = self.get_chunk().code.items.len;
        try self.expression();

        if (!self.parser.check(.LeftBrace)) {
            self.parser.error_at_current("expect '{' after 'while' condition");
            return;
        }

        const exit_jump = try self.emit_jump(.JumpIfFalse);
        try self.emit_byte(.Pop);
        try self.statement();
        try self.emit_loop(loop_start);

        self.patch_jump(exit_jump, 0);
        try self.emit_byte(.Pop);
    }

    fn for_statement(self: *Self) Allocator.Error!void {
        self.begin_scope();
        self.parser.consume(.Identifier, "expect variable name");

        self.declare_variable();
        // Id is not relevant, only used for global var
        try self.define_variable(0);

        self.parser.consume(.In, "expect 'in' after variable name");
        self.parser.consume(.Int, "expect 'int' value to iterate on");
        try self.int(false);

        try self.emit_byte(.CreateIter);
        // Placeholder var to keep locals and stack aligned
        self.add_local("__iter");
        // Important for the depth for clean scope exit
        self.mark_initialized();
        const iter_idx = self.local_count - 1;

        const loop_start = self.get_chunk().code.items.len;
        // Emits the 16bits jump operand
        const exit_jump = try self.emit_jump(.ForIter);
        // Index of the iterator in the stack
        try self.emit_byte_u8(@intCast(iter_idx));

        // TODO: we enter again a new scope here, making possible to
        // redeclare the same var as the placeholder
        try self.statement();
        try self.emit_loop(loop_start);

        // We use the speciall offset to jump 1 more back because patch jump
        // internally jumps 2 backward to jump over the jump OpCode itself
        // (composed of two parts). Here, we also need to jump over the iterator
        // index on the stack
        self.patch_jump(exit_jump, 1);

        try self.end_scope();
    }

    fn return_statement(self: *Self) Allocator.Error!void {
        if (self.kind == .Script) {
            self.parser.err("can't return from top-level code");
        }

        if (self.parser.match(.NewLine)) {
            try self.emit_return();
        } else {
            if (self.kind == .Initializer) {
                self.parser.err("can't return a value from an initializer");
            }

            try self.expression();

            // For the inline syntaxe: if true { return 0 }
            if (!self.parser.check(.RightBrace)) {
                self.parser.consume(.NewLine, "expect nothing after return value");
            }

            try self.emit_byte(.Return);
        }
    }

    fn and_(self: *Self, _: bool) Allocator.Error!void {
        // At runtime, left hand side of 'and' is already executed
        // and value is on top. It means that if it's false, we
        // skip the entire right hand side.
        const end_jump = try self.emit_jump(.JumpIfFalse);
        // Otherwise, we discard the right hand side 'true' and
        // evaluate the rhs which become the result of the 'and'
        try self.emit_byte(.Pop);
        try self.parse_precedence(.And);
        self.patch_jump(end_jump, 0);
    }

    // PERF: create a specific instruction to jump if true
    // there is no reason for 'or' to be slower than 'and'
    fn or_(self: *Self, _: bool) Allocator.Error!void {
        // Jump just the unconditional jump if false to
        // evaluate the second part of 'or' in case the
        // first one is false
        const else_jump = try self.emit_jump(.JumpIfFalse);
        // If the first is true, we haven't jump and we can
        // now jump to the next part
        const end_jump = try self.emit_jump(.Jump);
        self.patch_jump(else_jump, 0);
        try self.emit_byte(.Pop);
        try self.parse_precedence(.Or);
        self.patch_jump(end_jump, 0);
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

    fn call(self: *Self, _: bool) Allocator.Error!void {
        const args_count = try self.argument_list();
        try self.emit_bytes_u8(.Call, args_count);
    }

    fn dot(self: *Self, can_assign: bool) Allocator.Error!void {
        self.parser.consume(.Identifier, "expect property name after '.'");
        const name_id = try self.identifier_constant(&self.parser.previous);

        if (can_assign and self.parser.match(.Equal)) {
            try self.expression();
            try self.emit_bytes_u8(.SetProperty, name_id);
        } else if (self.parser.match(.LeftParen)) {
            const arg_count = try self.argument_list();
            try self.emit_bytes_u8(.Invoke, name_id);
            try self.emit_byte_u8(arg_count);
        } else {
            try self.emit_bytes_u8(.GetProperty, name_id);
        }
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

    // At this point, 'self' has just been parsed
    // Calls variable that handles the local machinery for us
    fn self_(self: *Self, _: bool) Allocator.Error!void {
        if (!self.in_struct) {
            self.parser.err("can't use 'self' outside of structure");
            return;
        }
        try self.variable(false);
    }

    /// Creates a local or global variable and a Set/Get OpCode
    fn named_variable(self: *Self, name: *const Token, can_assign: bool) Allocator.Error!void {
        var var_id = self.resolve_local(name);
        // NOTE: initialize with local and just (if (.. == -1)) ?
        var set_op: OpCode = undefined;
        var get_op: OpCode = undefined;

        if (var_id != -1) {
            set_op = .SetLocal;
            get_op = .GetLocal;
        } else {
            var_id = self.resolve_upvalue(name);

            if (var_id != -1) {
                set_op = .SetUpvalue;
                get_op = .GetUpvalue;
            } else {
                var_id = try self.identifier_constant(name);
                set_op = .SetGlobal;
                get_op = .GetGlobal;
            }
        }

        if (can_assign and self.parser.match(.Equal)) {
            try self.expression();
            try self.emit_bytes_u8(set_op, @intCast(var_id));
        } else {
            try self.emit_bytes_u8(get_op, @intCast(var_id));
        }
    }

    fn emit_byte(self: *Self, op: OpCode) Allocator.Error!void {
        try self.get_chunk().write_op(op, @truncate(self.parser.previous.line));
    }

    fn emit_byte_u8(self: *Self, byte: u8) Allocator.Error!void {
        try self.get_chunk().write_byte(byte, @truncate(self.parser.previous.line));
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

        return self.get_chunk().code.items.len - 2;
    }

    /// Patches the jump.
    /// The `special` argument is to add an additionnal offset when needed (
    /// for example when patching a `.ForIter`
    fn patch_jump(self: *Self, offset: usize, special: usize) void {
        // -2 is the jump offset itself
        const chunk = self.get_chunk();
        const jump: u16 = @intCast(chunk.code.items.len - offset - 2 - special);

        // FIXME: it's already casted in u16, check is useless. Check before cast?
        if (jump > std.math.maxInt(u16)) {
            self.parser.err("too much code to jump over");
        }

        chunk.code.items[offset] = @as(u8, @intCast(jump >> 8)) & 0xff;
        chunk.code.items[offset + 1] = @intCast(jump & 0xff);
    }

    fn emit_loop(self: *Self, start: usize) Allocator.Error!void {
        try self.emit_byte(.Loop);
        // +2 for loop operand 16bits
        const offset = self.get_chunk().code.items.len - start + 2;

        if (offset > std.math.maxInt(u16)) {
            self.parser.err("loop body too large");
        }

        try self.emit_byte_u8(@as(u8, @intCast(offset >> 8)) & 0xff);
        try self.emit_byte_u8(@intCast(offset & 0xff));
    }

    fn get_chunk(self: *const Self) *Chunk {
        return &self.function.chunk;
    }

    fn end_compiler(self: *Self) Allocator.Error!*ObjFunction {
        try self.emit_return();

        if (config.PRINT_CODE) {
            if (!self.parser.had_error) {
                const Disassembler = @import("disassembler.zig").Disassembler;
                const dis = Disassembler.init(self.get_chunk());

                const obj = self.function.name;
                const name = if (obj) |o| o.chars else "<script>";

                try dis.dis_chunk(name);

                // If we compiled the outter compiler (no name), we put space
                if (obj == null) {
                    print("\n\n", .{});
                }
            }
        }

        return self.function;
    }

    /// Called when there is only an implicit return at the end of a
    /// function body. Pushes a null on top of the stack. For real
    /// returns, there is a return_statement handler
    fn emit_return(self: *Self) Allocator.Error!void {
        // If we are in 'init' method, we return self, which is at slot 0
        if (self.kind == .Initializer) {
            try self.emit_bytes_u8(.GetLocal, 0);
        } else {
            try self.emit_byte(.Null);
        }

        try self.emit_byte(.Return);
    }

    fn emit_constant(self: *Self, value: Value) Allocator.Error!void {
        try self.emit_bytes_u8(.Constant, try self.make_constant(value));
    }

    /// Writes a constant to the chunk. Returns its index
    fn make_constant(self: *Self, value: Value) Allocator.Error!u8 {
        const constant = try self.get_chunk().write_constant(value);

        // NOTE: for now, unnecessary check because it writes to an ArrayList
        if (constant > std.math.maxInt(u8)) {
            self.parser.err("too many constants in one chunk");
            return 0;
        }

        return @truncate(constant);
    }
};
