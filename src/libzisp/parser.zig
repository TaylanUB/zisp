//
// === Parser for Code & Data ===
//
// Zisp s-expressions come in two flavors: code (sugar) and data (no sugar).
//
// Code expressions are first parsed into the same data types which the data
// expressions can express; it's merely surface-level syntax sugar.
//
// Data expressions don't support any syntax sugar and have a simple format,
// only being able to represent the following data types:
//
//   string -> foo , "foo bar"
//
//   number -> 123 , -1.23 , +123 , +nan.0 , -inf.0 , ...
//
//   rune   -> #foo     ;limited to 6 ASCII letters (a - z, A - Z)
//
//   list   -> (...)    ;the usual: actually pairs; may be improper
//
//   null   -> ()       ;also called nil around here
//
// We may use terms like "code parser" and "data parser" out of convenience,
// although there may only be a single parser that implements both formats by
// switching between modes.
//
// When the code parser encounters syntax sugar, it always transforms it into a
// list starting with a rune, like in the following examples:
//
//   #(...)   -> (#HASH ...)
//
//   [...]    -> (#SQUARE ...)
//
//   'foo     -> (#QUOTE . foo)
//
// These can combine:
//
//   #{...}   -> (#HASH #BRACE ...)
//
//   #'foo    -> (#HASH #QUOTE . foo)
//
//   ##'[...] -> (#HASH #HASH #QUOTE #SQUARE ...)
//
// As a specialty, double-quoted strings are actually considered sugar by the
// code parser, and are transformed as follows into data:
//
//   "..."    -> (#QUOTE "...")
//
// (Otherwise, all string literals would be identifiers, or all identifiers
// would be string literals, because Zisp doesn't differentiate strings and
// symbols like traditional lisps.)
//
// Runes are case-sensitive, and the code parser emits runes using only capital
// letters so as to leave lowercase runes free for user extensions.
//
//
// === Decoder ===
//
// A separate process called "decoding" can transform simple data structures,
// consisting of only the above types, into a richer set of Zisp data types.
//
// For example, the decoder may turn (#HASH ...) into a vector, as one would
// expect a vector literal like #(...) to work in Scheme.
//
// Runes may be decoded in isolation as well, rather than transforming a list
// whose head they appear in.  This is how #true and #false are implemented.
//
// The decoder interprets (#QUOTE ...) to implement the traditional quoting
// mechanism, but in a better way:
//
// Traditional quote is "unhygienic" in Scheme terms.  An expression such as
// '(foo bar) will always be read as (quote (foo bar)) regardless of what sort
// of lexical context it appears in, so the semantics will depend on whatever
// the identifier "quote" is bound to in that lexical context.
//
// The Zisp decoder, which transforms not text to text, but objects to objects,
// can turn (#QUOTE ...) into an abstract object which encapsulates the notion
// of quoting, which the Zisp evaluator can recognize and act upon.
//
// One way to think about this, in Scheme (R6RS / syntax-case) terms, is that
// expressions like '(foo bar) turn directly into a *syntax object* when read,
// rather than a regular list object.
//
//
// === Implementation details ===
//
// Instead of using recursion directly, the parser is written in something akin
// to a manual continuation-passing style, which ensures that parsing depth is
// not limited by the stack size.
//
// All state/context is passed around via a struct pointer.  The parser has a
// main loop which calls a function, passes it the state, and expects to get a
// new state pointer in return, which tells which function the main loop should
// call next, based on the .next field of the state.
//
// When a called function wants to call the parser recursively, it sets the
// .next field to an enumeration value that indicates where the parser should
// return to after it's done with the sub-parsing, and then constructs a new
// state struct, saving a pointer to the original in a .parent field.
//
// Making the parser "return" is a matter of setting the .retval field, and
// setting the .next field to the value .finish, to indicate to the main loop
// that it should either pass control back to the parent, or finish parsing.
//
//
// === Notation used in comments ===
//
// Some comments throughout the file give you an example of where the parser
// currently is in a stream.  These illustrations use the pipe symbol, which
// looks like a cursor, to indicate the current position of the parser:
//
//   (foo| bar baz)   <- parser arrived at the end of the string foo
//
//   (foo bar baz)|   <- parser reached EOF (assuming no trailing spaces)
//

const std = @import("std");

const gc = @import("gc.zig");
const list = @import("list.zig");
const value = @import("value.zig");

const Value = value.Value;

const State = struct {
    alloc: std.mem.Allocator,

    input: []const u8,
    pos: usize = 0,

    mode: enum { code, data } = .code,

    next: Next = .start_parsing,

    parent: ?*State = null,

    datum_rune: Value = value.boole.f,
    list_stack: Value = value.nil.nil,
    opening_bracket: enum { paren, square, brace } = .paren,
    opening_quote: enum { quote, grave, comma } = .quote,

    retval: Value = value.eof.eof,

    fn eof(self: *State) bool {
        return self.pos >= self.input.len;
    }

    fn peek(self: *State) u8 {
        return self.input[self.pos];
    }

    fn skip(self: *State) void {
        self.pos += 1;
    }

    fn getc(self: *State) u8 {
        const c = self.peek();
        self.skip();
        return c;
    }

    // Consumes whitespace and line comments.
    fn consumeBlanks(self: *State) void {
        while (!self.eof()) {
            if (self.isWhitespace()) {
                self.skip();
            } else if (self.peek() == ';') {
                self.skip();
                self.consumeLineComment();
            } else {
                return;
            }
        }
    }

    fn consumeLineComment(self: *State) void {
        while (!self.eof()) {
            if (self.getc() == '\n') {
                return;
            }
        }
    }

    fn isWhitespace(self: *State) bool {
        return switch (self.peek()) {
            '\t', '\n', ' ' => true,
            else => false,
        };
    }

    fn isFinalNull(self: *State) bool {
        return self.peek() == 0 and self.pos == self.input.len - 1;
    }

    fn newSubstate(self: *State, next: Next) *State {
        const sub = self.alloc.create(State) catch @panic("OOM");
        sub.* = .{ .alloc = self.alloc, .input = self.input };
        sub.pos = self.pos;
        sub.mode = self.mode;
        sub.next = next;
        sub.parent = self;
        return sub;
    }

    fn setReturn(self: *State, val: Value) *State {
        self.retval = val;
        self.next = .finish;
        return self;
    }

    fn finish(self: *State) ?*State {
        if (self.parent) |parent| {
            parent.retval = self.retval;
            parent.pos = self.pos;
            parent.alloc.destroy(self);
            return parent;
        } else {
            return null;
        }
    }
};

// Probably best *not* to use function pointers here, but rather dispatch to
// functions manually based on enum value.  This should help the optimizer.

const Next = enum {
    start_parsing,
    start_datum,
    end_hash_datum,
    end_rune_datum,
    end_quote,
    continue_list,
    end_improper_list,
    finish,
};

pub fn parse(input: []const u8) Value {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    var top = State{ .alloc = gpa.allocator(), .input = input };
    var s = &top;
    while (true) {
        s = switch (s.next) {
            .start_parsing => startParsing(s),
            .start_datum => startDatum(s),
            .end_hash_datum => endHashDatum(s),
            .end_rune_datum => endRuneDatum(s),
            .end_quote => endQuote(s),
            .continue_list => continueList(s),
            .end_improper_list => endImproperList(s),
            .finish => s.finish() orelse break,
        };
    }
    if (s.eof() or s.isFinalNull()) {
        return s.retval;
    } else {
        // Should never happen.
        err(s, "PARSER BUG: unconsumed input");
    }
}

fn startParsing(s: *State) *State {
    s.consumeBlanks();
    if (s.eof()) {
        return s.setReturn(value.eof.eof);
    }
    return switch (s.peek()) {
        // whitespace already consumed
        0...31, 127...255 => err(s, "invalid character"),
        ')', ']', '}' => err(s, "unexpected closing bracket"),
        else => startDatum(s),
    };
}

// This is called when we *immediately* expect a datum and nothing else; for
// example, no whitespace or comments, because they've already been consumed.
fn startDatum(s: *State) *State {
    if (s.isWhitespace()) {
        return err(s, "expected datum, got whitespace");
    }
    if (s.eof()) {
        return err(s, "expected datum, got EOF");
    }
    return switch (s.getc()) {
        // whitespace checked above
        0...31, 127...255 => err(s, "invalid character"),

        ')', ']', '}' => err(s, "unexpected closing bracket"),

        ';' => err(s, "expected datum, got semicolon"),

        '#' => handleHash(s),

        '"' => startQuotedString(s),

        '\'', '`', ',' => |c| startQuote(s, c),

        '(', '[', '{' => |c| startList(s, c),

        '+', '-' => |c| handlePlusMinus(s, c),

        '0'...'9' => |c| handleDigit(s, c),

        // Periods only allowed between digits, and to express improper lists.
        // Things like the following look too much like it could be a typo:
        //
        //   (foo .5) (foo .bar)
        //
        '.' => err(s, "misplaced period"),

        else => startBareString(s),
    };
}

fn handleHash(s: *State) *State {
    //
    // We just consumed a hash.  Possibilities include:
    //
    //   #|foo       ;rune
    //
    //   #|;DATUM    ;datum comment
    //
    //   #|DATUM     ;hash-datum (code mode only)
    //

    if (s.isWhitespace()) {
        return err(s, "whitespace after hash");
    }
    if (s.eof()) {
        return err(s, "EOF after hash");
    }

    // Is it a rune?  #foo
    switch (s.peek()) {
        'A'...'Z', 'a'...'z' => return handleRune(s),
        else => {},
    }

    // Is it a datum comment?  #;DATUM
    if (s.peek() == ';') {
        s.skip();
        // Don't change s.next in this case.  Just let the parser try to redo
        // what it was doing as soon as the commented-out datum has been read.
        return s.newSubstate(.start_datum);
    }

    // Otherwise, it must be a hash-datum.  #DATUM

    // But data mode doesn't allow that.
    if (s.mode == .data) {
        return err(s, "invalid use of hash in data mode");
    }

    s.next = .end_hash_datum;
    return s.newSubstate(.start_datum);
}

fn handleRune(s: *State) *State {
    const rune = readRune(s) orelse return err(s, "rune too long");

    //
    // Now we're at the end of the rune, but it could be a rune-datum:
    //
    //   #foo|(...)
    //

    if (s.eof() or switch (s.peek()) {
        '\t', '\n', ' ', ')', ']', '}' => true,
        else => false,
    }) {
        // Nope, just a stand-alone rune.
        return s.setReturn(rune);
    }

    // Otherwise, it's followed by a datum, like: #foo(...)

    // Which is only allowed in code mode.
    if (s.mode == .data) {
        return err(s, "invalid use of hash in data mode");
    }

    s.datum_rune = rune;
    s.next = .end_rune_datum;
    return s.newSubstate(.start_datum);
}

fn readRune(s: *State) ?Value {
    var buf: [6]u8 = undefined;
    var i: u8 = 0;
    while (!s.eof()) : (i += 1) switch (s.peek()) {
        'a'...'z', 'A'...'Z' => {
            if (i == buf.len) {
                return null;
            }
            buf[i] = s.getc();
        },
        else => break,
    };

    // 'i' can't be 0 since this function is only called if at least one ASCII
    // letter was seen after the hash.
    std.debug.assert(i != 0);

    return value.rune.pack(buf[0..i]);
}

fn endRuneDatum(s: *State) *State {
    return s.setReturn(value.pair.cons(
        s.datum_rune,
        s.retval,
    ));
}

fn endHashDatum(s: *State) *State {
    return s.setReturn(value.pair.cons(
        value.rune.pack("HASH"),
        s.retval,
    ));
}

fn startQuotedString(s: *State) *State {
    // We are now here:
    //
    //   "|..."
    //
    const str = readQuotedString(s) catch return err(s, "unclosed string");
    if (s.mode == .code) {
        // "foo bar" => (#QUOTE . "foo bar")
        const rune = value.rune.pack("QUOTE");
        const pair = value.pair.cons(rune, str);
        return s.setReturn(pair);
    } else {
        return s.setReturn(str);
    }
}

const StringReadError = enum { UnclosedString };

fn readQuotedString(s: *State) error{UnclosedString}!Value {
    return try readQuotedSstr(s) orelse readQuotedLongString(s);
}

fn readQuotedSstr(s: *State) error{UnclosedString}!?Value {
    // We will reset to this position if we fail.
    const start_pos = s.pos;

    var buf: [6]u8 = undefined;
    var i: u8 = 0;
    while (!s.eof()) {
        const c = s.getc();
        if (c == '"') {
            // ok, return what we accumulated
            return value.sstr.pack(buf[0..i]);
        }
        if (i == 6) {
            // failed; reset and bail out
            s.pos = start_pos;
            return null;
        }
        // ok, save this byte and go on
        buf[i] = c;
        i += 1;
    }
    return error.UnclosedString;
}

fn readQuotedLongString(s: *State) Value {
    return err(s, "NOT YET IMPLEMENTED");
}

fn startBareString(s: *State) *State {
    return readBareSstr(s) orelse readBareLongString(s);
}

fn readBareSstr(s: *State) ?*State {
    // We will reset to this position if we fail.
    const start_pos = s.pos;

    var buf: [6]u8 = undefined;
    var i: u8 = 0;
    while (!s.eof()) : (i += 1) {
        if (isBareStringEnd(s)) {
            break;
        }
        if (i == buf.len) {
            // failed; reset and bail out
            s.pos = start_pos;
            return null;
        }
        buf[i] = s.getc();
        i += 1;
    }

    return s.setReturn(value.sstr.pack(buf[0..i]));
}

fn isBareStringEnd(s: *State) bool {
    // We will ignore illegal characters here, because they aren't consumed by
    // this function; whatever code comes next must handle them.
    return s.eof() or switch (s.peek()) {
        0...31, 127...255 => true,
        '(', ')', '[', ']', '{', '}', ';', '#', '"', '\'', '`', ',' => true,
        else => false,
    };
}

fn readBareLongString(s: *State) *State {
    return err(s, "NOT YET IMPLEMENTED");
}

fn startQuote(s: *State, c: u8) *State {
    // Allowed in Scheme, but why?  Not in Zisp.
    if (s.isWhitespace()) {
        return err(s, "whitespace after apostrophe");
    }
    s.opening_quote = switch (c) {
        '\'' => .quote,
        '`' => .grave,
        ',' => .comma,
        else => unreachable,
    };
    const sub = s.newSubstate(.start_datum);
    sub.mode = .data;
    s.next = .end_quote;
    return sub;
}

fn endQuote(s: *State) *State {
    const name = switch (s.opening_quote) {
        .quote => "QUOTE",
        .grave => "GRAVE",
        .comma => "COMMA",
    };
    return s.setReturn(value.pair.cons(
        value.rune.pack(name),
        s.retval,
    ));
}

fn startList(s: *State, open: u8) *State {
    if (s.mode == .data and open != '(') {
        return err(s, "invalid opening bracket in data mode");
    }

    s.consumeBlanks();

    // Check for empty lists: (), [], {}
    if (open == '(' and s.peek() == ')') {
        s.skip();
        return s.setReturn(value.nil.nil);
    }
    if (open == '[' and s.peek() == ']') {
        s.skip();
        return s.setReturn(value.pair.cons(
            value.rune.pack("SQUARE"),
            value.nil.nil,
        ));
    }
    if (open == '{' and s.peek() == '}') {
        s.skip();
        return s.setReturn(value.pair.cons(
            value.rune.pack("BRACE"),
            value.nil.nil,
        ));
    }

    s.opening_bracket = switch (open) {
        '(' => .paren,
        '[' => .square,
        '{' => .brace,
        else => unreachable,
    };
    s.next = .continue_list;
    return s.newSubstate(.start_datum);
}

fn continueList(s: *State) *State {
    s.consumeBlanks();

    if (s.eof()) {
        return err(s, "unexpected EOF while parsing list");
    }

    const stack = value.pair.cons(s.retval, s.list_stack);

    const open = s.opening_bracket;
    const char = s.peek();

    // Check for proper ending: (foo bar baz)
    if (open == .paren and char == ')') {
        s.skip();
        return s.setReturn(list.reverse(stack));
    }
    if (open == .square and char == ']') {
        s.skip();
        return s.setReturn(value.pair.cons(
            value.rune.pack("SQUARE"),
            list.reverse(stack),
        ));
    }
    if (open == .brace and char == '}') {
        s.skip();
        return s.setReturn(value.pair.cons(
            value.rune.pack("BRACE"),
            list.reverse(stack),
        ));
    }

    s.list_stack = stack;

    // Check for improper ending: (foo bar . baz)
    if (char == '.') {
        s.skip();

        // We should now be at (... foo .| bar) and whitespace must follow.
        // Scheme allows (foo .(bar)) but we don't.  Mind your spaces!
        if (!s.isWhitespace()) {
            return err(s, "invalid use of period");
        }

        s.consumeBlanks();

        s.next = .end_improper_list;
        return s.newSubstate(.start_datum);
    }

    // OK, next element.
    return s.newSubstate(.start_datum);
}

fn endImproperList(s: *State) *State {
    s.consumeBlanks();

    if (s.eof()) {
        return err(s, "unexpected EOF");
    }

    const result = list.reverseWithTail(s.list_stack, s.retval);
    const open = s.opening_bracket;
    const char = s.getc();
    if (open == .paren and char == ')') {
        return s.setReturn(result);
    }
    if (open == .square and char == ']') {
        const rune = value.rune.pack("SQUARE");
        return s.setReturn(value.pair.cons(rune, result));
    }
    if (open == .brace and char == '}') {
        const rune = value.rune.pack("BRACE");
        return s.setReturn(value.pair.cons(rune, result));
    }

    return err(s, "malformed list or extra datum at end of improper list");
}

fn handlePlusMinus(s: *State, c: u8) *State {
    _ = c;
    return s;
}

fn handleDigit(s: *State, c: u8) *State {
    _ = c;
    return s;
}

fn err(s: *State, msg: []const u8) noreturn {
    std.debug.print("{s}\n", .{msg});
    std.debug.print("pos: {}\n", .{s.pos});
    @panic("parse error");
}
