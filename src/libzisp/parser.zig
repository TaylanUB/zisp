//
// === Parser for Code & Data ===
//
// Zisp s-expressions come in two flavors: code (sugar) and data (no sugar).
//
// However, code expressions are parsed into the same data types which the data
// expressions can represent, so homoiconicity is preserved.
//
// The "sugar" used in code expressions is merely shorthand for more complex
// data expressions, which could have been written by hand.
//
// Data expressions have a very simple format, and are only able to express a
// minimal set of data types:
//
//   string -> foo , "foo bar"     ;symbols and strings are the same data type
//
//   number -> -1.2 , +nan.0 , ... ;this is the most complex type to represent
//
//   rune   -> #foo                ;limited to 6 ASCII letters (a - z, A - Z)
//
//   pair   -> (DATUM . DATUM)     ;the only composite data type supported
//
//   nil    -> ()                  ;we prefer the term nil over null
//
// The list short-hand syntax may be considered the only "syntax sugar" that is
// supported by the data parser:
//
//   (DATUM DATUM DATUM)  ->  (DATUM . (DATUM . (DATUM . ())))
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
// These can combine arbitrarily:
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
//   "..."    -> (#STRING . "...")
//
// (Otherwise, all string literals would be identifiers, or all identifiers
// would be string literals, because Zisp doesn't differentiate strings and
// symbols like traditional lisps.  Also, note that although we could reuse
// #QUOTE here, instead of using #STRING, this would make it impossible to
// differentiate between the expressions #'foo and #"foo" once parsing is
// finished, even though we may want to give them different meanings.)
//
// Runes are case-sensitive, and the code parser only emits runes using
// upper-case letters, so lower-case runes are free for user extensions.
//
// Note that 'foo becomes (quote foo) in Scheme, but (#QUOTE . foo) in Zisp.
// The operand of #QUOTE is the entire cdr.  The same principle is used when
// parsing other sugar:
//
//          Incorrect                              Correct
//
//   #(x y z) -> (#HASH (x y z))            #(x y z) -> (#HASH x y z)
//
//   [x y z]  -> (#SQUARE (x y z))          [x y z]  -> (#SQUARE x y z)
//
//   #{x}     -> (#HASH (#BRACE (x)))       #{x}     -> (#HASH #BRACE x)
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
// The decoder recognizes (#QUOTE ...) to implement the traditional quoting
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
// main loop which calls a function, passing it the state, and also expects a
// state pointer in return.
//
// The state has a .next field, which indicates which function the main loop
// should call next.  It also has a .parent field, used as follows:
//
// If a function wants to call the parser recursively, it sets the .next field
// of the current state to where the recursive call should return to, then it
// creates a new state with a given starting point, sets its .parent field to
// the current state, and returns the new state pointer.
//
// If a function wants to make the parser return, it sets the .retval field of
// the current state, and sets .next to the .perform_return value.  This makes
// the main loop either return to the parent state (after copying the .retval
// field from child to parent), or if there is no parent state, it returns the
// value as the top-level result.
//
// Note: While it's possible to simply set .next and return the current state,
// to have another function be called next (possibly even setting .retval to
// pass a value to it), this is completely unnecessary.  A few non-recursive
// function calls will obviously not blow the stack.  It's only recursive
// parsing that we use these fields for.
//
// Note 2: When calling the parser recursively, it may seem sensible to always
// set the .next of the new state to .start_datum, because you already cleared
// incoming whitespace and comments from the stream.  However, in some cases,
// you must set it to .start_parse instead.  This is due to datum comments.
// After a datum comment is parsed, the parser will ignore it and restore the
// previous state, to try again what it was doing.  If the state was set to
// .start_datum, this means no whitespace or comments would be tolerated after
// the datum comment.
//
//
// === Notation used in comments ===
//
// Some comments throughout the file give you an example of where the parser
// currently might be in a stream.  These illustrations use the pipe symbol,
// which looks like a cursor, to indicate the current position:
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

    next: Fn = .start_parse,

    parent: ?*State = null,

    datum_rune: Value = value.boole.f,
    list_stack: Value = value.nil.nil,
    opening_bracket: u8 = 0,
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

    fn recurParse(self: *State, start_from: Fn, return_to: Fn) *State {
        const sub = self.alloc.create(State) catch @panic("OOM");
        sub.* = .{ .alloc = self.alloc, .input = self.input };
        sub.pos = self.pos;
        sub.mode = self.mode;
        sub.next = start_from;
        sub.parent = self;
        self.next = return_to;
        return sub;
    }

    fn returnDatum(self: *State, val: Value) *State {
        self.retval = val;
        self.next = .perform_return;
        return self;
    }

    fn performReturn(self: *State) ?*State {
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

const Fn = enum {
    start_parse,
    start_datum,
    end_hash_datum,
    end_rune_datum,
    end_quote,
    continue_list,
    end_improper_list,
    handle_trailing_datum_comments,
    perform_return,
};

pub fn parse(input: []const u8) Value {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    var top = State{ .alloc = gpa.allocator(), .input = input };
    var s = &top;
    while (true) s = switch (s.next) {
        .start_parse => startParse(s),
        .start_datum => startDatum(s),
        .end_hash_datum => endHashDatum(s),
        .end_rune_datum => endRuneDatum(s),
        .end_quote => endQuote(s),
        .continue_list => continueList(s),
        .end_improper_list => endImproperList(s),
        .handle_trailing_datum_comments => handleTrailingDatumComments(s),
        .perform_return => s.performReturn() orelse return s.retval,
    };
}

fn startParse(s: *State) *State {
    s.consumeBlanks();
    if (s.eof()) {
        return s.returnDatum(value.eof.eof);
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
    if (s.eof()) {
        return err(s, "expected datum, got EOF");
    }
    if (s.isWhitespace()) {
        return err(s, "expected datum, got whitespace");
    }
    return switch (s.peek()) {
        // whitespace checked above
        0...31, 127...255 => err(s, "invalid character"),

        ')', ']', '}' => err(s, "unexpected closing bracket"),

        ';' => err(s, "expected datum, got semicolon"),

        '#' => handleHash(s),

        '"' => startQuotedString(s),

        '\'', '`', ',' => startQuote(s),

        '(', '[', '{' => startList(s),

        '+', '-' => handlePlusMinus(s),

        '0'...'9' => handleDigit(s),

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
    s.skip();
    //
    // We just consumed a hash.  Possibilities include:
    //
    //   #|foo       ;rune
    //
    //   #|;DATUM    ;datum comment
    //
    //   #|DATUM     ;hash-datum (code mode only)
    //

    if (s.eof()) {
        return err(s, "EOF after hash");
    }
    if (s.isWhitespace()) {
        return err(s, "whitespace after hash");
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
        return s.recurParse(.start_datum, s.next);
    }

    // Otherwise, it must be a hash-datum.  #DATUM

    // But data mode doesn't allow that.
    if (s.mode == .data) {
        return err(s, "use of hash-datum sequence not allowed in data mode");
    }

    return s.recurParse(.start_datum, .end_hash_datum);
}

fn handleRune(s: *State) *State {
    const rune = readRune(s) orelse return err(s, "rune too long");

    //
    // Now we're at the end of the rune, but it could be a rune-datum:
    //
    //   #foo|(...)
    //

    if (isEndOfRune(s)) {
        // Nope, just a stand-alone rune.
        return s.returnDatum(rune);
    }

    // Otherwise, it's followed by a datum, like: #foo(...)

    // Which is only allowed in code mode.
    if (s.mode == .data) {
        return err(s, "invalid use of hash in data mode");
    }

    s.datum_rune = rune;
    return s.recurParse(.start_datum, .end_rune_datum);
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

fn isEndOfRune(s: *State) bool {
    return s.eof() or switch (s.peek()) {
        '\t', '\n', ' ', ')', ']', '}' => true,
        else => false,
    };
}

fn endRuneDatum(s: *State) *State {
    return s.returnDatum(value.pair.cons(
        s.datum_rune,
        s.retval,
    ));
}

fn endHashDatum(s: *State) *State {
    return s.returnDatum(value.pair.cons(
        value.rune.pack("HASH"),
        s.retval,
    ));
}

fn startQuotedString(s: *State) *State {
    // Consume opening quote.
    s.skip();

    const str = readQuotedString(s) catch return err(s, "unclosed string");
    if (s.mode == .code) {
        // "foo bar" => (#STRING . "foo bar")
        const rune = value.rune.pack("STRING");
        const pair = value.pair.cons(rune, str);
        return s.returnDatum(pair);
    } else {
        return s.returnDatum(str);
    }
}

// RQS = Read Quoted String
const RqsError = enum { Unclosed };

fn readQuotedString(s: *State) !Value {
    return try readQuotedSstr(s) orelse readQuotedLongString(s);
}

fn readQuotedSstr(s: *State) !?Value {
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
    return error.Unclosed;
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
    }

    return s.returnDatum(value.sstr.pack(buf[0..i]));
}

fn isBareStringEnd(s: *State) bool {
    // We will ignore illegal characters here, because they aren't consumed by
    // this function; whatever code comes next must handle them.
    return s.eof() or switch (s.peek()) {
        0...32, 127...255 => true,
        '(', ')', '[', ']', '{', '}', ';', '#', '"', '\'', '`', ',' => true,
        else => false,
    };
}

fn readBareLongString(s: *State) *State {
    return err(s, "NOT YET IMPLEMENTED");
}

fn startQuote(s: *State) *State {
    s.opening_quote = switch (s.getc()) {
        '\'' => .quote,
        '`' => .grave,
        ',' => .comma,
        else => unreachable,
    };
    return s.recurParse(.start_datum, .end_quote);
}

fn endQuote(s: *State) *State {
    const name = switch (s.opening_quote) {
        .quote => "QUOTE",
        .grave => "GRAVE",
        .comma => "COMMA",
    };
    return s.returnDatum(value.pair.cons(
        value.rune.pack(name),
        s.retval,
    ));
}

fn startList(s: *State) *State {
    const open = s.getc();

    if (s.mode == .data and open != '(') {
        return err(s, "invalid opening bracket in data mode");
    }

    s.consumeBlanks();

    if (s.eof()) {
        return err(s, "unexpected EOF while parsing list");
    }

    const char = s.peek();

    // Check for empty lists: (), [], {}
    if (open == '(' and char == ')') {
        s.skip();
        return s.returnDatum(value.nil.nil);
    }
    if (open == '[' and char == ']') {
        s.skip();
        return s.returnDatum(value.pair.cons(
            value.rune.pack("SQUARE"),
            value.nil.nil,
        ));
    }
    if (open == '{' and char == '}') {
        s.skip();
        return s.returnDatum(value.pair.cons(
            value.rune.pack("BRACE"),
            value.nil.nil,
        ));
    }

    s.opening_bracket = open;

    // Now we're facing the first element of the list.
    return s.recurParse(.start_parse, .continue_list);
}

fn continueList(s: *State) *State {
    //
    // We now consumed a list element and are at its end.  Possibilities:
    //
    //   (... foo| )
    //
    //   (... foo| . bar)
    //
    //   (... foo| bar baz ...)
    //
    // (This may be the first element, or any other; doesn't matter.)
    //

    s.consumeBlanks();

    if (s.eof()) {
        return err(s, "unexpected EOF while parsing list");
    }

    const stack = value.pair.cons(s.retval, s.list_stack);

    const open = s.opening_bracket;
    const char = s.peek();

    // Check for proper ending: (foo bar baz)
    if (open == '(' and char == ')') {
        s.skip();
        return s.returnDatum(list.reverse(stack));
    }
    if (open == '[' and char == ']') {
        s.skip();
        return s.returnDatum(value.pair.cons(
            value.rune.pack("SQUARE"),
            list.reverse(stack),
        ));
    }
    if (open == '{' and char == '}') {
        s.skip();
        return s.returnDatum(value.pair.cons(
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
            return err(s, "misplaced period");
        }

        return s.recurParse(.start_parse, .end_improper_list);
    }

    // OK, we're at the next element now and the list is going on.
    return s.recurParse(.start_parse, .continue_list);
}

fn endImproperList(s: *State) *State {
    //
    // We're at the very end of an improper list now:
    //
    //   (... foo . bar| )
    //
    // There's another sneaky possibility though: trailing datum comments!
    //
    //   (... foo . bar #;DATUM ... )
    //
    // These were being handled "automatically" while parsing regular list
    // elements, but here we have to special-handle them; see below.
    //

    s.consumeBlanks();

    if (s.eof()) {
        return err(s, "unexpected EOF while parsing list");
    }

    const result = list.reverseWithTail(s.list_stack, s.retval);

    const char = s.getc();
    const open = s.opening_bracket;

    if (open == '(' and char == ')') {
        return s.returnDatum(result);
    }
    if (open == '[' and char == ']') {
        const rune = value.rune.pack("SQUARE");
        return s.returnDatum(value.pair.cons(rune, result));
    }
    if (open == '{' and char == '}') {
        const rune = value.rune.pack("BRACE");
        return s.returnDatum(value.pair.cons(rune, result));
    }

    // Handle trailing datum comments, but don't allow anything else!

    if (char == '#' and s.peek() == ';') {
        s.skip();

        // We will "abuse" the list_stack field to save the result.
        s.list_stack = result;
        return s.recurParse(.start_datum, .handle_trailing_datum_comments);
    }

    return err(s, "malformed list / extra datum at end of improper list");
}

fn handleTrailingDatumComments(s: *State) *State {
    s.consumeBlanks();

    if (s.eof()) {
        return err(s, "unexpected EOF while parsing list");
    }

    const result = s.list_stack;

    const char = s.getc();
    const open = s.opening_bracket;

    if (open == '(' and char == ')') {
        return s.returnDatum(result);
    }
    if (open == '[' and char == ']') {
        const rune = value.rune.pack("SQUARE");
        return s.returnDatum(value.pair.cons(rune, result));
    }
    if (open == '{' and char == '}') {
        const rune = value.rune.pack("BRACE");
        return s.returnDatum(value.pair.cons(rune, result));
    }

    // Handle trailing datum comments, but don't allow anything else!

    if (char == '#' and s.peek() == ';') {
        s.skip();

        // We will "abuse" the list_stack field to save the result.
        s.list_stack = result;
        return s.recurParse(.start_datum, .handle_trailing_datum_comments);
    }

    return err(s, "malformed list / extra datum at end of improper list");
}

fn handlePlusMinus(s: *State) *State {
    return s;
}

fn handleDigit(s: *State) *State {
    return s;
}

fn err(s: *State, msg: []const u8) noreturn {
    std.debug.print("{s}\n", .{msg});
    std.debug.print("pos: {}\n", .{s.pos});
    @panic("parse error");
}
