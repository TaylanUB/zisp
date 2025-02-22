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
//   string -> foo , "foo bar"   ;symbols and strings are the same data type
//
//   rune   -> #foo              ;limited to 6 ASCII letters (a - z, A - Z)
//
//   pair   -> (DATUM . DATUM)   ;the only composite data type supported
//
//   nil    -> ()                ;we prefer the term nil over null
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
// differentiate between the code expressions #'foo and #"foo".)
//
// Runes are case-sensitive, and the code parser only emits runes using
// upper-case letters, so lower-case runes are free for user extensions.
//
// You may be wondering about numbers.  As far as the parser is concerned,
// numbers are strings.  It's the decoder (see below) that will turn bare
// strings (those not marked with #STRING) into numbers.
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
// The decoder may also perform arbitrary transforms on any type; for example,
// it may turn bare strings (those not marked with #STRING) into numbers when
// it's decoding data representing code.  This is how number literals are
// implemented in Zisp.
//
// The decoder recognizes (#QUOTE ...) to implement the traditional quoting
// mechanism, but in a better way:
//
// Traditional quote is "unhygienic" in Scheme terms.  An expression such as
// '(foo bar) will always be read as (quote (foo bar)) regardless of what sort
// of lexical context it appears in, so the semantics will depend on whatever
// the identifier "quote" is bound to in that lexical context, meaning that the
// expression may end up evaluating to something other than the list (foo bar).
//
// The Zisp decoder, which transforms not text to text, but objects to objects,
// can turn (#QUOTE ...) into an abstract object which encapsulates the notion
// of quoting, which the Zisp evaluator can recognize and act upon, ensuring
// that an expression like '(foo bar) always turns into the list (foo bar).
//
// One way to think about this, in Scheme (R6RS / syntax-case) terms, is that
// expressions like '(foo bar) turn directly into a *syntax object* when read,
// rather than a regular list object.
//
// The decoder is, of course, configurable and extensible.  The transformations
// mentioned above would be performed only when it's told to decode data which
// represents Zisp code.  The decoder may be given a different configuration,
// telling it to decode, for example, data which represents a different kind of
// domain-specific data, such as application settings, build system commands,
// complex data records with non-standard data types, and so on.
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
// Note 3: When it comes to pairs/lists, we mainly try to parse them as lists,
// and a pair becomes a special-case of an improper list (two elements).  This
// has the advantage of saving memory: If we implemented list parsing as pair
// parsing, we would be calling the parser recursively, deeper and deeper, for
// every pair that the list is made up of.  Although we're not limited by stack
// space, thanks to the strategy described above, this would still waste memory
// while parsing.
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

const gc = @import("../gc.zig");
const list = @import("../list.zig");
const value = @import("../value.zig");

const Value = value.Value;

const State = struct {
    alloc: std.mem.Allocator,

    input: []const u8,
    pos: usize = 0,

    mode: enum { code, data } = .code,

    next: Fn = .start_parse,

    parent: ?*State = null,

    // Used to store various context, but most notably the stack of list
    // elements parsed so far, so just initialize it to nil.
    context: Value = value.nil.nil,

    opening_bracket: u8 = 0,

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
    finalize_improper_list,
    end_improper_list,
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
        .finalize_improper_list => finalizeImproperList(s),
        .end_improper_list => endImproperList(s),
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
        0...32, 127...255 => err(s, "invalid character"),
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
        0...32, 127...255 => err(s, "invalid character"),

        ')', ']', '}' => err(s, "unexpected closing bracket"),

        ';' => err(s, "expected datum, got semicolon"),

        '#' => handleHash(s),

        '"' => startQuotedString(s),

        '\'', '`', ',' => startQuote(s),

        '(', '[', '{' => startList(s),

        // Periods are only allowed in the middle of a string, or to express
        // improper lists, because the following look too much like typos:
        //
        //   (foo. bar)  (foo .bar)  (123. 456)  (123 .456)
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
        'a'...'z', 'A'...'Z' => return handleRune(s),
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

    s.context = rune;
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
    return s.returnDatum(value.pair.cons(s.context, s.retval));
}

fn endHashDatum(s: *State) *State {
    const rune = value.rune.pack("HASH");
    return s.returnDatum(value.pair.cons(rune, s.retval));
}

fn startQuotedString(s: *State) *State {
    // We're at |"..." so consume the opening quote before we start reading.
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
    // We're at |foo so start reading directly.
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
    // We're at one of:  |'...  |`...  |,...
    s.context = value.rune.pack(switch (s.getc()) {
        '\'' => "QUOTE",
        '`' => "GRAVE",
        ',' => "COMMA",
        else => unreachable,
    });
    return s.recurParse(.start_datum, .end_quote);
}

fn endQuote(s: *State) *State {
    return s.returnDatum(value.pair.cons(s.context, s.retval));
}

// List processing is, unsurprisingly, the most complicated, and it's made even
// more complicated by the possibility of datum comments in strange places...

fn startList(s: *State) *State {
    const open = s.getc();

    if (s.mode == .data and open != '(') {
        return err(s, "invalid opening bracket in data mode");
    }

    s.consumeBlanks();
    if (s.eof()) {
        return err(s, "unexpected EOF while parsing list");
    }

    s.opening_bracket = open;
    return if (isEndOfList(s))
        endList(s)
    else
        s.recurParse(.start_parse, .continue_list);
}

fn isEndOfList(s: *State) bool {
    return switch (s.peek()) {
        ')', ']', '}' => true,
        else => false,
    };
}

fn continueList(s: *State) *State {
    s.context = value.pair.cons(s.retval, s.context);

    s.consumeBlanks();
    if (s.eof()) {
        return err(s, "unexpected EOF while parsing list");
    }

    if (isEndOfList(s)) {
        s.context = list.reverse(s.context);
        return endList(s);
    }

    if (s.peek() == '.') {
        s.skip();
        // Scheme allows (foo .(bar)) but we don't.  Mind your spaces!
        if (!s.isWhitespace()) {
            return err(s, "misplaced period");
        }
        return s.recurParse(.start_parse, .finalize_improper_list);
    }

    return s.recurParse(.start_parse, .continue_list);
}

fn finalizeImproperList(s: *State) *State {
    s.context = list.reverseWithTail(s.context, s.retval);
    return endImproperList(s);
}

fn endImproperList(s: *State) *State {
    s.consumeBlanks();
    if (s.eof()) {
        return err(s, "unexpected EOF while parsing list");
    }

    if (isEndOfList(s)) {
        return endList(s);
    }

    if (s.getc() == '#') {
        if (s.eof()) {
            return err(s, "unexpected EOF after hash while parsing list");
        }
        if (s.getc() == ';') {
            return s.recurParse(.start_datum, .end_improper_list);
        }
    }

    return err(s, "malformed list / extra datum at end of improper list");
}

fn endList(s: *State) *State {
    const open = s.opening_bracket;
    const char = s.getc();

    // Check for proper ending: (foo bar baz)
    if (open == '(' and char == ')') {
        return s.returnDatum(s.context);
    }
    if (open == '[' and char == ']') {
        const rune = value.rune.pack("SQUARE");
        return s.returnDatum(value.pair.cons(rune, s.context));
    }
    if (open == '{' and char == '}') {
        const rune = value.rune.pack("BRACE");
        return s.returnDatum(value.pair.cons(rune, s.context));
    }

    return err(s, "wrong closing bracket for list");
}

fn err(s: *State, msg: []const u8) noreturn {
    std.debug.print("{s}\n", .{msg});
    std.debug.print("pos: {}\n", .{s.pos});
    @panic("parse error");
}
