//
// === Parser for Code & Data ===
//
// Zisp s-expressions are defined in terms of an extremely minimal set of data
// types; only that which is necessary to build representations of more complex
// expressions and types:
//
//   type      format/examples    comment
//   ----      ---------------    -------
//
//   string    foo , "foo bar"    quoted strings are flagged as such
//
//   rune      #name              name is: [a-zA-Z][a-zA-Z0-9]{0,5}
//
//   pair      (DATUM . DATUM)    the only composite data type supported
//
//   nil       ()                 we prefer the term nil over null
//
// The parser recognizes various "syntax sugar" and transforms uses of it into
// uses of the above types.
//
// The most ubiquitous example is of course the list syntax:
//
//   (datum1 datum2 ...)  ->  (datum1 . (datum2 . (... . ())))
//
// The following table summarizes the other supported transformations:
//
//   #datum  -> (#HASH . datum)        #rune(...)  -> (#rune ...)
//
//   [...]   -> (#SQUARE ...)          dat1dat2    -> (#JOIN dat1 . dat2)
//
//   {...}   -> (#BRACE ...)           dat1.dat2   -> (#DOT dat1 . dat2)
//
//   'datum  -> (#QUOTE . datum)       dat1:dat2   -> (#COLON dat1 . dat2)
//
//   `datum  -> (#GRAVE . datum)       dat1|dat2   -> (#PIPE dat1 . dat2)
//
//   ,datum  -> (#COMMA . datum)       #n#=datum   -> (#LABEL n . datum)
//
//   #n#     -> (#LABEL . n)
//
// Notes:
//
// * The terms datum, dat1, and dat2 each refer to an arbitrary datum; ellipsis
//   means zero or more data; n is a non-negative integer.
//
// * The #datum form applies only to expressions that cannot be mistaken for a
//   rune, such as for example: #(...) or #"..." or #'string etc.; following a
//   hash sign with a plain string would otherwise be parsed as a rune.
//
// * Though not represented in the table due to notational difficulty, the
//   format "#rune(...)" doesn't require a list in the second position; any
//   datum works, so long as there's no ambiguity; for example:
//
//     #rune1#rune2  -> (#rune1 . #rune2)
//
//     #rune"text"   -> (#rune . "text")
//
//     #rune'string  -> (#rune #QUOTE . string)
//
//   As a counter-example, following a rune immediately with a bare string is
//   not possible, since it's ambiguous:
//
//     #abcdefgh  ;Could be (#abcdef . gh) or (#abcde . fgh) or ...
//
//   The parser will see this as an attempt to use an 8-letter rune name, and
//   raise an error, since rune names are limited to 6 characters.
//
// Syntax sugar can combine arbitrarily; some examples follow:
//
//   #{...}            -> (#HASH #BRACE ...)
//
//   #'foo             -> (#HASH #QUOTE . foo)
//
//   ##'[...]          -> (#HASH #HASH #QUOTE #SQUARE ...)
//
//   {x y}[i j]        -> (#JOIN (#BRACE x y) #SQUARE i j)
//
//   foo.bar.baz{x y}  -> (#JOIN (#DOT (#DOT foo . bar) . baz) #BRACE x y)
//
// Runes are case-sensitive, and the parser only emits runes using upper-case
// letters when expressing syntax sugar, so there can be no accidental clash
// with runes that appear verbatim in code.
//
// Although strings and symbols aren't disjoint types in Zisp, the parser flags
// double-quoted string literals to allow distinguishing them from bare strings.
// Otherwise, it would not be possible for the compiler to tell the difference
// between an identifier and a string literal.
//
// You may be wondering about numbers.  As far as the parser is concerned,
// numbers are strings.  It's the decoder (see below) that will turn bare
// strings into numbers where appropriate.
//
// Note that 'foo becomes (quote foo) in Scheme, but (#QUOTE . foo) in Zisp.
// The operand of #QUOTE is the entire cdr.  The same principle is used when
// parsing other sugar; some examples follow:
//
//          Incorrect                              Correct
//
//   #(x y z) -> (#HASH (x y z))            #(x y z) -> (#HASH x y z)
//
//   [x y z]  -> (#SQUARE (x y z))          [x y z]  -> (#SQUARE x y z)
//
//   #{x}     -> (#HASH (#BRACE (x)))       #{x}     -> (#HASH #BRACE x)
//
//   foo(x y) -> (#JOIN foo (x y))          foo(x y) -> (#JOIN foo x y)
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
// whose head they appear in.  This can implement #true and #false.
//
// The decoder may also perform arbitrary transforms on any type; for example,
// it may turn bare strings (those not flagged as double-quoted) into numbers
// when appropriate.  This is how number literals are implemented.
//
// The decoder recognizes (#QUOTE ...) to implement the traditional quoting
// mechanism, but with a significant difference:
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
// not limited by the stack size.  This is also called "trampolining."
//
// All state/context is passed around via a struct pointer.  The parser has a
// main loop which calls a function, passing it the state, and also expects a
// state pointer in return.
//
// The state has a .next field, that indicates which function the main loop
// should call next.  It also has a .parent field, potentially linking to a
// previous state.  These two fields are used as follows:
//
// If a function wants to call the parser recursively, it sets the .next field
// of the *current* state to where the recursive call should return to.  Then,
// it creates a new state, sets the .next field of that one to where it should
// start (either .start_parse or .start_datum), sets .parent to the old state,
// and returns the new one.
//
// So, the main loop will now do what the newly returned state is instructing.
//
// If a function wants to make the parser return from this recursive parsing
// routine, it sets the .retval field of the current state, and sets .next to
// the special .perform_return instruction.  This makes the main loop copy the
// value in .retval into the .retval field of the parent state, and reinstates
// it, continuing with whatever was put in its .next field, which should be a
// function that will consume the return value.
//
// If .parent is null, the main loop returns .retval as the final result.
//
// Some further notes:
//
// While it's possible to just set .next and return the current state, to make
// the main loop call another function next (possibly even setting .retval to
// pass a value to it), this is completely unnecessary.  A few non-recursive
// function calls won't blow the stack.  It's only recursive parsing that we
// need the above strategy for.
//
// The difference between .start_parse and .start_datum is that the former will
// allow whitespace and comments at the beginning, whereas the latter expects a
// datum immediately without anything else in front of it.
//
// When calling the parser recursively, it may seem logical to always make it
// start with .start_datum, because we already cleared whitespace and comments
// out of the way.  However, in some cases, we must use .start_parse instead.
// This is because of datum comments.  When one appears, we start a recursive
// parser, but instead of making it return to a function that will consume the
// result, we make it return to the original state's starting point.  This way,
// the parser "retries" what it was originally doing, with the comment out of
// the way.  If we always used .start_datum for recursive parsing, this would
// never allow whitespace *after* a datum comment, because we would be back at
// .start_datum as soon as we reach the end of the datum comment.  Whitespace
// after a datum comment is sometimes allowed and sometimes not, so there's no
// generic solution that's always correct:
//
//   (foo #;bar baz)     ;; whitespace after #;bar allowed
//
//   #'#;(+ 1 2)(+ 3 4)  ;; whitespace after #;(+ 1 2) not allowed
//
// When it comes to pairs and lists, we basically treat everything as a list,
// and a pair is just seen as the shortest possible improper list.  This saves
// memory: If we implemented list parsing as pair parsing, we would be calling
// the parser recursively, deeper and deeper, for every list element.  Though
// we're not limited by stack space thanks to the strategy described above, it
// would still waste memory by creating tons of new state objects.
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

const lib = @import("../lib.zig");
const value = @import("../value.zig");

const ShortString = value.ShortString;
const Value = value.Value;

const Context = struct {
    // What to do next.
    next: Fn = .start_parse,
    // For storing some context value, like accumulated list elements.
    val: Value = undefined,
    // For storing some context char, like opening bracket.
    char: u8 = undefined,
};

const State = struct {
    input: []const u8,
    pos: usize = 0,

    context: Context = .{},
    stack: std.ArrayList(Context),
    retval: Value = undefined,

    fn init(input: []const u8, alloc: std.mem.Allocator) State {
        return .{ .input = input, .stack = .init(alloc) };
    }

    fn deinit(s: *State) void {
        s.stack.deinit();
    }

    fn recurParse(s: *State, start: Fn, end: Fn) void {
        s.stack.append(.{
            .next = end,
            .val = s.context.val,
            .char = s.context.char,
        }) catch @panic("OOM");
        s.context.next = start;
    }

    fn returnDatum(s: *State, val: Value) void {
        s.retval = val;
        if (s.stack.pop()) |c| {
            s.context = c;
        } else {
            s.context.next = .done;
        }
    }

    fn eof(s: *State) bool {
        return s.pos >= s.input.len;
    }

    fn peek(s: *State) u8 {
        return s.input[s.pos];
    }

    fn skip(s: *State) void {
        // std.debug.print("{c}\n", .{s.input[s.pos]});
        s.pos += 1;
    }

    fn getc(s: *State) u8 {
        const c = s.peek();
        s.skip();
        return c;
    }

    // Consumes whitespace and line comments.
    fn consumeBlanks(s: *State) void {
        while (!s.eof()) {
            switch (s.peek()) {
                '\t', '\n', ' ' => s.skip(),
                ';' => s.consumeLineComment(),
                else => return,
            }
        }
    }

    fn consumeLineComment(s: *State) void {
        while (!s.eof()) {
            if (s.getc() == '\n') {
                return;
            }
        }
    }

    fn isWhitespace(s: *State) bool {
        return switch (s.peek()) {
            '\t', '\n', ' ' => true,
            else => false,
        };
    }
};

const CharPred = fn (u8) bool;
const ShortStringPack = fn ([]const u8) Value;

// Helper function to read runes and short strings.
fn readShortString(
    s: *State,
    pred: CharPred,
    pack: ShortStringPack,
) ?Value {
    var str = ShortString{};
    while (!s.eof() and pred(s.peek())) {
        str.append(s.getc()) catch return null;
    }
    return pack(str.constSlice());
}

// Probably best *not* to use function pointers here, but rather dispatch to
// functions manually based on enum value.  This should help the optimizer.

const Fn = enum {
    start_parse,
    start_datum,
    end_join_datum,
    end_label_datum,
    end_hash_datum,
    end_quote_datum,
    continue_list,
    finish_improper_list,
    end_improper_list,
    done,
};

pub fn parse(input: []const u8) Value {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer if (gpa.deinit() == .leak) @panic("leak");
    var s: State = .init(input, gpa.allocator());
    defer s.deinit();
    while (s.context.next != .done) call(&s);
    return s.retval;
}

fn call(s: *State) void {
    // std.debug.print("{}\n", .{s.next});
    switch (s.context.next) {
        .start_parse => startParse(s),
        .start_datum => startDatum(s),
        .end_join_datum => endJoinedDatum(s),
        .end_label_datum => endLabelDatum(s),
        .end_hash_datum => endHashDatum(s),
        .end_quote_datum => endQuoteDatum(s),
        .continue_list => continueList(s),
        .finish_improper_list => finishImproperList(s),
        .end_improper_list => endImproperList(s),
        .done => unreachable,
    }
}

fn startParse(s: *State) void {
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
fn startDatum(s: *State) void {
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

        '.' => err(s, "misplaced period"),

        else => startBareString(s),
    };
}

fn endDatum(s: *State, d: Value) void {
    //
    // We're at the end of a datum; check for the various ways data can be
    // joined together, like DATUM|DATUM or DATUM|.DATUM etc.
    //

    if (isEndOfDatum(s)) {
        // Nope, end it.
        return s.returnDatum(d);
    }

    // There's a stupid special-case we have to handle here, where a datum
    // comment may fool us into thinking there's something to join: foo|#;bar

    const c = s.peek();
    switch (c) {
        '.', ':', '|' => s.skip(),
        '#' => if (checkTrailingDatumComment(s)) {
            return s.returnDatum(d);
        },
        else => {},
    }
    s.context.val = d;
    s.context.char = c;
    return s.recurParse(.start_datum, .end_join_datum);
}

fn checkTrailingDatumComment(s: *State) bool {
    const pos = s.pos;
    s.skip();
    if (s.eof()) {
        // Error, but let it be handled later.
        return false;
    }
    const c = s.peek();
    s.pos = pos;
    return c == ';';
}

fn isEndOfDatum(s: *State) bool {
    return s.eof() or switch (s.peek()) {
        '\t', '\n', ' ', ';', ')', ']', '}' => true,
        else => false,
    };
}

fn endJoinedDatum(s: *State) void {
    const rune = value.rune.pack(switch (s.context.char) {
        '.' => "DOT",
        ':' => "COLON",
        '|' => "PIPE",
        else => "JOIN",
    });
    const joined = value.pair.cons(s.context.val, s.retval);
    return endDatum(s, value.pair.cons(rune, joined));
}

fn handleHash(s: *State) void {
    s.skip();
    //
    // We just consumed a hash.  Possibilities include:
    //
    //   #|foo          ;rune
    //
    //   #|n#[=DATUM]   ;datum label, with or without datum
    //
    //   #|;DATUM       ;datum comment
    //
    //   #|DATUM        ;hash-datum
    //

    if (s.eof()) {
        return err(s, "EOF after hash");
    }
    if (s.isWhitespace()) {
        return err(s, "whitespace after hash");
    }

    switch (s.peek()) {
        'a'...'z', 'A'...'Z' => return handleRune(s),
        '0'...'9' => return handleDatumLabel(s),
        ';' => {
            s.skip();
            // Don't change next in this case.  Just let the parser redo what it
            // was doing as soon as the commented-out datum has been read.
            return s.recurParse(.start_datum, s.context.next);
        },
        else => return s.recurParse(.start_datum, .end_hash_datum),
    }
}

fn handleRune(s: *State) void {
    const r = readRune(s) orelse return err(s, "rune too long");
    return endDatum(s, r);
}

fn readRune(s: *State) ?Value {
    return readShortString(s, std.ascii.isAlphanumeric, value.rune.pack);
}

fn handleDatumLabel(s: *State) void {
    const n = readDatumLabel(s) orelse return err(s, "datum label too long");
    //
    // We're at the end of the numeric label now; possibilities are:
    //
    //   #n|#
    //
    //   #n|#=DATUM
    //

    if (s.eof()) {
        return err(s, "unexpected EOF while reading datum label");
    }
    if (s.getc() != '#') {
        return err(s, "invalid character while reading datum label");
    }

    if (s.eof() or s.isWhitespace()) {
        const rune = value.rune.pack("LABEL");
        return endDatum(s, value.pair.cons(rune, n));
    }

    if (s.getc() != '=') {
        return err(s, "invalid character after numeric datum label");
    }

    s.context.val = n;
    return s.recurParse(.start_datum, .end_label_datum);
}

fn readDatumLabel(s: *State) ?Value {
    return readShortString(s, std.ascii.isDigit, value.sstr.pack);
}

fn endLabelDatum(s: *State) void {
    const rune = value.rune.pack("LABEL");
    const payload = value.pair.cons(s.context.val, s.retval);
    return endDatum(s, value.pair.cons(rune, payload));
}

fn endHashDatum(s: *State) void {
    const rune = value.rune.pack("HASH");
    return endDatum(s, value.pair.cons(rune, s.retval));
}

fn startQuotedString(s: *State) void {
    // We're at |"..." so consume the opening quote before we start reading.
    s.skip();

    const str = readQuotedString(s) catch return err(s, "unclosed string");
    return endDatum(s, str);
}

// RQS = Read Quoted String
const RqsError = enum { Unclosed };

fn readQuotedString(s: *State) !Value {
    return try readQuotedSstr(s) orelse readQuotedLongString(s);
}

fn readQuotedSstr(s: *State) !?Value {
    const start_pos = s.pos;

    // TODO: Handle escapes.
    var buf: [6]u8 = undefined;
    var i: u8 = 0;
    while (!s.eof()) {
        const c = s.getc();
        if (c == '"') {
            // ok, return what we accumulated
            return value.sstr.packQuoted(buf[0..i]);
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

fn startBareString(s: *State) void {
    // We're at |foo so start reading directly.
    return readBareSstr(s) orelse readBareLongString(s);
}

fn readBareSstr(s: *State) ?void {
    const sp = s.pos;
    if (readShortString(s, isSstrChar, value.sstr.pack)) |sstr| {
        return endDatum(s, sstr);
    } else {
        s.pos = sp;
        return null;
    }
}

fn isSstrChar(c: u8) bool {
    // We will ignore illegal characters here, because they aren't consumed by
    // this function; whatever code comes next must handle them.
    return switch (c) {
        '(', ')', '[', ']', '{', '}', ';', '#', '"', '\'', '`', ',' => false,
        0...32, 127...255 => false,
        else => true,
    };
}

fn readBareLongString(s: *State) void {
    return err(s, "NOT YET IMPLEMENTED");
}

fn startQuote(s: *State) void {
    // We're at one of:  |'...  |`...  |,...
    s.context.val = value.rune.pack(switch (s.getc()) {
        '\'' => "QUOTE",
        '`' => "GRAVE",
        ',' => "COMMA",
        else => unreachable,
    });
    return s.recurParse(.start_datum, .end_quote_datum);
}

fn endQuoteDatum(s: *State) void {
    return endDatum(s, value.pair.cons(s.context.val, s.retval));
}

// List processing is, unsurprisingly, the most complicated, and it's made even
// more complicated by the possibility of datum comments in strange places...

// Make sure to use .start_parse instead of .start_datum to handle elements, so
// that an arbitrary number of datum comments, separated by blanks (whitespace
// and line comments) are handled automatically.

fn startList(s: *State) void {
    const open = s.getc();

    s.consumeBlanks();
    if (s.eof()) {
        return err(s, "unexpected EOF while parsing list");
    }

    s.context.val = value.nil.nil;
    s.context.char = open;
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

fn endList(s: *State) void {
    const open = s.context.char;
    const char = s.getc();

    if (open == '(' and char == ')') {
        return endDatum(s, s.context.val);
    }
    if (open == '[' and char == ']') {
        const rune = value.rune.pack("SQUARE");
        return endDatum(s, value.pair.cons(rune, s.context.val));
    }
    if (open == '{' and char == '}') {
        const rune = value.rune.pack("BRACE");
        return endDatum(s, value.pair.cons(rune, s.context.val));
    }

    return err(s, "wrong closing bracket for list");
}

fn continueList(s: *State) void {
    // Note that this accumulates list elements in reverse.
    s.context.val = value.pair.cons(s.retval, s.context.val);

    s.consumeBlanks();
    if (s.eof()) {
        return err(s, "unexpected EOF while parsing list");
    }

    if (isEndOfList(s)) {
        s.context.val = lib.list.reverse(s.context.val);
        return endList(s);
    }

    if (s.peek() == '.') {
        s.skip();
        // Scheme allows (foo .(bar)) but we don't.  Mind your spaces!
        if (!s.isWhitespace()) {
            return err(s, "misplaced period");
        }
        return s.recurParse(.start_parse, .finish_improper_list);
    }

    return s.recurParse(.start_parse, .continue_list);
}

fn finishImproperList(s: *State) void {
    s.context.val = lib.list.reverseWithTail(s.context.val, s.retval);
    return endImproperList(s);
}

// Handling the end of an improper list is a bit awkward, because there may be
// datum comments *after* the final cdr, where we don't actually want to parse
// any further data.  So we keep looping here just looking for datum comments.

fn endImproperList(s: *State) void {
    s.consumeBlanks();
    if (s.eof()) {
        return err(s, "unexpected EOF at end of improper list");
    }

    if (isEndOfList(s)) {
        return endList(s);
    }

    if (s.getc() == '#') {
        if (s.eof()) {
            return err(s, "unexpected hash and EOF at end of improper list");
        }
        if (s.getc() == ';') {
            return s.recurParse(.start_datum, .end_improper_list);
        }
    }

    return err(s, "malformed list / extra datum at end of improper list");
}

fn err(s: *State, msg: []const u8) noreturn {
    std.debug.print("{s}\n", .{msg});
    std.debug.print("pos: {}\n", .{s.pos});
    @panic("parse error");
}
