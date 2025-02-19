const std = @import("std");

const gc = @import("gc.zig");
const value = @import("value.zig");

const Value = value.Value;

const Next = enum {
    start,
    datum,
    hash_end,
    rune_datum_end,
    quote_end,
    list,
    list_end,
    done,
};

const State = struct {
    alloc: std.mem.Allocator,

    input: []const u8,
    pos: usize = 0,

    mode: enum { code, data } = .code,

    next: Next = .start,

    parent: ?*State = null,

    last_rune: ?Value = null,
    list_tail: ?Value = null,

    retval: Value = value.eof.eof,

    fn eof(self: *State) bool {
        return self.pos >= self.input.len;
    }

    fn peek(self: *State) u8 {
        return self.input[self.pos];
    }

    fn getc(self: *State) u8 {
        const c = self.peek();
        self.pos += 1;
        return c;
    }

    fn isFinalNull(self: *State) bool {
        return self.peek() == 0 and self.pos == self.input.len - 1;
    }

    fn newChild(self: *State, next: Next) *State {
        const s = self.alloc.create(State) catch @panic("OOM");
        s.* = State{ .alloc = self.alloc, .input = self.input };
        s.pos = self.pos;
        s.mode = self.mode;
        s.next = next;
        s.parent = self;
        return s;
    }

    fn setReturn(self: *State, val: Value) *State {
        self.retval = val;
        self.next = .done;
        return self;
    }

    fn finish(self: *State) ?*State {
        if (self.parent) |p| {
            p.retval = self.retval;
            p.pos = self.pos;
            p.alloc.destroy(self);
            return p;
        } else {
            return null;
        }
    }
};

pub fn read(input: []const u8) Value {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    var top = State{ .alloc = gpa.allocator(), .input = input };
    var s = &top;
    while (true) s = switch (s.next) {
        .start => start(s),
        .datum => datum(s),
        .hash_end => hashEnd(s),
        .rune_datum_end => runeDatumEnd(s),
        .quote_end => quoteEnd(s),
        .list => list(s),
        .list_end => list(s),
        .done => s.finish() orelse break,
    };
    if (s.eof() or s.isFinalNull()) {
        return s.retval;
    } else {
        @panic("unconsumed input");
    }
}

fn start(s: *State) *State {
    while (true) {
        if (s.eof()) {
            s.next = .done;
            return s;
        }
        const c = s.getc();
        if (isWhitespace(c)) {
            continue;
        }
        return switch (c) {
            // whitespace checked above
            0...31, 127...255 => err(s, "invalid character"),
            ')', ']', '}' => err(s, "unexpected closing bracket"),
            ';' => semi(s),
            else => ret: {
                // backtrack; let other function handle it
                s.pos -= 1;
                break :ret datum(s);
            },
        };
    }
}

fn semi(s: *State) *State {
    while (true) {
        if (s.eof()) {
            s.next = .done;
            return s;
        }
        const c = s.getc();
        if (c == '\n') {
            break;
        }
    }
    return s;
}

fn datum(s: *State) *State {
    const c = s.getc();
    if (isWhitespace(c)) {
        return err(s, "expected datum, got whitespace");
    }
    return switch (c) {
        // whitespace checked above
        0...31, 127...255 => err(s, "invalid character"),
        ')', ']', '}' => err(s, "unexpected closing bracket"),
        ';' => err(s, "expected datum, got semicolon"),
        '"' => string(s),
        '#' => hash(s),
        '\'' => quote(s),
        '(' => list(s),
        '+' => plus(s),
        ',' => comma(s),
        '.' => dot(s),
        '0'...'9' => number(s),
        '[' => square(s),
        '`' => backtick(s),
        '{' => brace(s),
        else => symbol(s),
    };
}

fn isWhitespace(c: u8) bool {
    return switch (c) {
        '\t', '\n', ' ' => true,
        else => false,
    };
}

// Whitespace, semicolon, and closing brackets of any kind
fn isEndDelimiter(c: u8) bool {
    return switch (c) {
        '\t', '\n', ' ', ';' => true,
        ')', ']', '}' => true,
        else => false,
    };
}

fn string(s: *State) *State {
    const str = readString(s) catch return err(s, "unclosed string");
    if (s.mode == .code) {
        // "foo bar" => (#string . "foo bar")
        const rune = value.rune.pack("string");
        const pair = value.pair.cons(rune, str);
        return s.setReturn(pair);
    } else {
        return s.setReturn(str);
    }
}

const StringReadError = enum { UnclosedString };

fn readString(s: *State) error{UnclosedString}!Value {
    return try tryReadSstr(s) orelse readLongString(s);
}

fn tryReadSstr(s: *State) error{UnclosedString}!?Value {
    // We will reset to this position if we fail.
    const start_pos = s.pos;

    var buf: [6]u8 = undefined;
    var i: usize = 0;
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

fn readLongString(s: *State) Value {
    _ = s;
    @panic("not implemented");
}

fn hash(s: *State) *State {
    if (isWhitespace(s.peek())) {
        return err(s, "whitespace after hash sign");
    }

    // is it a datum comment?
    if (s.peek() == ';') {
        // consume semicolon
        _ = s.getc();
        // Just ignore value and return to starting state after reading it.
        s.next = .start;
    } else {
        s.next = .hash_end;
    }

    // No whitespace or anything; hash must be immediately followed by datum,
    // including if it's a datum comment.  Note that if it's actually a rune
    // we're reading, like #foo, we abuse our ability to reading an sstr here
    // and later turn it into a rune instead, since they're the same length.
    return s.newChild(.datum);
}

fn hashEnd(s: *State) *State {
    // It's not actually a sstr but a rune, like: #foo or #foo(...)
    if (value.sstr.check(s.retval)) {
        return hashRuneEnd(s);
    }

    // Hash followed by an actual datum; becomes a (#hash ...) invocation:
    //
    //   #(...) -> (#hash . (...))
    //
    //   #"..." -> (#hash . "...")
    //

    // But data mode doesn't allow that.
    if (s.mode == .data) {
        return err(s, "invalid use of hash in data mode");
    }

    // Also, bare long strings are not OK here; too similar to a rune.
    if (value.ptr.checkZisp(s.retval, .string)) {
        return err(s, "long string after hash sign");
    }

    return s.setReturn(value.pair.cons(
        value.rune.pack("hash"),
        s.retval,
    ));
}

// Note: Can only come here from hashEnd().
fn hashRuneEnd(s: *State) *State {
    // Convert the fake sstr that was meant to be a rune.
    const rune = value.rune.make(s.retval);

    // Maybe it's a stand-alone rune, like: #foo
    if (isEndDelimiter(s.peek())) {
        // Which is only allowed in data mode.
        if (s.mode == .code) {
            return err(s, "bare runes not allowed in code");
        } else {
            return s.setReturn(rune);
        }
    }

    // Otherwise, it's followed by a datum, like: #foo(...)

    // Which is only allowed in code mode.
    if (s.mode == .data) {
        return err(s, "invalid use of hash in data mode");
    } else {
        s.last_rune = rune;
        s.next = .rune_datum_end;
        return s.newChild(.datum);
    }
}

fn runeDatumEnd(s: *State) *State {
    if (s.last_rune) |rune| {
        return s.setReturn(value.pair.cons(rune, s.retval));
    } else {
        unreachable;
    }
}

fn quote(s: *State) *State {
    // Allowed in Scheme, but why? Not in Zisp.
    if (isWhitespace(s.peek())) {
        return err(s, "whitespace after apostrophe");
    }
    s.next = .quote_end;
    const c = s.newChild(.datum);
    c.mode = .data;
    return c;
}

fn quoteEnd(s: *State) *State {
    return s.setReturn(value.pair.cons(
        value.rune.pack("quote"),
        s.retval,
    ));
}

fn list(s: *State) *State {
    return s;
}

fn plus(s: *State) *State {
    return s;
}

fn comma(s: *State) *State {
    return s;
}

fn dot(s: *State) *State {
    return s;
}

fn number(s: *State) *State {
    return s;
}

fn square(s: *State) *State {
    return s;
}

fn backtick(s: *State) *State {
    return s;
}

fn brace(s: *State) *State {
    return s;
}

fn symbol(s: *State) *State {
    return s;
}

fn err(s: *State, msg: []const u8) *State {
    _ = s;
    std.debug.print("{s}\n", .{msg});
    @panic("reader error");
}
