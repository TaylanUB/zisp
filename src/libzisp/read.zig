const std = @import("std");

const gc = @import("gc.zig");
const value = @import("value.zig");

const Value = value.Value;

const State = struct {
    alloc: std.mem.Allocator,
    input: []const u8,
    pos: usize = 0,

    next: enum {
        start,

        list,
        list_end,

        err,

        done,
    } = .start,

    retval: Value = value.eof.eof,

    parent: ?*State = null,
};

pub fn read(input: []const u8) Value {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    var top = State{ .alloc = gpa.allocator(), .input = input };
    var s = &top;
    while (s.pos <= s.input.len) : (s.pos += 1) {
        s = switch (s.next) {
            .start => start(s),

            .list => list(s),
            .list_end => list(s),

            .err => err(s),

            .done => ret: {
                if (s.parent) |parent| {
                    s.alloc.destroy(s);
                    break :ret parent;
                } else {
                    return s.retval;
                }
            },
        };
    }
    unreachable;
}

fn start(s: *State) *State {
    switch (s.input[s.pos]) {
        0...8 => s.next = .err,

        '\t', '\n' => {},

        11...31 => s.next = .err,

        ' ' => {},

        '!' => s.next = .err,

        '"' => quotedString(s),

        else => s.next = .err,
    }
    return s;
}

fn quotedString(s: *State) void {
    var buf: [6]u8 = .{0} ** 6;
    const len = readString(&buf, s);
    s.retval = value.pair.cons(
        value.sstr.pack("quote"),
        value.pair.cons(
            value.sstr.pack(buf[0..len]),
            value.nil.nil,
        ),
    );
    s.next = .done;
}

fn readString(buf: []u8, s: *State) usize {
    s.pos += 1; // skip opening quote
    for (s.input[s.pos..], 0..) |c, i| {
        if (c == '"') {
            s.pos += i;
            return i;
        }
        buf[i] = c;
    }
    unreachable;
}

fn list(s: *State) *State {
    return s;
}

fn err(s: *State) *State {
    return s;
}
