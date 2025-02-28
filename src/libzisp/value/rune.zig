const std = @import("std");

const value = @import("../value.zig");

const ShortString = value.ShortString;
const Value = value.Value;

// Zig API

pub fn check(v: Value) bool {
    return v.isOtherTag(.rune);
}

pub fn assert(v: Value) void {
    if (!check(v)) {
        v.dump();
        @panic("not rune");
    }
}

pub fn isValidRune(s: []const u8) bool {
    if (s.len == 0 or s.len > 6) {
        return false;
    }
    for (s) |c| {
        switch (c) {
            'A'...'Z' => {},
            'a'...'z' => {},
            else => return false,
        }
    }
    return true;
}

fn assertValidRune(s: []const u8) void {
    if (!isValidRune(s)) {
        std.debug.print("invalid rune: '{s}'\n", .{s});
        @panic("invalid rune");
    }
}

// See sstr.zig which uses equivalent code; probably good to keep in sync.

pub fn pack(s: []const u8) Value {
    assertValidRune(s);
    var v = Value{ .rune = .{ .name = 0 } };
    const dest: [*]u8 = @ptrCast(&v.rune.name);
    @memcpy(dest, s);
    return v;
}

pub fn unpack(v: Value) ShortString {
    assert(v);
    const s: [6]u8 = @bitCast(v.rune.name);
    inline for (0..6) |i| {
        if (s[i] == 0) return .{ .buffer = s, .len = i };
    }
    return .{ .buffer = s, .len = 6 };
}

// Zisp API

pub fn pred(v: Value) Value {
    return value.boole.pack(check(v));
}

pub fn make(v: Value) Value {
    const s, const l = value.sstr.unpack(v);
    return pack(s[0..l]);
}

pub fn getName(v: Value) Value {
    const s, const l = unpack(v);
    return value.sstr.pack(s[0..l]);
}

// TODO: Registering decoders
