const std = @import("std");

const value = @import("../value.zig");

const ShortString = value.ShortString;
const OtherTag = value.OtherTag;
const Value = value.Value;

// Zig API

pub fn check(v: Value) bool {
    return v.isOtherTag(.sstr) or v.isOtherTag(.qstr);
}

pub fn assert(v: Value) void {
    if (!check(v)) {
        v.dump();
        @panic("not sstr");
    }
}

pub fn checkQuoted(v: Value) bool {
    return v.isOtherTag(.qstr);
}

// For now, ignore encoding, just treat it as []u8.

pub fn isValidSstr(s: []const u8) bool {
    if (s.len > 6) {
        return false;
    }
    for (s) |c| {
        if (c == 0) {
            return false;
        }
    }
    return true;
}

fn assertValidSstr(s: []const u8) void {
    if (!isValidSstr(s)) {
        std.debug.print("invalid sstr: {s}\n", .{s});
        @panic("invalid sstr");
    }
}

// Different ways of doing the following have been tested, including manual
// shifting and bit masking, but memcpy always wins easily according to our
// micro-benchmarks, under both ReleaseSafe and ReleaseFast.

// Note: rune.zig uses equivalent code; probably good to keep in sync.

pub fn pack(s: []const u8) Value {
    return _pack(s, .sstr);
}

pub fn packQuoted(s: []const u8) Value {
    return _pack(s, .qstr);
}

fn _pack(s: []const u8, tag: OtherTag) Value {
    assertValidSstr(s);
    var v = Value{ .sstr = .{ .string = 0, .tag = tag } };
    const dest: [*]u8 = @ptrCast(&v.sstr.string);
    @memcpy(dest, s);
    return v;
}

pub fn unpack(v: Value) ShortString {
    assert(v);
    const s: [6]u8 = @bitCast(v.sstr.string);
    inline for (0..6) |i| {
        if (s[i] == 0) return .{ .buffer = s, .len = i };
    }
    return .{ .buffer = s, .len = 6 };
}

// No Zisp API for sstr specifically, since it's a string.  See string.zig.
