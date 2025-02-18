const std = @import("std");

const Value = @import("../value.zig").Value;

// Zig API

pub fn check(v: Value) bool {
    return v.isOther(.sstr);
}

pub fn assert(v: Value) void {
    if (!check(v)) {
        v.dump();
        @panic("not sstr");
    }
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
        std.debug.print("invalid sstr: {s}", .{s});
        @panic("invalid sstr");
    }
}

// Different ways of doing the following have been tested, including manual
// shifting and bit masking, but memcpy always wins easily according to our
// micro-benchmarks, under both ReleaseSafe and ReleaseFast.

pub fn pack(s: []const u8) Value {
    assertValidSstr(s);
    var v = Value{ .sstr = .{ .string = 0 } };
    const dest: [*]u8 = @ptrCast(&v.sstr.string);
    @memcpy(dest, s);
    return v;
}

pub fn unpack(v: Value) struct { [6]u8, u3 } {
    const s: [6]u8 = @bitCast(v.sstr.string);
    inline for (0..6) |i| {
        if (s[i] == 0) return .{ s, i };
    }
    return .{ s, 6 };
}

// No Zisp API for sstr specifically, since it's a string.  See string.zig.
