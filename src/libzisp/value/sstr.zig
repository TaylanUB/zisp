const std = @import("std");

const Value = @import("../value.zig").Value;

// Zig API

pub fn check(v: Value) bool {
    return v.isPacked() and
        !v.sstr.fixnum and
        !v.sstr.ptr and
        v.sstr.tag == .sstr;
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
// micro-benchmarks, both under ReleaseSafe and under ReleaseFast.

pub fn pack(s: []const u8) Value {
    assertValidSstr(s);
    var v = Value{ .sstr = .{ .value = 0 } };
    const dest: [*]u8 = @ptrCast(&v.sstr.value);
    @memcpy(dest, s);
    return v;
}

// It's tempting to inline for here to eliminate the if statement or prevent
// need of @truncate but all alternatives were a little slower.

pub fn unpack(v: Value) struct { [6]u8, u3 } {
    var s: [6]u8 = undefined;
    const src: *const [6]u8 = @ptrCast(&v.sstr.value);
    @memcpy(&s, src);
    for (0..6) |i| {
        if (s[i] == 0) return .{ s, @intCast(i) };
    }
    return .{ s, 6 };
}
