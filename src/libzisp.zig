//! By convention, root.zig is the root source file when making a library. If
//! you are making an executable, the convention is to delete this file and
//! start with main.zig instead.
const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;

const value = @import("libzisp/value.zig");

test "double" {
    const d1: f64 = 0.123456789;
    const d2: f64 = -0.987654321;
    const v1 = value.double.pack(d1);
    const v2 = value.double.pack(d2);
    const v3 = value.double.add(v1, v2);
    const result = value.double.unpack(v3);

    try std.testing.expect(value.double.check(v1));
    try std.testing.expect(value.double.check(v2));
    try std.testing.expect(value.double.check(v3));
    try std.testing.expect(result == d1 + d2);
}

test "fixnum" {
    const int1: i64 = 123456789;
    const int2: i64 = -987654321;
    const v1 = value.fixnum.pack(int1);
    const v2 = value.fixnum.pack(int2);
    const v3 = value.fixnum.add(v1, v2);
    const result = value.fixnum.unpack(v3);

    try std.testing.expect(value.fixnum.check(v1));
    try std.testing.expect(value.fixnum.check(v2));
    try std.testing.expect(value.fixnum.check(v3));
    try std.testing.expect(result == int1 + int2);
}

test "ptr" {
    const ptr1 = value.ptr.pack(@ptrFromInt(256), value.ptr.Tag.string);
    try std.testing.expect(value.ptr.check(ptr1));
    try std.testing.expect(value.ptr.checkZisp(ptr1));
    try std.testing.expect(value.ptr.checkNormal(ptr1));

    const ptr2 = value.ptr.makeWeak(ptr1);
    try std.testing.expect(value.ptr.check(ptr2));
    try std.testing.expect(value.ptr.checkZisp(ptr2));
    try std.testing.expect(value.ptr.checkWeak(ptr2));

    // Make sure ptr1 wasn't modified
    try std.testing.expect(value.ptr.checkNormal(ptr1));
}
