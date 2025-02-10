//! By convention, root.zig is the root source file when making a library. If
//! you are making an executable, the convention is to delete this file and
//! start with main.zig instead.
const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;

// Read the following article to understand the NaN-packing strategy:
//
// https://tkammer.de/zisp/notes/nan.html
//
// Note: Packed structs are least-to-most significant, so the order of fields
// must be reversed relative to a typical big-endian illustration of the bit
// patterns of IEEE 754 double-precision floating point numbers.

const Value = packed union {
    double: f64,
    nan: packed struct {
        rest: u51,
        quiet: u1,
        exp: u11,
        sign: u1,
    },
    int: packed struct {
        code: u51,
        neg: bool,
        exp: u11,
        is_int: bool,
    },
    pointer: packed struct {
        value: u48,
        type: u3,
        _zo: u1,
        _qnan: u12,
    },
};

// Helpers

inline fn zisp_dump(v: Value) void {
    std.debug.dumpHex(std.mem.asBytes(&v));
}

///! Checks for any IEEE 754 NaN.
inline fn zisp_is_nan(v: Value) bool {
    return v.nan.exp == std.math.maxInt(u11);
}

///! Checks for a Zisp value packed into a NaN.
inline fn zisp_is_packed(v: Value) bool {
    return zisp_is_nan(v) and v.nan.rest != 0;
}

///! Checks for a regular double including infinity or canonical NaN
inline fn zisp_is_double(v: Value) bool {
    return !zisp_is_packed(v);
}

inline fn zisp_assert_double(v: Value) void {
    if (!zisp_is_double(v)) {
        zisp_dump(v);
        @panic("not double");
    }
}

inline fn zisp_is_int(v: Value) bool {
    return zisp_is_packed(v) and v.int.is_int;
}

inline fn zisp_assert_int(v: Value) void {
    if (!zisp_is_int(v)) {
        zisp_dump(v);
        @panic("not int");
    }
}

// See detailed NaN packing docs for why the +/- 1.
const zisp_int_min = std.math.minInt(i52) + 1;
const zisp_int_max = std.math.maxInt(i52) - 1;

inline fn zisp_assert_int_range(int: i64) void {
    if (int < zisp_int_min) {
        std.debug.print("int to pack is too small: {}", .{int});
        @panic("int to pack is too small");
    }
    if (int > zisp_int_max) {
        std.debug.print("int to pack is too large: {}", .{int});
        @panic("int to pack is too large");
    }
}

inline fn zisp_int_pack_neg(int: i64) Value {
    return @bitCast(int);
}

inline fn zisp_int_unpack_neg(v: Value) i64 {
    return @bitCast(v);
}

const zisp_int_pos_mask: u64 = 0xfff7ffffffffffff;

inline fn zisp_int_pack_pos(int: i64) Value {
    const uint: u64 = @bitCast(int);
    return @bitCast(uint ^ zisp_int_pos_mask);
}

inline fn zisp_int_unpack_pos(v: Value) i64 {
    const uint: u64 = @bitCast(v);
    return @bitCast(uint ^ zisp_int_pos_mask);
}

inline fn zisp_int_pack(int: i64) Value {
    zisp_assert_int_range(int);
    if (int < 0) {
        return zisp_int_pack_neg(int);
    } else {
        return zisp_int_pack_pos(int);
    }
}

inline fn zisp_int_unpack(v: Value) i64 {
    zisp_assert_int(v);
    if (v.int.neg) {
        return zisp_int_unpack_neg(v);
    } else {
        return zisp_int_unpack_pos(v);
    }
}

// Doubles

pub fn zisp_double(d: f64) Value {
    return @bitCast(d);
}

// pub fn zisp_double_p(v: Value) Value {
//     return zisp_bool(zisp_is_double(v));
// }

pub fn zisp_double_get(v: Value) f64 {
    zisp_assert_double(v);
    return v.double;
}

pub fn zisp_double_add(v1: Value, v2: Value) Value {
    const d1 = zisp_double_get(v1);
    const d2 = zisp_double_get(v2);
    return zisp_double(d1 + d2);
}

// Ints

pub fn zisp_int(int: i64) Value {
    return zisp_int_pack(int);
}

// pub fn zisp_int_p(v: Value) Value {
//     return zisp_bool(zisp_is_int(v));
// }

pub fn zisp_int_get(v: Value) i64 {
    return zisp_int_unpack(v);
}

pub fn zisp_int_add(v1: Value, v2: Value) Value {
    const int1 = zisp_int_get(v1);
    const int2 = zisp_int_get(v2);
    return zisp_int(int1 + int2);
}

// Tests

test "double add functionality" {
    const d1: f64 = 0.123456789;
    const d2: f64 = -0.987654321;
    const v1 = zisp_double(d1);
    const v2 = zisp_double(d2);
    const v3 = zisp_double_add(v1, v2);
    const result = zisp_double_get(v3);
    try std.testing.expect(result == d1 + d2);
}

test "int add functionality" {
    const int1: i64 = 123456789;
    const int2: i64 = -987654321;
    const v1 = zisp_int(int1);
    const v2 = zisp_int(int2);
    const v3 = zisp_int_add(v1, v2);
    const result = zisp_int_get(v3);
    try std.testing.expect(result == int1 + int2);
}
