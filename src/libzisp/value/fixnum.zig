const std = @import("std");

const Value = @import("../value.zig").Value;

// Zig API

/// Checks for a Zisp fixnum.
pub fn check(v: Value) bool {
    return v.isPacked() and v.fixnum.is_fixnum;
}

/// Asserts check().
pub fn assert(v: Value) void {
    if (!check(v)) {
        v.dump();
        @panic("not fixnum");
    }
}

// See detailed NaN packing docs for why the +/- 1.
const fixnum_min = std.math.minInt(i52) + 1;
const fixnum_max = std.math.maxInt(i52) - 1;

fn isValidRange(int: i64) bool {
    return fixnum_min < int and int < fixnum_max;
}

fn assertValidRange(int: i64) void {
    if (int < fixnum_min) {
        std.debug.print("int too small for fixnum: {}", .{int});
        @panic("int too small for fixnum");
    }
    if (int > fixnum_max) {
        std.debug.print("int too large for fixnum: {}", .{int});
        @panic("int too large for fixnum");
    }
}

fn packNegative(int: i64) Value {
    return @bitCast(int);
}

fn unpackNegative(v: Value) i64 {
    return @bitCast(v);
}

const positive_mask: u64 = 0xfff7ffffffffffff;

fn packPositive(int: i64) Value {
    const uint: u64 = @bitCast(int);
    return @bitCast(uint ^ positive_mask);
}

fn unpackPositive(v: Value) i64 {
    const uint: u64 = @bitCast(v);
    return @bitCast(uint ^ positive_mask);
}

pub fn pack(int: i64) Value {
    assertValidRange(int);
    if (int < 0) {
        return packNegative(int);
    } else {
        return packPositive(int);
    }
}

pub fn unpack(v: Value) i64 {
    assert(v);
    if (v.fixnum.negative) {
        return unpackNegative(v);
    } else {
        return unpackPositive(v);
    }
}

// Zisp API

pub fn pred(v: Value) Value {
    return Value.boole.pack(check(v));
}

pub fn add(v1: Value, v2: Value) Value {
    const int1 = unpack(v1);
    const int2 = unpack(v2);
    return pack(int1 + int2);
}
