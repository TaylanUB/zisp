const std = @import("std");
const value = @import("../value.zig");
const gc = @import("../gc.zig");

const Bucket = gc.Bucket;
const Value = value.Value;

// Zig API

pub fn check(v: Value) bool {
    return v.isPtr();
}

pub fn assert(v: Value) void {
    if (!check(v)) {
        v.dump();
        @panic("not a pointer");
    }
}

// Foreign Pointers

pub fn checkForeign(v: Value) bool {
    return check(v) and v.ptr.is_foreign;
}

pub fn assertForeign(v: Value) void {
    if (!checkForeign(v)) {
        v.dump();
        @panic("not foreign pointer");
    }
}

pub fn checkForeignRange(ptr: *anyopaque) bool {
    const int = @intFromPtr(ptr);
    return int <= std.math.maxInt(u50);
}

fn assertForeignRange(ptr: *anyopaque) void {
    if (!checkForeignRange(ptr)) {
        std.debug.print("foreign pointer out of range: {}\n", .{ptr});
        @panic("foreign pointer out of range");
    }
}

pub fn packForeign(ptr: *anyopaque) Value {
    assertForeignRange(ptr);
    const int: u50 = @intCast(@intFromPtr(ptr));
    return .{ .fptr = .{ .value = int } };
}

pub fn unpackForeign(v: Value) *anyopaque {
    assertForeign(v);
    return @ptrFromInt(v.fptr.value);
}

// Zisp Pointers

fn _checkZisp(v: Value) bool {
    return check(v) and !v.ptr.is_foreign;
}

fn _assertZisp(v: Value) void {
    if (!_checkZisp(v)) {
        v.dump();
        @panic("not zisp pointer");
    }
}

pub fn checkWeak(v: Value) bool {
    return _checkZisp(v) and v.zptr.is_weak;
}

pub fn assertWeak(v: Value) void {
    if (!checkWeak(v)) {
        v.dump();
        @panic("not zisp weak pointer");
    }
}

pub fn checkZisp(v: Value, tag: Tag) bool {
    return _checkZisp(v) and unpack(v).@"1" == tag;
}

pub fn assertZisp(v: Value, tag: Tag) void {
    if (!checkZisp(v, tag)) {
        v.dump();
        @panic("not zisp pointer or wrong tag");
    }
}

pub fn checkStrong(v: Value) bool {
    return _checkZisp(v) and !v.zptr.is_weak;
}

pub fn assertStrong(v: Value) void {
    if (!checkStrong(v)) {
        v.dump();
        @panic("not zisp strong pointer");
    }
}

pub fn packZisp(ptr: [*]Bucket, tag: Tag, is_weak: bool) Value {
    return .{ .zptr = .{
        .tagged_value = tagPtr(ptr, tag),
        .is_weak = is_weak,
    } };
}

pub fn pack(ptr: [*]Bucket, tag: Tag) Value {
    return packZisp(ptr, tag, false);
}

pub fn packWeak(ptr: [*]Bucket, tag: Tag) Value {
    return packZisp(ptr, tag, true);
}

// Unpacks weak as well; no need for a separate fn.
pub fn unpack(v: Value) struct { [*]Bucket, Tag } {
    _assertZisp(v);
    return untagPtr(v.zptr.tagged_value);
}

pub fn setWeakNull(v: *Value) void {
    assertWeak(v.*);
    v.zptr.tagged_value = 0;
}

pub fn isWeakNull(v: Value) bool {
    assertWeak(v);
    return v.zptr.tagged_value == 0;
}

fn tagPtr(ptr: [*]Bucket, tag: Tag) u48 {
    const int: usize = @intFromPtr(ptr);
    const untagged: u48 = @intCast(int);
    return untagged | @intFromEnum(tag);
}

fn untagPtr(tagged: u48) struct { [*]Bucket, Tag } {
    const untagged: u48 = tagged & 0xfffffffffff8;
    const ptr: [*]Bucket = @ptrFromInt(untagged);
    const int: u3 = @truncate(tagged);
    const tag: Tag = @enumFromInt(int);
    return .{ ptr, tag };
}

pub const Tag = enum(u3) {
    /// 0. Strings / Symbols
    string,
    /// 1. Bignums / Ratnums
    number,
    /// 2. Pairs ([2]Value)
    pair,
    /// 3. Collections: Vector, table, etc.
    coll,
    /// 4. OOP: Classes, instances, etc.
    oop,
    /// 5. String buffers
    text,
    /// 6. Procedures
    proc,
    /// 7. Others
    other,
};

// Zisp API

pub fn predForeign(v: Value) Value {
    return value.boole.pack(checkForeign(v));
}

pub fn makeWeak(v: Value) Value {
    assertStrong(v);
    var copy = v;
    copy.zptr.is_weak = true;
    return copy;
}

pub fn predWeak(v: Value) Value {
    return value.boole.pack(checkWeak(v));
}

pub fn predWeakNull(v: Value) Value {
    return value.boole.pack(isWeakNull(v));
}

pub fn getWeak(v: Value) Value {
    if (isWeakNull(v)) {
        return value.boole.f;
    } else {
        var copy = v;
        copy.zptr.is_weak = false;
        return copy;
    }
}
