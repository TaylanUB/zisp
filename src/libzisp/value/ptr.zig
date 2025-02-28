const std = @import("std");
const value = @import("../value.zig");
const gc = @import("../gc.zig");

const Value = value.Value;
const Hval = value.Hval;

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

pub fn packForeign(int: u50) Value {
    return .{ .fptr = .{ .value = int } };
}

pub fn unpackForeign(v: Value) u50 {
    assertForeign(v);
    return v.fptr.value;
}

// Zisp Pointers

pub fn checkZisp(v: Value) bool {
    return check(v) and !v.ptr.is_foreign;
}

pub fn assertZisp(v: Value) void {
    if (!checkZisp(v)) {
        v.dump();
        @panic("not zisp pointer");
    }
}

pub fn checkWeak(v: Value) bool {
    return checkZisp(v) and v.zptr.is_weak;
}

pub fn assertWeak(v: Value) void {
    if (!checkWeak(v)) {
        v.dump();
        @panic("not zisp weak pointer");
    }
}

pub fn checkZispTag(v: Value, tag: Tag) bool {
    return checkZisp(v) and unpack(v).@"1" == tag;
}

pub fn assertZispTag(v: Value, tag: Tag) void {
    if (!checkZispTag(v, tag)) {
        v.dump();
        @panic("not zisp pointer or wrong tag");
    }
}

pub fn checkStrong(v: Value) bool {
    return checkZisp(v) and !v.zptr.is_weak;
}

pub fn assertStrong(v: Value) void {
    if (!checkStrong(v)) {
        v.dump();
        @panic("not zisp strong pointer");
    }
}

pub fn packZisp(ptr: [*]Hval, tag: Tag, is_weak: bool) Value {
    return .{ .zptr = .{
        .tagged_value = tagPtr(ptr, tag),
        .is_weak = is_weak,
    } };
}

pub fn pack(ptr: [*]Hval, tag: Tag) Value {
    return packZisp(ptr, tag, false);
}

pub fn packWeak(ptr: [*]Hval, tag: Tag) Value {
    return packZisp(ptr, tag, true);
}

// Unpacks weak as well; no need for a separate fn.
pub fn unpack(v: Value) struct { [*]Hval, Tag } {
    assertZisp(v);
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

fn tagPtr(ptr: [*]Hval, tag: Tag) u48 {
    const int: usize = @intFromPtr(ptr);
    const untagged: u48 = @intCast(int);
    return untagged | @intFromEnum(tag);
}

fn untagPtr(tagged: u48) struct { [*]Hval, Tag } {
    const untagged: u48 = tagged & 0xfffffffffff8;
    const ptr: [*]Hval = @ptrFromInt(untagged);
    const int: u3 = @truncate(tagged);
    const tag: Tag = @enumFromInt(int);
    return .{ ptr, tag };
}

pub const Tag = enum(u3) {
    /// *[2]Value
    pair,
    /// Interned string (symbol)
    istr,
    /// Procedure
    proc,
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
