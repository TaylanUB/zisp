const std = @import("std");

const Value = @import("../value.zig").Value;

// Zig API

pub fn check(v: Value) bool {
    return v.isPacked() and v.ptr.is_ptr;
}

pub fn assert(v: Value) void {
    if (!check(v)) {
        v.dump();
        @panic("not a pointer");
    }
}

// Foreign Pointers

pub fn checkForeign(v: Value) bool {
    return check(v) and v.ptr.foreign;
}

pub fn assertForeign(v: Value) void {
    if (!checkForeign(v)) {
        v.dump();
        @panic("not foreign pointer");
    }
}

pub fn packForeign(int: u50) Value {
    return .{ .fptr = .{int} };
}

pub fn unpackForeign(v: Value) u64 {
    assertForeign(v);
    return v.ptr.value.foreign;
}

// Zisp Pointers

pub fn checkZisp(v: Value) bool {
    return check(v) and !v.ptr.foreign;
}

pub fn assertZisp(v: Value) void {
    if (!checkZisp(v)) {
        v.dump();
        @panic("not zisp pointer");
    }
}

pub fn checkWeak(v: Value) bool {
    return checkZisp(v) and v.ptr.weak;
}

pub fn assertWeak(v: Value) void {
    if (!checkWeak(v)) {
        v.dump();
        @panic("not weak zisp pointer");
    }
}

pub fn checkNormal(v: Value) bool {
    return checkZisp(v) and !v.ptr.weak;
}

pub fn assertNormal(v: Value) void {
    if (!checkNormal(v)) {
        v.dump();
        @panic("not normal zisp pointer");
    }
}

pub fn packZisp(ptr: *anyopaque, tag: Tag, weak: bool) Value {
    return .{ .ptr = .{
        .value = tagPtr(ptr, tag),
        .weak = weak,
    } };
}

pub fn pack(ptr: *anyopaque, tag: Tag) Value {
    return packZisp(ptr, tag, false);
}

pub fn packWeak(ptr: *anyopaque, tag: Tag) Value {
    return packZisp(ptr, tag, true);
}

// Unpacks weak as well; no need for a separate fn.
pub fn unpack(v: Value) struct { ptr: *anyopaque, tag: Tag } {
    assertZisp(v);
    return untagPtr(v.ptr.value.tagged);
}

// Weak pointers may be null.
pub fn isNull(v: Value) bool {
    assertWeak(v);
    const ptr, _ = untagPtr(v.ptr.value.tagged);
    return @intFromPtr(ptr) == 0;
}

pub fn tagPtr(ptr: *anyopaque, tag: Tag) u49 {
    const int: u64 = @intFromPtr(ptr);
    const untagged: u49 = @truncate(int);
    return untagged << 1 | @intFromEnum(tag);
}

pub fn untagPtr(tagged: 49) struct { ptr: *anyopaque, tag: Tag } {
    const untagged: u49 = tagged >> 1 & 0xfffffffffff0;
    const ptr: *anyopaque = @ptrFromInt(untagged);
    const int: u4 = @truncate(tagged);
    const tag: Tag = @enumFromInt(int);
    return .{ .ptr = ptr, .tag = tag };
}

pub const Tag = enum(u4) {
    /// 1. Strings / Symbols
    string,
    /// 2. Bignums / Ratnums
    number,
    /// 3. Pairs ([2]Value)
    pair,
    /// 4. Vector, bytevector, etc.
    array,
    /// 5. Ordered hash table
    table,
    /// 6. String buffer
    text,
    /// 7. Class, interface, etc.
    role,
    /// 8. Instance, basically
    actor,
    /// 9. I/O Port
    port,
    /// 10. Procedure
    proc,
    /// 11. Continuation
    cont,
    /// Other
    other = 15,
};

// Zisp API

pub fn predForeign(v: Value) Value {
    return Value.boole.pack(checkForeign(v));
}

pub fn makeWeak(v: Value) Value {
    assertNormal(v);
    var copy = v;
    copy.ptr.weak = true;
    return copy;
}

pub fn predWeak(v: Value) Value {
    const isWeak = checkWeak(v);
    return Value.boole.pack(isWeak);
}

pub fn predWeakNull(v: Value) Value {
    assertWeak(v);
    return Value.boole.pack(v.ptr.weak);
}

pub fn getWeak(v: Value) Value {
    assertWeak(v);
    if (isNull(v)) {
        return Value.boole.pack(false);
    } else {
        var copy = v;
        copy.ptr.weak = false;
        return copy;
    }
}
