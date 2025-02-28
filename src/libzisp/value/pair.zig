const std = @import("std");
const value = @import("../value.zig");
const gc = @import("../gc.zig");

const ptr = @import("ptr.zig");

const Value = value.Value;

// Zig API

pub fn check(v: Value) bool {
    return ptr.checkZispTag(v, .pair);
}

pub fn assert(v: Value) void {
    if (!check(v)) {
        v.dump();
        @panic("not pair");
    }
}

// Zisp API

pub fn pred(v: Value) Value {
    return value.boole.pack(check(v));
}

pub fn cons(v1: Value, v2: Value) Value {
    const mem = gc.alloc(2);
    mem[0] = .{ .value = v1 };
    mem[1] = .{ .value = v2 };
    return ptr.pack(mem.ptr, .pair);
}

fn getMem(v: Value) *[2]Value {
    return @ptrCast(ptr.unpack(v).@"0");
}

pub fn car(v: Value) Value {
    assert(v);
    return getMem(v)[0];
}

pub fn cdr(v: Value) Value {
    assert(v);
    return getMem(v)[1];
}

pub fn setcar(v: Value, new: Value) void {
    assert(v);
    getMem(v)[0] = new;
}

pub fn setcdr(v: Value, new: Value) void {
    assert(v);
    getMem(v)[1] = new;
}
