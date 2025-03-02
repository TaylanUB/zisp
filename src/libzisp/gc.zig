const std = @import("std");

const value = @import("value.zig");

const Value = value.Value;
const Hval = value.Hval;

var _gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
const gpa = _gpa.allocator();

var cpool = std.heap.MemoryPool([2]Value).init(gpa);

pub fn cons(v1: Value, v2: Value) *[2]Value {
    const mem = cpool.create() catch @panic("OOM");
    mem[0] = v1;
    mem[1] = v2;
    return mem;
}

pub fn alloc(count: usize) []Hval {
    return gpa.alloc(Hval, count) catch @panic("OOM");
}
