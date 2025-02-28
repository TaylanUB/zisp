const std = @import("std");

const value = @import("value.zig");

const Hval = value.Hval;

var _gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
const gpa = _gpa.allocator();

pub fn alloc(count: usize) []Hval {
    return gpa.alloc(Hval, count) catch @panic("OOM");
}
