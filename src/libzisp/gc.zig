const std = @import("std");

const Value = @import("value.zig").Value;

var _gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
const gpa = _gpa.allocator();

pub const Bucket = packed union {
    bits: u64,
    value: Value,
};

pub fn alloc(count: usize) []Bucket {
    return gpa.alloc(Bucket, count) catch @panic("OOM");
}
