const std = @import("std");

const value = @import("../value.zig");

const Value = value.Value;

pub fn unparse(v: Value) []u8 {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    var out: std.ArrayList(u8) = .init(gpa.allocator());
    if (value.rune.check(v)) {
        const name, const len = value.rune.unpack(v);
        out.append('#') catch @panic("");
        out.appendSlice(name[0..len]) catch @panic("");
    }
    return out.toOwnedSlice() catch @panic("");
}
