const std = @import("std");

const value = @import("../value.zig");

const ShortString = value.ShortString;
const Value = value.Value;

// const State = struct {

// }

pub fn unparse(v: Value) []u8 {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    var out: std.ArrayList(u8) = .init(gpa.allocator());
    if (value.rune.check(v)) {
        const name = value.rune.unpack(v);
        out.append('#') catch @panic("");
        out.appendSlice(name.slice()) catch @panic("");
    }
    return out.toOwnedSlice() catch @panic("");
}
