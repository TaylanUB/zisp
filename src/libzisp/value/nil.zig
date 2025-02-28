const value = @import("../value.zig");

const Value = value.Value;

pub const nil = Value{ .misc = .{ .value = .nil } };

// Zig API

pub fn check(v: Value) bool {
    return v.bits == nil.bits;
}

pub fn assert(v: Value) void {
    if (!check(v)) {
        v.dump();
        @panic("not bool");
    }
}

// Zisp API

pub fn get() Value {
    return nil;
}

pub fn pred(v: Value) Value {
    return value.boole.pack(check(v));
}
