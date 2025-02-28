const value = @import("../value.zig");

const Value = value.Value;

pub const eof = Value{ .misc = .{ .value = .eof } };

// Zig API

pub fn check(v: Value) bool {
    return v.bits == eof.bits;
}

pub fn assert(v: Value) void {
    if (!check(v)) {
        v.dump();
        @panic("not bool");
    }
}

// Zisp API

pub fn get() Value {
    return eof;
}

pub fn pred(v: Value) Value {
    return value.boole.pack(check(v));
}
