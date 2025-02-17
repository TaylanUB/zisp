const Value = @import("../value.zig").Value;
const misc = @import("misc.zig");

pub const f = misc.f;
pub const t = misc.t;

// Zig API

/// Checks if the value is a boole.
pub fn check(v: Value) bool {
    return v.bits == f.bits or v.bits == t.bits;
}

pub fn assert(v: Value) void {
    if (!check(v)) {
        v.dump();
        @panic("not bool");
    }
}

pub fn pack(b: bool) Value {
    return if (b) t else f;
}

pub fn unpack(v: Value) bool {
    assert(v);
    return v.bits == t.bits;
}

// Zisp API

pub fn pred(v: Value) Value {
    return if (check(v)) t else f;
}
