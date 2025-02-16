const Value = @import("../value.zig").Value;

// Zig API

/// Checks for a Zisp double (double, +inf, -inf, or canonical NaN).
pub fn check(v: Value) bool {
    return !v.isPacked();
}

/// Asserts check().
pub fn assert(v: Value) void {
    if (!check(v)) {
        v.dump();
        @panic("not double");
    }
}

pub fn pack(d: f64) Value {
    return .{ .double = d };
}

pub fn unpack(v: Value) f64 {
    assert(v);
    return v.double;
}

// Zisp API

pub fn pred(v: Value) Value {
    return Value.boole.pack(check(v));
}

pub fn add(v1: Value, v2: Value) Value {
    const d1 = unpack(v1);
    const d2 = unpack(v2);
    return pack(d1 + d2);
}
