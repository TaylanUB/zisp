const value = @import("../value.zig");

const Value = value.Value;

// Zig API

pub fn check(v: Value) bool {
    return v.isOther(.char);
}

pub fn assert(v: Value) void {
    if (!check(v)) {
        v.dump();
        @panic("not char");
    }
}

pub fn pack(c: u21) Value {
    return .{ .char = .{ .value = c } };
}

pub fn unpack(v: Value) u21 {
    assert(v);
    return @truncate(v.char.value);
}

// Zisp API

pub fn pred(v: Value) Value {
    return value.boole.pack(check(v));
}
