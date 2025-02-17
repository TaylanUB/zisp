const Value = @import("../value.zig").Value;
const misc = @import("misc.zig");

pub const nil = misc.nil;

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
    return if (check(v)) misc.t else misc.f;
}
