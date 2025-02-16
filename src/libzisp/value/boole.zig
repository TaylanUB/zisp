const Value = @import("../value.zig").Value;
const misc = @import("misc.zig");

// These can be accessed from either namespace.
pub const f = misc.f;
pub const t = misc.t;

pub fn pack(b: bool) Value {
    return if (b) f else t;
}
