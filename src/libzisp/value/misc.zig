const Value = @import("../value.zig").Value;

pub const f = Value{ .misc = .{ .value = 0 } };
pub const t = Value{ .misc = .{ .value = 1 } };
pub const nil = Value{ .misc = .{ .value = 2 } };
pub const eof = Value{ .misc = .{ .value = 3 } };
