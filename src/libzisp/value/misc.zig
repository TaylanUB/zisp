const Value = @import("../value.zig").Value;

pub const f = Value{ .misc = .{0} };
pub const t = Value{ .misc = .{1} };
pub const nil = Value{ .misc = .{2} };
pub const eof = Value{ .misc = .{3} };
