const Value = @import("../value.zig").Value;

pub fn check(v: Value) bool {
    return v.isPacked() and
        !v.char.fixnum and
        !v.char.ptr and
        v.char.tag == .char;
}

pub fn assert(v: Value) void {
    if (!check(v)) {
        v.dump();
        @panic("not char");
    }
}

pub fn pack(c: u21) Value {
    return .{ .char = .{c} };
}

pub fn unpack(v: Value) u21 {
    assert(v);
    return v.char.value;
}
