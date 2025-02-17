//! By convention, root.zig is the root source file when making a library. If
//! you are making an executable, the convention is to delete this file and
//! start with main.zig instead.
const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;

pub const value = @import("libzisp/value.zig");

test "double" {
    const d1: f64 = 0.123456789;
    const d2: f64 = -0.987654321;
    const v1 = value.double.pack(d1);
    const v2 = value.double.pack(d2);
    const v3 = value.double.add(v1, v2);
    const result = value.double.unpack(v3);

    try std.testing.expect(value.double.check(v1));
    try std.testing.expect(value.double.check(v2));
    try std.testing.expect(value.double.check(v3));
    try std.testing.expectEqual(d1 + d2, result);
}

test "fixnum" {
    const int1: i64 = 123456789;
    const int2: i64 = -987654321;
    const v1 = value.fixnum.pack(int1);
    const v2 = value.fixnum.pack(int2);
    const v3 = value.fixnum.add(v1, v2);
    const result = value.fixnum.unpack(v3);

    try std.testing.expect(value.fixnum.check(v1));
    try std.testing.expect(value.fixnum.check(v2));
    try std.testing.expect(value.fixnum.check(v3));
    try std.testing.expectEqual(int1 + int2, result);
}

test "ptr" {
    const p: *anyopaque = @ptrFromInt(256);

    const p1 = value.ptr.pack(p, value.ptr.Tag.string);
    try std.testing.expect(value.ptr.check(p1));
    try std.testing.expect(value.ptr.checkZisp(p1));
    try std.testing.expect(value.ptr.checkNormal(p1));
    const p1v, const p1t = value.ptr.unpack(p1);
    try std.testing.expectEqual(p, p1v);
    try std.testing.expectEqual(value.ptr.Tag.string, p1t);

    const p2 = value.ptr.makeWeak(p1);
    try std.testing.expect(value.ptr.check(p2));
    try std.testing.expect(value.ptr.checkZisp(p2));
    try std.testing.expect(value.ptr.checkWeak(p2));
    const p2v, const p2t = value.ptr.unpack(p1);
    try std.testing.expectEqual(p, p2v);
    try std.testing.expectEqual(value.ptr.Tag.string, p2t);
}

test "sstr" {
    const impls = .{
        .{ value.sstr.pack, value.sstr.unpack },
        // .{ value.sstr.pack1, value.sstr.unpack1 },
        // .{ value.sstr.pack2, value.sstr.unpack2 },
        // .{ value.sstr.pack3, value.sstr.unpack3 },
        // .{ value.sstr.pack4, value.sstr.unpack4 },
    };

    inline for (impls, 0..) |impl, i| {
        const pack, const unpack = impl;

        const ss1 = pack("1");
        const ss2 = pack("123");
        const ss3 = pack("123456");

        const s1, const l1 = unpack(ss1);
        const s2, const l2 = unpack(ss2);
        const s3, const l3 = unpack(ss3);

        try std.testing.expect(value.sstr.check(ss1));
        try std.testing.expect(value.sstr.check(ss2));
        try std.testing.expect(value.sstr.check(ss3));

        try std.testing.expectEqual(1, l1);
        try std.testing.expectEqualStrings("1", s1[0..l1]);

        try std.testing.expectEqual(3, l2);
        try std.testing.expectEqualStrings("123", s2[0..l2]);

        try std.testing.expectEqual(6, l3);
        try std.testing.expectEqualStrings("123456", s3[0..l3]);

        var timer = try std.time.Timer.start();
        var ns: f64 = undefined;
        var secs: f64 = undefined;

        const iters = 1;
        if (iters > 1) {
            for (0..iters) |_i| {
                _ = _i;
                std.mem.doNotOptimizeAway(pack("1"));
                std.mem.doNotOptimizeAway(pack("123"));
                std.mem.doNotOptimizeAway(pack("123456"));
            }

            ns = @floatFromInt(timer.lap());
            secs = ns / 1_000_000_000;

            std.debug.print("pack{}: {d:.3}s\t", .{ i, secs });

            for (0..iters) |_i| {
                _ = _i;
                std.mem.doNotOptimizeAway(unpack(ss1));
                std.mem.doNotOptimizeAway(unpack(ss2));
                std.mem.doNotOptimizeAway(unpack(ss3));
            }

            ns = @floatFromInt(timer.lap());
            secs = ns / 1_000_000_000;

            std.debug.print("unpack{}: {d:.3}s\n", .{ i, secs });
        }
    }
}

test "char" {
    const c1 = value.char.pack('\x00');
    try std.testing.expect(value.char.check(c1));
    try std.testing.expectEqual('\x00', value.char.unpack(c1));

    const c2 = value.char.pack('😀');
    try std.testing.expect(value.char.check(c2));
    try std.testing.expectEqual('😀', value.char.unpack(c2));
}

test "misc" {
    const f = value.boole.pack(false);
    try std.testing.expect(value.boole.check(f));
    try std.testing.expectEqual(false, value.boole.unpack(f));
    try std.testing.expect(value.boole.unpack(value.boole.pred(f)));

    const t = value.boole.pack(true);
    try std.testing.expect(value.boole.check(t));
    try std.testing.expectEqual(true, value.boole.unpack(t));
    try std.testing.expect(value.boole.unpack(value.boole.pred(t)));

    const nil = value.nil.get();
    try std.testing.expect(value.nil.check(nil));
    try std.testing.expect(value.boole.unpack(value.nil.pred(nil)));

    const eof = value.eof.get();
    try std.testing.expect(value.eof.check(eof));
    try std.testing.expect(value.boole.unpack(value.eof.pred(eof)));
}
