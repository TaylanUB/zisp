//! By convention, root.zig is the root source file when making a library. If
//! you are making an executable, the convention is to delete this file and
//! start with main.zig instead.
const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;

pub const gc = @import("libzisp/gc.zig");
pub const parser = @import("libzisp/parser.zig");
pub const value = @import("libzisp/value.zig");

pub const Value = value.Value;
pub const Bucket = gc.Bucket;

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
    const ptr = value.ptr;

    const val: [*]Bucket = @ptrFromInt(256);
    const tag = ptr.Tag.string;

    const p = ptr.pack(val, tag);
    try std.testing.expect(ptr.check(p));
    try std.testing.expect(ptr.checkZisp(p, tag));
    try std.testing.expect(ptr.checkStrong(p));

    const pv, const pt = ptr.unpack(p);
    try std.testing.expectEqual(val, pv);
    try std.testing.expectEqual(tag, pt);

    var w = ptr.makeWeak(p);
    try std.testing.expect(ptr.check(w));
    try std.testing.expect(ptr.checkZisp(w, tag));
    try std.testing.expect(ptr.checkWeak(w));
    try std.testing.expectEqual(true, value.boole.unpack(ptr.predWeak(w)));
    try std.testing.expectEqual(false, value.boole.unpack(ptr.predWeakNull(w)));

    const wv, const wt = ptr.unpack(w);
    try std.testing.expectEqual(val, wv);
    try std.testing.expectEqual(tag, wt);

    const wv2, const wt2 = ptr.unpack(ptr.getWeak(w));
    try std.testing.expectEqual(val, wv2);
    try std.testing.expectEqual(tag, wt2);

    ptr.setWeakNull(&w);
    try std.testing.expect(ptr.check(w));
    try std.testing.expect(ptr.checkWeak(w));
    try std.testing.expect(ptr.isWeakNull(w));
    try std.testing.expectEqual(true, value.boole.unpack(ptr.predWeak(w)));
    try std.testing.expectEqual(true, value.boole.unpack(ptr.predWeakNull(w)));
    try std.testing.expectEqual(false, value.boole.unpack(ptr.getWeak(w)));
}

test "fptr" {
    const ptr = value.ptr;

    const int1: u50 = 0;
    const int2: u50 = std.math.maxInt(u50);

    const f1 = ptr.packForeign(int1);
    try std.testing.expect(ptr.checkForeign(f1));
    try std.testing.expectEqual(int1, ptr.unpackForeign(f1));

    const f2 = ptr.packForeign(int2);
    try std.testing.expect(ptr.checkForeign(f2));
    try std.testing.expectEqual(int2, ptr.unpackForeign(f2));
}

test "rune" {
    const r1 = value.rune.pack("test");
    try std.testing.expect(value.rune.check(r1));

    const s1, const l1 = value.rune.unpack(r1);
    try std.testing.expectEqualStrings("test", s1[0..l1]);
}

const SstrImpl = struct { SstrPack, SstrUnpack };
const SstrPack = *const fn ([]const u8) Value;
const SstrUnpack = *const fn (Value) struct { [6]u8, u3 };

test "sstr" {
    const impls = [_]SstrImpl{
        .{ value.sstr.pack, value.sstr.unpack },
        // .{ value.sstr.pack1, value.sstr.unpack1 },
        // .{ value.sstr.pack2, value.sstr.unpack2 },
        // .{ value.sstr.pack3, value.sstr.unpack3 },
        // .{ value.sstr.pack4, value.sstr.unpack4 },
    };

    for (impls) |impl| {
        try testSstr(impl);
    }

    if (impls.len > 1) {
        const iters = switch (@import("builtin").mode) {
            .Debug, .ReleaseSmall => 10_000_000,
            .ReleaseSafe => 100_000_000,
            .ReleaseFast => 1_000_000_000,
        };
        std.debug.print("Benchmarking with {} iters.\n", .{iters});
        inline for (impls, 0..) |impl, i| {
            try benchmarkSstr(impl, i, iters);
        }
    }
}

fn testSstr(impl: SstrImpl) !void {
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
}

fn benchmarkSstr(impl: SstrImpl, id: usize, iters: usize) !void {
    const pack, const unpack = impl;

    var timer = try std.time.Timer.start();
    var ns: f64 = undefined;
    var secs: f64 = undefined;

    var ss1: Value = undefined;
    var ss2: Value = undefined;
    var ss3: Value = undefined;

    for (0..iters) |_i| {
        _ = _i;
        ss1 = pack("1");
        ss2 = pack("123");
        ss3 = pack("123456");
    }

    ns = @floatFromInt(timer.lap());
    secs = ns / 1_000_000_000;

    std.debug.print("pack{}: {d:.3}s\t", .{ id, secs });

    for (0..iters) |_i| {
        _ = _i;
        std.mem.doNotOptimizeAway(unpack(ss1));
        std.mem.doNotOptimizeAway(unpack(ss2));
        std.mem.doNotOptimizeAway(unpack(ss3));
    }

    ns = @floatFromInt(timer.lap());
    secs = ns / 1_000_000_000;

    std.debug.print("unpack{}: {d:.3}s\n", .{ id, secs });
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

test "pair" {
    const v1 = value.fixnum.pack(1);
    const v2 = value.fixnum.pack(2);

    const v3 = value.fixnum.pack(3);
    const v4 = value.fixnum.pack(4);

    const p = value.pair.cons(v1, v2);
    try std.testing.expect(value.pair.check(p));
    try std.testing.expect(value.boole.unpack(value.pair.pred(p)));

    const car = value.pair.car(p);
    const cdr = value.pair.cdr(p);
    try std.testing.expectEqual(1, value.fixnum.unpack(car));
    try std.testing.expectEqual(2, value.fixnum.unpack(cdr));

    value.pair.setcar(p, v3);
    value.pair.setcdr(p, v4);

    const car2 = value.pair.car(p);
    const cdr2 = value.pair.cdr(p);
    try std.testing.expectEqual(3, value.fixnum.unpack(car2));
    try std.testing.expectEqual(4, value.fixnum.unpack(cdr2));
}

test "parse" {
    const val = parser.parse("\"foo\"");
    const r, const rl = value.rune.unpack(value.pair.car(val));
    const s, const sl = value.sstr.unpack(value.pair.cdr(val));
    try std.testing.expectEqualStrings("QUOTE", r[0..rl]);
    try std.testing.expectEqualStrings("foo", s[0..sl]);
}

test "parse2" {
    const val = parser.parse("#\"foo\"");

    const r, const rl = value.rune.unpack(value.pair.car(val));
    try std.testing.expectEqualStrings("HASH", r[0..rl]);

    const cdr = value.pair.cdr(val);

    const s, const sl = value.rune.unpack(value.pair.car(cdr));
    try std.testing.expectEqualStrings("QUOTE", s[0..sl]);

    const f, const fl = value.sstr.unpack(value.pair.cdr(cdr));
    try std.testing.expectEqualStrings("foo", f[0..fl]);

    _ = parser.parse("(foo \"bar\" [#x #\"baz\"] 'bat)");
}
