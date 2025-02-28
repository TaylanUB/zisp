const std = @import("std");

const value = @import("../value.zig");

const ShortString = value.ShortString;
const OtherTag = value.OtherTag;
const Value = value.Value;
const Hval = value.Hval;

pub fn unparse(w: anytype, v: Value) anyerror!void {
    try if (value.double.check(v))
        unparseDouble(w, v)
    else if (value.fixnum.check(v))
        unparseFixnum(w, v)
    else if (value.ptr.checkZisp(v))
        unparseHeap(w, v)
    else
        unparseOther(w, v);
}

fn unparseDouble(w: anytype, v: Value) !void {
    _ = w;
    _ = v;
    @panic("not implemented");
}

fn unparseFixnum(w: anytype, v: Value) !void {
    _ = w;
    _ = v;
    @panic("not implemented");
}

fn unparseHeap(w: anytype, v: Value) !void {
    const p, const t = value.ptr.unpack(v);
    try switch (t) {
        .pair => unparsePair(w, p),
        .istr => @panic("not implemented"),
        .proc => @panic("not implemented"),
    };
}

fn unparseOther(w: anytype, v: Value) !void {
    try switch (v.other.tag) {
        .rune => unparseRune(w, v),
        .sstr => unparseSstr(w, v),
        .qstr => unparseQstr(w, v),
        .char => unparseChar(w, v),
        .misc => unparseMisc(w, v),
    };
}

fn unparseRune(w: anytype, v: Value) !void {
    const name = value.rune.unpack(v);
    try w.writeByte('#');
    try w.writeAll(name.constSlice());
}

fn unparseSstr(w: anytype, v: Value) !void {
    const str = value.sstr.unpack(v);
    try w.writeAll(str.constSlice());
}

fn unparseQstr(w: anytype, v: Value) !void {
    const str = value.sstr.unpack(v);
    try w.writeByte('"');
    try w.writeAll(str.constSlice());
    try w.writeByte('"');
}

fn unparseChar(w: anytype, v: Value) !void {
    var buf: [4]u8 = undefined;
    const len = try std.unicode.utf8Encode(v.char.value, &buf);
    try w.writeAll(buf[0..len]);
}

fn unparseMisc(w: anytype, v: Value) !void {
    try switch (v.misc.value) {
        .f => w.writeAll("#f"),
        .t => w.writeAll("#t"),
        .nil => w.writeAll("()"),
        .eof => w.writeAll("#eof"),
        .undef => w.writeAll("#undef"),
    };
}

fn unparsePair(w: anytype, p: [*]Hval) !void {
    const vs: *[2]Value = @ptrCast(p);
    try w.writeByte('(');
    try unparse(w, vs[0]);
    var cdr = vs[1];
    while (value.pair.check(cdr)) : (cdr = value.pair.cdr(cdr)) {
        try w.writeByte(' ');
        try unparse(w, value.pair.car(cdr));
    }
    if (!value.nil.check(cdr)) {
        try w.writeByte(' ');
        try w.writeByte('.');
        try w.writeByte(' ');
        try unparse(w, cdr);
    }
    try w.writeByte(')');
}
