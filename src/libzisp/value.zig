//
// === NaN Packing Strategy ===
//
// Format of a double, in Zig least-to-most significant field order:
//
//   { fraction: u52, exponent: u11, sign: u1 }
//
// When the exponent bits are all set, it's either a NaN or an Infinity.
//
// For value packing, almost all remaining 53 bits are available, giving us
// about 2^53 values, except for the four following bit patterns:
//
//   *** FORBIDDEN VALUES ***
//
//   1. Negative cqNaN    = { sign = 1, exponent = max, fraction = 2^51 }
//
//   2. Negative Infinity = { sign = 1, exponent = max, fraction =    0 }
//
//   3. Positive cqNaN    = { sign = 0, exponent = max, fraction = 2^51 }
//
//   4. Positive Infinity = { sign = 0, exponent = max, fraction =    0 }
//
// The abbreviation "cqNaN" stands for canonical quiet NaN.
//
// Note that 2^51 means the MSb of the 52 fraction bits being set, and the rest
// being zero.  The fraction MSb is also called the is_quiet flag, because it
// demarcates quiet NaNs.  The rest being zero makes it the canonical qNaN.
//
// The positive and negative cqNaN are the *only* NaN values that can actually
// be returned by any FP operations, which is why we don't use them to pack
// values; we want to be able to represent NaN in Zisp as a double.
//
// Beyond those four bit patterns, all values with a maximum exponent (all bits
// set) are fair game for representing other values, so 2^53 - 4 possibilities.
//
// We split those 2^53 - 4 available values into four groups, each allowing for
// 2^51 - 1 different values to be encoded in them:
//
//   sign = 1, quiet = 1 :: Negative Fixnum from -1 to -2^51+1
//
//   sign = 1, quiet = 0 :: Positive Fixnum from 0 to 2^51-2
//
//   sign = 0, quiet = 1 :: Pointers
//
//   sign = 0, quiet = 0 :: Others
//
//
// === Fixnums ===
//
// Negative fixnums actually represent themselves without needing to go through
// any transformation.  Only the smallest 52-bit signed negative, -2^51, cannot
// be represented, as it would step on forbidden value 1, Negative cqNaN.
//
// Positive fixnums go through bitsiwe NOT (implemented via an XOR mask here to
// make it one operation together with the NaN masking) to avoid the all-zero
// payload value, which would step on forbidden value 2, Negative Infinity.
//
//
// === Pointers ===
//
// Pointers are further subdivided as follows based on the remaining 51 bits,
// with the first three bits used as a sort of tag:
//
//   000   :: Pointer to Zisp heap object (string, vector, etc.)
//
//   001   :: Weak pointer to Zisp heap object
//
//   01.   :: Undefined (may be used by GC to flag pointers for some reason?)
//
//   1..   :: Foreign pointer (basically, a 50-bit fixnum of another type)
//
// This means pointers to the Zisp heap are 48 bits.  Of those, we only really
// need 45, since 64-bit platforms are in practice limited to 48-bit addresses,
// and allocations happen at 8-byte boundaries, meaning the least significant 3
// bits are always unset.  Thus, we are able to store yet another 3-bit tag in
// those 48-bit pointers alongside the actual, multiple-of-8, 48-bit address.
//
// The forbidden value 3, Positive cqNaN, is avoided thanks to the fact that a
// regular Zisp heap pointer can never be null.  Weak pointers, which can be
// null, avoid stepping on that forbidden value thanks to bit 49 being set.
//
// Foreign pointers allow storing arbitrary pointers, or integers basically, of
// up to 50 bits, without any further definition in Zisp of what they mean.
//
//
// === Other values ===
//
// This 51-bit range is divided as follows, based on the high bits:
//
//   000   :: Rune
//
//   001   :: Short string
//
//   010   :: Short string literal
//
//   011   :: Unicode code point
//
//   100   :: Singleton values
//
//   101, 110, 111  :: Undefined
//
// Runes are symbols of 1 to 6 ASCII characters used to implement reader syntax.
//
// Zisp strings are immutable.  Any string fitting into 6 bytes or less will be
// stored as an immediate value, not requiring any heap allocation or interning.
// It's implicitly interned, so to speak.  This includes the empty string.
//
// The null byte serves as a terminator for strings shorter than 6 bytes, and
// therefore cannot appear in these strings; a string that short but actually
// containing a null byte will need to be heap allocated like other strings.
//
// There may also be strings that are this short, but ended up on the heap due
// to being uninterned.  Interning them will return the equivalent short string
// as an immediate.
//
// The separate type for a short string *literal* is for an efficiency hack in
// the parser; see commentary there.
//
// Unicode code points need a maximum of 21 bits, yet we have 48 available.
// This may be exploited for a future extension.
//
// Similarly, it's very unlikely that we will ever need more than a handful of
// singleton values (false, true, nil, and so on).  As such, this range of bit
// patterns may be subdivided in the future.  Right now, only the lowest 8 bits
// are allowed to be set, with the other 40 being reserved, so there's a limit
// of 256 singleton values that can be defined.
//
// And top of that, we have three more 48-bit value ranges that are unused!
//
// The forbidden value 4, Positive Infinity, would be the "empty string rune"
// but that isn't allowed anyway, so all is fine.
//

// Here's the original article explaining the strategy:
//
//   https://tkammer.de/zisp/notes/nan.html
//
// More about runes:
//
//   https://tkammer.de/zisp/notes/symbols.html
//
// Note: Packed structs are least-to-most significant, so the order of fields
// must be reversed relative to a typical big-endian illustration of the bit
// patterns of IEEE 754 double-precision floating point numbers.

const std = @import("std");

pub const double = @import("value/double.zig");
pub const fixnum = @import("value/fixnum.zig");

pub const ptr = @import("value/ptr.zig");

pub const rune = @import("value/rune.zig");
pub const sstr = @import("value/sstr.zig");
pub const char = @import("value/char.zig");
pub const boole = @import("value/boole.zig");
pub const nil = @import("value/nil.zig");
pub const eof = @import("value/eof.zig");

pub const pair = @import("value/pair.zig");

// To fill up the u11 exponent part of a NaN.
const FILL = 0x7ff;

// Used when dealing with runes and short strings.
pub const ShortString = std.BoundedArray(u8, 6);

pub const OtherTag = enum(u3) { rune, sstr, qstr, char, misc };

pub const MiscValue = enum(u8) { f, t, nil, eof, undef = 255 };

/// Represents a Zisp value/object.
pub const Value = packed union {
    /// To get the value as a regular double.
    double: f64,

    /// To get an agnostic value for direct comparison with == i.e. eq?.
    bits: u64,

    // Some of the structs below are just for inspection, whereas others are to
    // initialize a new value of that category as well as read it that way.

    /// Inspection through the lens of the general IEEE 754 double layout.
    ieee: packed struct {
        rest: u51,
        quiet: bool,
        exp: u11,
        sign: bool,
    },

    /// For initializing and reading fixnums.
    fixnum: packed struct {
        code: u51,
        negative: bool,
        _: u11 = FILL,
        _is_fixnum: bool = true,
    },

    /// Inspection through the lens of the ptr category.
    ptr: packed struct {
        _value: u48,
        is_weak: bool,
        _unused: bool,
        is_foreign: bool,
        _is_ptr: bool,
        _: u11,
        _is_fixnum: bool,
    },

    /// For initializing and reading foreign pointers.
    fptr: packed struct {
        value: u50,
        _is_foreign: bool = true,
        _is_ptr: bool = true,
        _: u11 = FILL,
        _is_fixnum: bool = false,
    },

    /// For initializing and reading Zisp heap pointers.
    zptr: packed struct {
        tagged_value: u48,
        is_weak: bool = false,
        _unused: bool = false,
        _is_foreign: bool = false,
        _is_ptr: bool = true,
        _: u11 = FILL,
        _is_fixnum: bool = false,
    },

    /// Inspection as an other (non-fixnum, non-pointer) packed value.
    other: packed struct {
        _value: u48,
        tag: OtherTag,
        _is_ptr: bool,
        _: u11,
        _is_ifxnum: bool,
    },

    /// For initializing and reading runes.
    rune: packed struct {
        // actually [6]u8 but packed struct cannot contain arrays
        name: u48,
        _tag: OtherTag = .rune,
        _is_ptr: bool = false,
        _: u11 = FILL,
        _is_fixnum: bool = false,
    },

    /// For initializing and reading short strings.
    sstr: packed struct {
        // actually [6]u8 but packed struct cannot contain arrays
        string: u48,
        tag: OtherTag,
        _is_ptr: bool = false,
        _: u11 = FILL,
        _is_fixnum: bool = false,
    },

    /// For initializing and reading characters.
    char: packed struct {
        value: u21,
        _reserved: u27 = 0,
        _tag: OtherTag = .char,
        _is_ptr: bool = false,
        _: u11 = FILL,
        _is_fixnum: bool = false,
    },

    /// For initializing and reading misc values aka singletons.
    misc: packed struct {
        value: MiscValue,
        _reserved: u40 = 0,
        _tag: OtherTag = .misc,
        _is_ptr: bool = false,
        _: u11 = FILL,
        _is_fixnum: bool = false,
    },

    /// Hexdumps the value.
    pub inline fn dump(v: Value) void {
        std.debug.dumpHex(std.mem.asBytes(&v));
    }

    // The following aren't type predicates per se, but rather determine which
    // general category the value is in.  The exceptions are fixnum and double,
    // since those aren't sub-categorized into further types.

    /// Checks for a Zisp double, including: +nan.0, -nan.0, +inf.0, -inf.0
    pub inline fn isDouble(v: Value) bool {
        return v.ieee.exp != FILL or v.ieee.rest == 0;
    }

    /// Checks for a non-double Zisp value packed into a NaN.
    pub inline fn isPacked(v: Value) bool {
        return !v.isDouble();
    }

    /// Checks for a fixnum.
    pub inline fn isFixnum(v: Value) bool {
        return v.isPacked() and v.ieee.sign;
    }

    /// Checks for any kind of pointer.
    pub inline fn isPtr(v: Value) bool {
        return v.isPacked() and !v.ieee.sign and v.ieee.quiet;
    }

    /// Checks for a non-double, non-fixnum, non-pointer Zisp value.
    pub inline fn isOther(v: Value) bool {
        return v.isPacked() and !v.ieee.sign and !v.ieee.quiet;
    }

    /// Checks for an other type of value based on tag.
    pub inline fn isOtherTag(v: Value, tag: OtherTag) bool {
        return v.isOther() and v.other.tag == tag;
    }
};

/// A "heap value" that could be a Value or object header.
pub const Hval = packed union {
    value: Value,
};
