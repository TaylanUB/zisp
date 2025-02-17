//
// Here's a summary of our packing strategy.
//
// Format of a double, in Zig least-to-most significant field order:
//
//   { sign: u1, exponent: u11, fraction: u52 }
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
// being zero.  Th fraction MSb is also called the is_quiet flag, because it
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
// Pointers are further subdivided as follows based on the remaining 51 bits:
//
//   MSb = 1          :: Foreign Pointer (or a "special 50-bit fixnum")
//
//   MSb = 0, SSb = 0 :: Pointer to heap object (string, vector, etc.)
//
//   MSb = 0, SSb = 1 :: Weak pointer to heap object
//
//   (SSb = Second-most significant bit)
//
// This means regular pointers to the Zisp heap are 49 bits.  Of these, we only
// really need 45, since 64-bit platforms are in practice limited to 48-bit
// addresses, and allocations happen at 8-byte boundaries, meaning the least
// significant 3 bit are always 0.  Thus, we are able to store 4-bit tags in
// those 49-bit pointers alongside the actual, multiple-of-8, 48-bit address.
//
// Note that foreign pointers avoid stepping on any forbidden value, thanks to
// bit 51 being set.
//
// The forbidden value 3, Positive cqNaN, is avoided thanks to the fact that a
// regular Zisp heap pointer can never be null.  Weak pointers, which can be
// null, avoid stepping on that forbidden value thanks to bit 50 being set.
//
//
// === Other values ===
//
// This 51-bit range is divided as follows, based on the initial bits:
//
//   000   :: Undefined
//
//   001   :: Small string
//
//   010   :: Unicode code point
//
//   011   :: Singleton values
//
//   1..   :: Undefined
//
// Zisp strings are immutable and always encoded in UTF-8.  Any string fitting
// into 6 bytes or less will be stored as an immediate value, not requiring any
// heap allocation or interning.  (It's implicitly interned.)
//
// The null byte serves as a terminator and cannot appear in these strings; a
// string that short but actually containing a null byte will need to be heap
// allocated like other strings.
//
// There may also be uninterned strings on the heap that are also as short but
// ended up on the heap due to being uninterned.  Calling intern on them will
// return the equivalent small string.
//
// Unicode code points need a maximum of 21 bits, yet we have 48 available.
// This may be exploited for a future extension.
//
// Similarly, it's extremely unlikely that we will ever need more than a few
// dozen singleton values (false, true, null, and so on).  As such, this range
// of bit patterns may be subdivided further in the future.
//
// And on top of all that we still have a 48-bit and a 50-bit range left!
//
// The forbidden value 4, Positive Infinity, is in the 48-bit undefined value
// range starting with the 000 tag.
//

// Here's the original article explaining the strategy:
//
// https://tkammer.de/zisp/notes/nan.html
//
// Note: Packed structs are least-to-most significant, so the order of fields
// must be reversed relative to a typical big-endian illustration of the bit
// patterns of IEEE 754 double-precision floating point numbers.

const std = @import("std");

pub const double = @import("value/double.zig");
pub const fixnum = @import("value/fixnum.zig");

pub const ptr = @import("value/ptr.zig");

pub const sstr = @import("value/sstr.zig");
pub const char = @import("value/char.zig");
pub const boole = @import("value/boole.zig");
pub const nil = @import("value/nil.zig");
pub const eof = @import("value/eof.zig");

/// To fill up the u11 exponent part of a NaN.
const FILL = 0x7ff;

/// Represents a Zisp value/object.
pub const Value = packed union {
    double: f64,
    bits: u64,

    nan: packed struct {
        rest: u51,
        quiet: u1,
        exp: u11 = FILL,
        sign: u1,
    },

    fixnum: packed struct {
        code: u51,
        negative: bool,
        _: u11 = FILL,
        is_fixnum: bool = true,
    },

    ptr: packed struct {
        // if foreign, we don't actually use value and is_weak
        value: u49,
        weak: bool = false,
        foreign: bool = false,
        is_ptr: bool = true,
        _: u11 = FILL,
        _fixnum: bool = false,
    },

    fptr: packed struct {
        value: u50,
        _foreign: bool = true,
        _ptr: bool = true,
        _: u11 = FILL,
        _fixnum: bool = false,
    },

    sstr: packed struct {
        // packed struct cannot contain array
        value: u48,
        tag: Tag = .sstr,
        ptr: bool = false,
        _: u11 = FILL,
        fixnum: bool = false,
    },

    char: packed struct {
        value: u48,
        tag: Tag = .char,
        ptr: bool = false,
        _: u11 = FILL,
        fixnum: bool = false,
    },

    misc: packed struct {
        value: u48,
        tag: Tag = .misc,
        ptr: bool = false,
        _: u11 = FILL,
        fixnum: bool = false,
    },

    const Tag = enum(u3) { sstr = 1, char = 2, misc = 3 };

    const Self = @This();

    /// Hexdumps the value.
    pub fn dump(self: Self) void {
        std.debug.dumpHex(std.mem.asBytes(&self));
    }

    /// Checks for a Zisp value (non-double) packed into a NaN.
    pub fn isPacked(self: Self) bool {
        return self.nan.exp == FILL and self.nan.rest != 0;
    }
};
