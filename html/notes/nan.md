# NaN-Packing

NaN-packing (also called NaN-boxing) is a strategy involving the use of NaN
bit patterns, that are otherwise unused, to store various values in them.

In the implementation of a dynamically typed language, this can be used to
ensure that all types in the language can be represented by a single 64-bit
value, which is either a valid double, an actual NaN value, or one of the
other NaN bit patterns that represent some other type, potentially in the
form of a pointer to a heap object containing further data.

This works because pointers only need 48 bits in practice, and the range of
unused NaN bit patterns contains an astounding `2^53 - 4` different values.

IMPORTANT NOTE: All illustrations of data structures and bit patterns use
big-endian.  When implementing the strategies described herein, it may be
necessary to reorder the elements.  For example, the elements of packed
structs in Zig are ordered least to most significant.


## The double format

The IEEE 754 double-precision binary floating-point aka binary64 format is:

    { sign: u1, exponent: u11, fraction: u52 }

Possible types of values a double can encode include:

    { sign == any, exponent != 0x7ff, fraction == any } :: Real (finite)
    { sign == any, exponent == 0x7ff, fraction == 0x0 } :: Infinity
    { sign == any, exponent == 0x7ff, fraction != 0x0 } :: NaN

Note:

    0x7ff = u11 with all bits set (0b11111111111)

In other words:

    all exponent bits set, fraction bits all zero :: Infinity
    all exponent bits set, fraction part non-zero :: NaN


## Details of NaN values

There are two different NaN types: signaling and quiet.  Quiet NaN may be
returned by FP operations to denote invalid results, whereas signaling NaN
are never returned by FP operations and serve other purposes.

Modern hardware sets the MSB of the fraction to indicate that the NaN is a
quiet one, so let's refine our definition for denoting NaN values:

    { sign: u1, exp: u11, quiet: u1, rest: u51 }

Variants of NaN:

    { sign == any, exp == 0x7ff, quiet == 0, rest >= 0x1 } :: sNaN
    { sign == any, exp == 0x7ff, quiet == 1, rest == any } :: qNaN

Note that in case of the signaling NaN, the rest of the fraction must be
non-zero, since otherwise the entire fraction part would be zero and thus
denote an infinity rather than a NaN.

Most systems have a "canonical" quiet NaN that they use:

    { sign == any, exp == 0x7ff, quiet == 1, rest == 0x0 } :: cqNaN

The sign bit of the canonical quiet NaN is undefined and may differ from
operation to operation or depending on the platform.

It's useful to see a few common examples expressed in hex:

    0x7ff8000000000000 :: cqNaN, sign bit nil
    0xfff8000000000000 :: cqNaN, sign bit set

    0x7ff8000000000001 :: smallest non-canon qNaN, sign bit nil
    0xfff8000000000001 :: smallest non-canon qNaN, sign bit set

    0x7fffffffffffffff :: largest non-canon qNaN, sign bit nil
    0xffffffffffffffff :: largest non-canon qNaN, sign bit set

    0x7ff0000000000001 :: smallest sNaN, sign bit nil
    0xfff0000000000001 :: smallest sNaN, sign bit set

    0x7ff7ffffffffffff :: largest sNaN, sign bit nil
    0xfff7ffffffffffff :: largest sNaN, sign bit set


## Unused NaN bit patterns

Let's start with the quiet NaN values.

Theoretically, there only needs to be one canonical quiet NaN, so we would
have `2^52 - 1` unused bit patterns in the quiet NaN range.  In practice,
however, the sign bit may differ from one operation to the next.

For example, the fabs function may simply clear the sign of the argument,
without minding it being a NaN.  In that case, if the platform's regular
canonical NaN is the one with the sign bit set, we would end up getting
another, "semi-canonical" quiet NaN bit pattern, with the sign bit nil.

So, both variants of the canonical quiet NaN are in use.

This leaves `2^52 - 2` definitely-unused quiet NaN bit patterns:

    { sign == any, exp == 0x7ff, quiet == 1, rest >= 0x1 } :: Unused qNaN

Remember that signaling NaN are defined in a very similar way:

    { sign == any, exp == 0x7ff, quiet == 0, rest >= 0x1 } :: sNaN

Since none of those can be returned by FP operations, they could all be seen
as unused, giving us another `2^52 - 2` bit patterns.

In total, this gives us `2^53 - 4` definitely-unused NaN bit patterns.


## Representing Zisp values and pointers as unused NaN bit patterns

Zisp wants to store two different things in unused NaN patterns:

1. Pointers (to anything in principle)

2. Non-double primitive aka "immediate" values

It may seem intuitive to use signaling NaN for one, and quiet NaN for the
other.  However, this would fragment our "payload" bits, since we would be
using the sign bit as its MSB and the remaining 51 bits of the fraction as
the rest of the payload.

Further, we want to use as many bit patterns as possible for fixnums, so we
can have a nice large fixnum range.  To this end, it would be nice if we
could, for example, use all bit patterns where the sign bit is set for our
representation of fixnums, and then the range of bit patterns with the sign
bit unset can be shared among the remaining values, and pointers.

Then let's do exactly that, and use the sign as the first major distinction
between fixnums and other values, using it as a sort of `is_int` flag:

    { sign == 0x0, exp == 0x7ff, payload == ??? } :: Non-Fixnum
    { sign == 0x1, exp == 0x7ff, payload == ??? } :: Fixnum

It will become apparent in a moment why we haven't defined the payload yet.

Given that our payload is the entire fraction part of the IEEE 754 double
format, we must be careful not to use the following two payload values
regardless of the sign bit:

1. Zero: This would make the bit pattern represent an infinity, since the
payload is the entire fraction and a zero fraction indicates infinity.

2. `0x8000000000000` (aka only the MSB is set): This would make the bit
pattern a canonical quiet NaN, since the payload MSB is the quiet bit.

This means that in each category (sign bit set, or nil) we have `2^52 - 2`
possible bit patterns, and the payload has a rather strange definition:

    0x0 < payload < 0x8000000000000 < payload < 0xfffffffffffff

Can we really fit a continuous range of fixnum values into that payload
without significantly complicating things?  Yes, we can!  Observe.


## Fixnum representation

We will store positive and negative fixnums as separate value ranges, using
the quiet bit to differentiate between them.

Let's go back to considering the quiet bit a separate field:

    { sign == 0x1, exp == 0x7ff, quiet == 0x0, rest >= 0x1 } :: Positive
    { sign == 0x1, exp == 0x7ff, quiet == 0x1, rest >= 0x1 } :: Negative

But, I hear you say, the positive range is missing zero!  Worry not, for
maths is wizardry.  We will actually store positive values as their unary
complement (bitwise NOT) meaning that all bits being set is our zero, and
only the LSB being set is the highest possible value.

This must be combined with a bitwise OR mask, to ensure that the 13 highest
of the 64 bits turn into the correct starting bit pattern for a signed NaN.
Unpacking it is just as simple: Take the unary complement (bitwise NOT) and
then use an AND mask to unset the 13 highest:

    POS_INT_PACK(x)   = ~x | 0xfff8000000000000

    POS_INT_UNPACK(x) = ~x & 0x0007ffffffffffff

If you've been paying very close attention, you may notice something: Given
that we know the 13 highest bits must always have a certain respective value
in the packed and unpacked representation (12 highest set when packed, none
set when unpacked), we can use an XOR to flip between the two, and the same
XOR can take care of flipping the remaining 51 bits at the same time!

This also means packing and unpacking is the same operation:

    POS_INT_PACK(x)   = x ^ 0xfff7ffffffffffff

    POS_INT_UNPACK(x) = x ^ 0xfff7ffffffffffff

There we go; packing and unpacking 51-bit positive fixnums with one XOR!
Amazing, isn't it?

As for the negative values, it's even simpler.  This time, the correct NaN
starting pattern has all 13 bits set, since the quiet bit being set is what
we use to determine the number being negative.  And would you believe it;
this means the packed negative fixnum already represents itself!

    NEG_INT_PACK(x)   = x

    NEG_INT_UNPACK(x) = x

Isn't that unbelievable?  I need to verify this strategy further, but based
on all information I can find about NaN values, it should work just fine.

The only disappointing thing is that it's positive integers that need an XOR
to pack and unpack, rather than negative ones.  One would expect positive
values to occur much more frequently in typical code.  But I think we can
live with a single XOR instruction!


## Pointers & Others

We still want to represent the following, which must share space within the
`2^52 - 2` bit patterns that can be packed into an unsigned NaN:

- Pointers of various kinds
- Unicode code points (21-bit values)
- False, true, null, end-of-file, and maybe a few more singletons

It seems sensible to split this into two main categories: pointers and other
values.  Let's use the quiet bit as a `pointer` flag:

    { sign == 0x0, exp == 0x7ff, quiet == 0x0, rest >= 0x1 } :: Other
    { sign == 0x0, exp == 0x7ff, quiet == 0x1, rest >= 0x1 } :: Pointer

Note how neither type is allowed to have a zero payload, since in case of an
unset quiet bit, this would make our value an infinity, and in case of a set
quiet bit it would give us a canonical quiet NaN.  Each of them is allowed
any other payload than zero.

### Pointers

It would seem that we have 51 bits left to represent a pointer (though we
need to avoid the value zero).  But we only need 48 bits... or even less!
Since allocations happen at 8-byte boundaries on 64-bit machines, we only
really need 45 of the 48 bits, given the least significant 3 will never be
set.  This gives us a whole 6 free bits to tag pointers with!  If we have
that much play room, we can do some crazy things.

#### Foreign pointers

Firstly, let's introduce the concept of a "foreign" pointer.  This means the
pointer doesn't necessarily point to a Zisp object, and may not be 8-byte
aligned.  As it may point to anything, there's no point in defining further
bits as tagging additional information, so we have all 50 bits available.

Let's cut out the 12 high bits of our double since we already know what they
must contain, and look at the definition of our 52-bit payload.

I will also mix up the notation a bit, to indicate that some fields are only
defined if a previous field has a given value.

    { pointer == 0x1, foreign: u1, rest: u50 }

(The `pointer` field is the `quiet` bit i.e. MSB of the 52-bit fraction.)

If the foreign bit is set, then the entire `rest` field shall be seen as
opaque and may contain any value.  Another way to look at this is that we
essentially defined another fixnum range of 50 bits.  This can include the
value zero, since the foreign bit being set ensures we don't step on the
forbidden all-zero payload value.

#### Zisp pointers

Now let's look at what we can do with "native" Zisp pointers.

Wouldn't it be nice if our language had an explicit "pointer" data type and
it didn't require any additional heap allocation?  So let's decide that one
bit is dedicated to distinguishing between an explicit pointer object, and
regular pointers that stand in for the object being pointed to.

Perhaps it would be good to show some Zisp pseudo-code to explain what that
means, since it may be a strange concept:

    ;; In memory, vec is represented as a regular/direct vector pointer.
    (define vec (vector 1 2 3))

    ;; We can of course use this variable as a vector.
    (vector? vec)      ;=> #t
    (vector-ref vec 0) ;=> 1

    ;; Now we create an explicit pointer object pointing to that vector.
    ;; Distinguished by a special bit in the in-memory value of vec-ptr.
    (define vec-ptr (pointer vec))

    ;; This variable is *not* a vector; it's a vector-pointer.
    (vector? vec-ptr)      ;=> #f
    (vector-ref vec-ptr 0) ;ERROR
    (pointer? vec-ptr)     ;=> #t
    (pointer-ref vec-ptr)  ;=> #(1 2 3)

This is *not* the same thing as a box, because it can *only* refer to heap
allocated objects, not immediates, whereas a box would be able to hold an
immediate value like an integer or double as well.

    (pointer 42) ;ERROR
    (box 42)     ;=> #<box:42>

A box would necessarily need heap allocation, whereas a pointer doesn't.

It's *also not* the same thing as a foreign pointer, because those can be
anything, whereas these pointer objects definitely point to Zisp objects.

Pointers may or may not be mutable; I've not made up my mind yet.  It may
seem like a pointless data type, but it adds a little bit of expressive
strength to our language.  For example, when working with an FFI.  And
there's really not much else we can do with all our bits.

Let's use the term "indirect" for this tag, since "pointer" is already used:

    { pointer == 0x1, foreign == 0x0, indirect: u1, rest: u49 }

Direct or indirect makes no difference to the fact that the pointer value
will be 8-byte aligned, so we still have 4 bits for more information about
what's being pointed to.  Also, since the actual pointer value can never be
zero (all non-foreign pointers must point to a valid Zisp object), we avoid
the forbidden zero pattern.  Thus, we can indicate 16 different values with
our 4 remaining bits.

It would have been nice to avoid fragmentation of these remaining tag bits.
However, we want to avoid shifting, so let's go with this definition for the
remaining 49 bits:

    { tag_high: u1, pointer_value: u45, tag_low: u3 }

The pointer value is extracted by masking the entire bit sequence, so it
actually becomes a 48-bit value without further shifting.

The tag can be used to tell us what we're pointing to, so that type checks
often don't require following the pointer.  The memory location that's being
pointed to may duplicate this information, since we may want to ensure that
any Zisp object on the heap carries its type information within itself, but
I'm not yet decided on that.

In any case, let's list some common heap data types that our 4-bit tag can
represent, making sure to have an "other" wildcard for future extensions.

The right side shows the value of the type tag when it's acquired by masking
the 49-bit Zisp pointer payload.

    0. String (Symbol) .... 0x0000000000000
    1. Pair (List)          0x0000000000001
    2. Vector ............. 0x0000000000002
    3. Map (Hash-table)     0x0000000000003
    4. Box ................ 0x0000000000004
    5. Record               0x0000000000005
    6. Class .............. 0x0000000000006
    7. Instance             0x0000000000007
    8. Text ............... 0x1000000000000
    9. Byte-vector          0x1000000000001
    10. Procedure ......... 0x1000000000002
    11. Continuation        0x1000000000003
    12. Port .............. 0x1000000000004
    13. Error               0x1000000000005
    14. Enum .............. 0x1000000000006
    15. Other               0x1000000000007

This list is likely to change; for example: errors should probably be class
instances, continuations could be merged with procedures, and so on.  But
this gives us a rough picture and demonstrates that 16 distinct values is
quite sufficient for avoiding a pointer de-reference in type checking.

(Why is it so important to avoid following a pointer when checking a type?
Who knows?  Did I say it was important?  Why look at me like that??)

### Other values, including Unicode

WIP


<!--
;; Local Variables:
;; fill-column: 77
;; End:
-->
