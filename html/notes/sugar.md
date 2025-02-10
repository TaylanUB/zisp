# A little bit of syntax sugar never hurt anyone

## Lambda shorthands

We could benefit from a more minimal syntax to express lambda, as an
alternative to the written-out form:

```scheme

;; {x ... expr} = (lambda (x ...) expr)

(map {x y (+ x y)} xs ys)

(for-each {x (display x)} objects)

```

## Vector and map references

Furthermore, `foo[bar]` could be sugar for `(vector-ref foo bar)` and
`foo{bar}` could be sugar for `(map-ref foo bar)` or the like.  (Not
sure yet whether to use the word "map" for a data type, due to the
overlap with the `map` function.  Perhaps `mapping`.  In any case
there must be a map kind of data type in the standard library.)

## Records

Why not make `foo.bar` reader syntax for `(record-ref foo bar)` where
`record-ref` is a macro that interprets `bar` as a constant?

Admittedly, this loses us some elegance compared to the widely used
SRFI 9 (standardized in R7RS-small) where field getters only exist as
procedures, so making a field "private" is simply a matter of leaving
the getter out of your module exports.

A general `record-ref` that accepts field names would need to be aware
of the concept of private fields and block access to them except if
invoked in a context where the private fields are accessible.

I think the nice syntax is worth the added complexity.  It doesn't
seem terribly difficult to expand the concept of "lexical scope" to
include field accessibility information.

Alternatively, we could just not have private fields, like in Python.
Fields that are meant to be private could be named in a special way by
convention, such as `%foo` or whatever.  I have to check whether real
encapsulation would provide us with any substantial benefits, such as
in optimization.

Oh, speaking of performance, of course `(record-ref x y)` has a big
problem: It would require dynamic dispatch since the type of x is not
statically known.  (And no we don't want to write `person.person-age`,
we want `person.age` where `age` is not treated as an identifier but
merely as a symbol for lookup.)

It may be that we want to add static typing to Zisp!

We may also add OOP-style objects and only use the dot notation for
their methods, but not fields.  The reader syntax for `foo.bar` may
then expand to `(method-dispatch foo bar)`.  It would also work for
fields if we really did add static typing, of course.  The reason it
would only work on methods is that those need dynamic dispatch anyway.

THIS POINT NEEDS A LOT MORE CONSIDERATION!

## Built-in SRFI 17

The functionality of SRFI 17 should be a core aspect of the language,
so the following all work:

```scheme

;; Vector
(define vec (vector 1 2 3))
(set! vec[n] value)

;; Some kind of mapping
(define table (make-table))
(set! table{key} value)

;; Record type
(define rec (make-foo))
(set! rec.field value)

```
