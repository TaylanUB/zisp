# More immutability

I see no reason to have mutable variables in the language.

Usually, code is analyzed to distinguish between mutable and immutable
variables, because this aids in optimization.  This means you end up
with two types of variables, and whether a variable is of one or the
other type is determined solely from how it's used.  Ugly!

An explicit box data structure can trivially replicate the features of
a mutable variable, so let's just use that instead.

Our `set!` can assume a box when a plain identifier is used.  But to
get the value, we call `get`.

```scheme

(let ((x (box 0)))
  (while foo
    (set! x (+ x 1)))
  (get x))

```

I've not yet made up my mind on whether pairs should be immutable by
default, but they probably should.  Strings, as also mentioned in
[symbols](symbols.html), will be immutable, since string constants
will be the same thing as symbols.

## Late additions

It now occurs to me that, if you want your explicitly boxed value to
not be heap-allocated, your compiler will need to analyze its use and
potentially unbox it.

So, in terms of code analysis complexity, it may not actually make a
difference, but I still like the more explicit demarcation of mutable
variables.  Perhaps the syntax and semantics could be changed to:

```scheme

(let ((x (mutable 0)))
  (while foo
    (set! x (+ x 1)))
  x)

```

This is different in that passing around `x` will not actually pass
around a box whose contents can be mutated; rather, it's a regular
variable like in Scheme, but mutable unlike normal Zisp variables.
The `mutable` identifier would be part of the `let` syntax and not
possible to use anywhere else.  (Probably not even with `define`.)

It's really just to make code more explicit and easier to grasp,
without any effects on compiler complexity, probably.
