# Only Booleans have truthiness

Like in Java, there should be no implicit conversion of values to a
Boolean.  This leads to sloppy code and subtle bugs.

I believe the code base of Guix contained an example of this at some
point: Build phases ending in a call to `system*` would return 0 or 1
which would pass as "true" regardless.

In most cases, you should know the actual type of the value you're
receiving, and do an appropriate check, be it `zero?`, `null?`, or
some other check.  If you truly want to check if something is "any
value other than false" then you can always do:

```scheme

(if (not (eq? #f value))
    (do something))

```

No, you cannot use `(not (not x))` because `not` obviously expects a
Boolean argument!  Duh.

I'm actually serious about this.  Scheme went all the way to make null
separate from false, but then refused to go all the way and decided to
allow non-Boolean values to function as Booleans anyway.

Of course, performing a type-check on every single conditional may
incur a serious performance penalty.  If so, then the same flag that
determines [whether returned values can be ignored](strict-mode.html)
may also determine whether non-Booleans can be coerced into Booleans.
