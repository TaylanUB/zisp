# No shadowing (shock!) and a reduced set of `let` forms

This may be shocking for Schemers but I believe shadowing variables is
evil.  I've already written a bug once that would have been prevented
had it not been possible to shadow variables, and I don't even write
that much Scheme code.

And let's face it: The presence of four different forms by the name of
`let`, `let*`, `letrec`, and `letrec*` is daunting when you're coming
from another language.  Lack of shadowing allows us to reduce this
without losing out much functionality.

In the absence of shadowing, `let` becomes nearly useless because you
can always use `letrec` to fulfill the same need; it's strictly more
powerful.  (The only thing `let` can do that `letrec` can't, is to
refer to the previous binding of a variable before shadowing it.)

Further, the value of vanilla `letrec` is dubious when `letrec*` is
strictly more powerful.  So, in Zisp, `let` is the ultimate binding
form that does what `letrec*` does in Scheme.

Except it does more!  We haven't looked at the whole `let-values`
family yet.  In Zisp, these are also merged into the ultimate `let`,
using the SRFI 71 syntax:

```scheme

(let ((a (one-value))
      (b c (two-values))
      ((values d e . rest) (arbitrary-values)))
  (do-things))

```

You may be wondering whether it also supports the "let loop" syntax of
vanilla Scheme `let` and the answer is no, because I hate that syntax.
It has too high a risk of leading to absolute spaghetti code with no
clear indication as to how the loop variables are being updated.

If you want to loop, use a dedicated looping syntax!  Even `do` is
better than "let loop" shenanigans if you ask me.
