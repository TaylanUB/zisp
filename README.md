# Zisp: 21st-century Scheme-inspired language

Zisp is my experimental toy language inspired by Scheme.  The idea is
that it's a modern "re-imagining" of what Scheme may have been had it
been invented today, and had it been designed with pragmatic use as a
primary concern in its design.

This language doesn't actually exist yet.  You are merely reading the
ramblings of a madman.


## Compilation is execution

Any Scheme implementation with support for procedural macros allows
arbitrary code execution at compile-time.  However, this is slightly
awkward:

```scheme

  (define-syntax comptime
    (lambda (stx)
      (syntax-case stx ()
        ((_)
         (begin
           (display "foo\n")))))

  (comptime)

```

Compiling this with, for example, `guild compile foo.scm` will lead to
the line "foo" being printed.  (The actual program is a no-op and will
do nothing when run after compiled.)

One can of course implement a macro such as `eval-when-compile` to
make this slightly less awkward.  Using R6RS:

```scheme

  (import (rnrs eval))

  (define-syntax eval-when-compile
    (lambda (stx)
      (syntax-case stx ()
        ((_ imports body ...)
         (eval
           (syntax->datum #'(begin body ...))
           (apply environment (syntax->datum #'imports)))))))

  (eval-when-compile
   ((rnrs))
   (display "foo\n"))

```

An implementation may of course contain such a macro in its standard
library, but it's unclear why the language should put such a hurdle in
our way.  There are problems beyond this little hurdle as well.

Top-level forms in Scheme are semantically executed at run-time, not
compile-time.

(Actually, the Scheme standards don't explicitly define a run-time or
compile-time stage, but it's arguably implicit in the fact that macros
are *not* first-class, and are defined by the language in such a way
that they can be executed entirely at compile-time if ahead-of-time
compilation is supported by an implementation.)

Consider the case where the programmer wants to perform a relatively
costly calculation at compile-time and store the result as part of the
compiled program.  Say, a lookup-table.  Naively, we may attempt the
following:

```scheme

  ;; The fictional file `lookup-table.dat` would be in the source code
  ;; repository, and the fictional procedure `process-data` would read
  ;; it and return a data structure.
  (define lookup-table (process-data "lookup-table.dat"))

```

This will not work.  Compiling a Scheme file containing such a form
will produce a program that calls `process-data` at run-time and not
at compile-time as intended.

One can of course resolve this with an explicit use of a procedural
macro.  In fact, all one needs to do is redefine `process-data` as a
macro, but I find this to be an unnecessary complication.

Further, any sufficiently intelligent implementation *will* actually
execute such top-level definitions at compile-time, given that they
only make use of compile-time constants and pure functions.

```scheme

  ;; Guile will compute the value at compile-time.
  (define seconds-per-day (number->string (* 24 60 60)))

```

This is easily observed by running `guild compile test.scm -o test.go`
and then `strings test.go` which will contain the string 86400 in its
output.  (If we didn't use `number->string`, it would be harder to
locate the number 86400 in the output, since it would be binary.)

This works because Guile implements the "partial evaluation" strategy
for program optimization.  This requires the optimizer to know which
procedures are pure.  A limitation in the implementation may lead to
some such opportunities to be missed, and for the compiled program to
execute unnecessary code at run-time.

To recap: The top-level of a Scheme file is conceptually executed at
run-time.  But an optimizer may execute some of it at compile-time
anyway.  However, we're at the mercy of the implementation quality for
this to happen consistently.  We can use procedural macros to force
some execution to definitely happen at compile-time.  What a mess!

It would be so much simpler if compiling a program meant, at the
language semantics level, that the top-level is executed.

Any run-time initialization of a program or module should be explicit,
such as by putting it into a `main` function, or having the module
export an initialization function.  (The language may, if I feel like
it, allow for declaring a module initializer function, which would be
invoked automatically when a module is loaded.)


## Everything can be serialized

If you're familiar with Guile --and I suspect most implementations of
Scheme have a similar limitation-- then you may have noticed an issue
in the above example where the programmer intends to compute a lookup
table at compile-time.

Not all Scheme objects can be serialized.  This not only applies to
the `write` procedure, but also the compiler's ability to put objects
into the binary representation of a compiled program.  (For example,
the .data section of an ELF file in case of Guile.)

This can be demonstrated as follows:

```scheme

  (define-syntax process-data
    (lambda (stx)
      (syntax-case stx ()
        ((_ file)
         ;; Ignore `file`; this is just an example!
         (let ((ht (make-eqv-hashtable)))
           (hashtable-set! ht 1 2)
           ht)))))

  (define lookup-table (process-data "lookup-table.dat"))

```

Compiling this with `guild` will yield an error, complaining about an
"unhandled constant" represented as #<r6rs:hashtable ...> in the error
message.  What it's actually trying to say is that hash tables aren't
constants, and the compiler doesn't know how to put them into the ELF
file it's writing.

(At least, this is the case as of February 2025, using Guile 3.0.10;
who knows what the future will provide!)

In Zisp, I want absolutely everything to be possible to serialize, and
the compiler should simply be using this capability of the language to
write out compiled binaries.

For example, given that any Zisp program has to declare a `main` entry
point function, all the compiler would do is execute the file and then
call `(write main)`.  How elegant!

This serialization of a function would, of course, involve traversing
all references therein, and including them in the output somehow.  The
same will apply to writing out any data structure.  This means that
serializing a module is *not* a matter of invoking `write` on each of
its exported definitions.  This would lead to lots of duplicate data
between the outputs, and `eq?` relations would be lost after reading
them back in.  We probably want a first-class `module` type that can
be serialized as one object.

(If we want symmetry between `read` and `write`, then `read` needs to
detect that it's looking at a compiled binary.  Not entirely sure yet
if I really want this, but I think I do.)


## Symbols are strings are symbols

In Scheme, symbols are literally just interned and immutable strings.
They can contain any character a string can, constructed either via
`string->symbol` or the modern `|foo bar baz|' syntax for quoted
symbols.  Why not just embrace the fact that they are strings?

Scheme strings are mutable, but they are a terrible choice for text
manipulation, because they are constant-length.  They are literally
just vectors of characters.  If you wanted a vector of characters,
well, use a vector of characters!

Zisp won't differentiate between symbols and strings.  All strings
will be immutable, string constants will be automatically interned,
and bare symbols will just be reader syntax for a string constant.

Instead of `string->symbol` we will have `string-intern` which
basically does the same thing.  Dynamically generated strings that
aren't passed to this function will be uninterned.


## Stop the "cons" madness!

Lists are neat, but they aren't the best representation for sequences
of fixed length.  An array/vector is a better choice for this.

R6RS already uses vectors in some places where a traditional lisper
may have expected to see lists.  Namely, in the procedural layer of
record types, where the fields of a record type are represented by
vectors.  There may be more places in Scheme where this makes sense.


## Better handling of rest args

Initially, I was thinking of using either immutable vectors, or
immutable lists transparently backed by arrays, to represent rest
arguments.  But the following paper offers a very compelling
alternative:

  https://legacy.cs.indiana.edu/~dyb/pubs/LaSC-3-3-pp229-244.pdf

Long story short, rest argumenst are received through a parameter list
such as `(arg1 ... argn & rest)` and the identifier `rest` is special
in that it can only be passed on to another procedure using the same
syntax.  For example, to explicitly put the rest args into a list and
map over them:

```scheme

  (define (map* proc & args)
    (map proc (list & args)))

  (map* square 1 2 3) ;=> (1 4 9)

```

Recursive functions that directly consume an arbitrary number of args,
without needing to allocate any data structure, can be implemented by
combining this feature with what is today known as `case-lambda`:

```scheme

  (define combine*
    (case-lambda
      ((x) x)
      ((x y & rest) (combine* (combine x y) & rest))

```

Though the paper proposes the use of `&` so as to differentiate it
from the regular rest-argument mechanism of Scheme, I intend to make
Zisp use only this mechanism, so we can use the dot notation for it.
Rewriting the above examples in this style gives us:

```scheme

  (define (map* proc . args)
    (map proc (list . args)))

  (define combine*
    (case-lambda
      ((x) x)
      ((x y . rest) (combine* (combine x y) . rest))))

```

I find this very pleasing on the eyes, and a very elegant way to use
improper lists in evaluation context, which isn't allowed in Scheme.


## More ergonomic multiple-values

The paper linked above proposes to reuse the rest args syntax for an
elegant solution to consuming multiple values:

```scheme

  (proc x y & <expr>)

```

In the above, `<expr>` may evaluate to multiple values, and the values
will be passed to `proc` as additional arguments.

Essentially, this means that the special rest-arg identifier is itself
a representation of multiple values, or in other words, evaluating it
results in multiple values even though it's just an identifier!

Demonstration, using Zisp notation for multiple-value rest args:

```scheme

  (define (foobar . rest)
    (let-values (((x y z) rest))
      ;; This is a meaningless example for demonstration, since we
      ;; could have just made the function accept three parameters.
      ;; The `let-values` here is the one from SRFI 11.
      ))

```

The paper also proposes terminating lambda bodies with `& <expr>` to
return multiple values, but this is obsolete as of R5RS, which allows
the final expression in a body to evaluate to multiple values anyway.

However, the use of & to pass multiple values as arguments is very
convenient and way clearer than R5RS's clumsy `call-with-values`:

```scheme

  ;; Returns {bool, obj} where bool indicates success/failure and obj
  ;; is meaningless if bool is false; allows differentiating between
  ;; the case where #f is found as the value vs. nothing being found.
  (define (lookup alist key)
    (if (null? alist)
        (values #f #f)
        (if (eqv? key (caar alist))
            (values #t (cdar alist))
            (lookup (cdr alist) key))))

  (define (display-if-found found? obj)
    (when found? (display obj)))

  ;; Incredibly ugly `call-with-values`:
  (call-with-values
    (lambda () (lookup '((x . y)) 'x))
    display-if-found)

  ;; (Up until here is valid R5RS code, by the way.)

  ;; So much cleaner:
  (display-if-found & (lookup '((x . y)) 'x))  ;; displays x

```

Unfortunately, we can't reuse the improper list syntax in the last
example, since the following s-expressions are equivalent:

```scheme

  (foo . (bar baz))
  (foo bar baz)

```

In Zisp, this will be solved by making `apply` a special-form where
the last operand is expected to evaluate to multiple values rather
than a list:

```scheme

  ;; (apply <proc-expr> <argn-expr> ... <restargs-expr>)

  (apply display-if-found (lookup '((x . y)) 'x))

```

Note that this means the forms `(apply foo rest)` and `(foo . rest)`
are equivalent if `rest` is an identifier and not a pair/list, while
`(apply foo (x ...))` is of course NOT equivalent to `(foo x ...)`.


## A little bit of syntax sugar never hurt anyone

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

Furthermore, `foo[bar]` could be sugar for `(vector-ref foo bar)` and
`foo{bar}` could be sugar for `(map-ref foo bar)` or the like.  (Not
sure yet whether to use the word "map" for a data type, due to the
overlap with the `map` function.)

The functionality of SRFI 17 should be a core aspect of the language,
so the following all work:

```scheme

  ;; Record
  (define rec (make-foo))
  (set! rec.field value)

  ;; Vector
  (define vec (vector 1 2 3))
  (set! vec[n] value)

  ;; Some kind of map
  (define ht (hash-table))
  (set! ht{key} value)

```


## More immutability

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
default, but they probably should.

Strings, as already mentioned before, will be immutable, since string
constants will be the same thing as a symbol.


## No shadowing (shock!) and a reduced set of `let` forms

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


## Return zero values when there's nothing to return

This is only a minor point: It's a long-running pet peeve of mine that
Scheme specifies "an unspecified value" to be returned when there's
nothing meaningful to return.  It's a remnant from before we had the
ability to return multiple values, and should be eliminated.

Any operation that has nothing meaningful to return, will return zero
values in Zisp.


## Strict mode to disallow ignoring returned values

This ties in to the last point.  In Scheme, a non-tail expression in a
body can return an arbitrary number of values, which will be silently
ignored.

This can lead to bugs, where a procedure actually returns some kind of
success or failure indicator (instead of raising an error) and the
programmer forgets to handle it.

Though it may be too inefficient to enable globally, there should at
least be a mode of compilation that emits code which checks at every
single function return whether there are any values that are being
ignored, and raises an error if so.

There would of course be a form to explicitly ignore values.


## Only Booleans have truthiness

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
determines whether returned values can be ignored may also determine
whether non-Booleans can be coerced into Booleans.


## Subtyping of record types

It's a serious limitation of SRFI 9 that it doesn't allow creating
subtypes with additional fields.  This is an invaluable strategy for
representing a hierarchy of types, which are ubiquitious in real life
and thus in programming.

Sadly, this brings with it some significant complications if records
are to be initialized with default values to ensure invariants.  The
R6RS solves this with an incredibly sophisticated system, which we
might need to adopt.  (Search for "protocol" and "record-constructor
descriptor" in the R6RS.)

However, we may be able to get away with a simpler approach...


## OOP is not that bad

The immense complexity of orchestrating the correct initialization of
a record type can be overcome by 
