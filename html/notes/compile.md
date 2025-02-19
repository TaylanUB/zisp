# Compilation is execution

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
