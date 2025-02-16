# Stop the "cons" madness!

Lists are neat, but they aren't the best representation for sequences
of fixed length.  An array/vector is a better choice for this.

R6RS already uses vectors in some places where a traditional lisper
may have expected to see lists.  Namely, in the procedural layer of
record types, where the fields of a record type are represented by
vectors.  Another example is `hashtable-keys` and `hashtable-entries`
which both return vectors.  There may be more places in Scheme where
this makes sense.

In the following, we discuss a better handling of rest-argument lists
and a change to `apply`.  In short, rest arguments are not actually
lists anymore, but rather a special kind of identifier that refers to
multiple values.  And `apply`, which becomes a special form, expects
its last argument not to be a list but rather an expression that may
evaluate to multiple values.


## Better handling of rest args

Initially, I was thinking of using either immutable vectors, or
immutable lists transparently backed by arrays, to represent rest
arguments.  But the following paper offers a very compelling
alternative:

[A New Approach to Procedures with Variable Arity](https://legacy.cs.indiana.edu/~dyb/pubs/LaSC-3-3-pp229-244.pdf)

Let's first summarize the paper, and then see how we can adapt its
ideas for Zisp.

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
convenient and much cleaner than R5RS's clumsy `call-with-values`:

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
`(apply foo (x ...))` is of course different from `(foo x ...)`.

I find this all incredibly pleasing.  Lists never had any business in
representing arguments in the first place; it should always have been
multiple values!

(The phrase "argument list" is probably going to stick around forever
though, even if it's technically wrong in Zisp.)
