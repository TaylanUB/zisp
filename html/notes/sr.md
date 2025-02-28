# Better syntax-rules?

Yesterday, someone on IRC asked for help in improving the following
syntax-rules (s-r) macro:

```scheme

(define-syntax alist-let*
  (syntax-rules ()
  
    ;; uses subpattern to avoid fender
    ;; alist-expr is evaluated only once
    ((_ alist-expr ((key alias) ...) body body* ...)
     (let ((alist alist-expr))
       (let ((alias (assq-ref alist 'key)) ...)
         body body* ...)))
         
    ((_ alist-expr (key ...) body body* ...)
     (let ((alist alist-expr))
       (let ((key (assq-ref alist 'key)) ...)
         body body* ...)))

))

;; Example uses:

(define alist '((foo . 1) (bar . 2)))

(alist-let alist (foo bar)
  (+ foo bar))                     ;=> 3

(alist-let alist ((foo x) (bar y))
  (+ x y))                         ;=> 3

;; Problem: Can't mix plain key with (key alias) forms:

(alist-let alist ((foo x) bar)
  (+ x bar))                       ;ERROR

```

How do we make it accept a mix of plain keys and `(key alias)` pairs?
Oh boy, it's more difficult than you may think if you're new to s-r
macros.  Basically, there's no "obvious" solution, and all we have is
various hacks we can apply.

Let's look at two fairly straightforward hacks, and their problems.

## Option 1

```scheme

;; Solution 1: Internal helper patterns using a dummy constant.

(define-syntax alist-let*
  (syntax-rules ()

    ((_ "1" alist ((key alias) rest ...) body body* ...)
     (let ((alias (assq-ref alist 'key)))
       (alist-let* "1" alist (rest ...) body body* ...)))

    ((_ "1" alist (key rest ...) body body* ...)
     (let ((key (assq-ref alist 'key)))
       (alist-let* "1" alist (rest ...) body body* ...)))

    ((_ "1" alist () body body* ...)
     (begin body body* ...))

    ;; dispatch, ensuring alist-expr only eval'd once
    ((_ <alist> <bindings> <body> <body*> ...)
     (let ((alist <alist>))
       (alist-let* "1" alist <bindings> <body> <body*> ...)))

))

```

(I've switched to my `<foo>` notation for pattern variables in the
"dispatcher" part.  Don't let it distract you.  I strongly endorse
that convention for s-r pattern variables, to make it clear that
they're like "empty slots" where *any* expression can match, but
that's a topic for another day.)

What the solution above does, is "dispatch" actual uses of the macro,
which obviously won't have the string literal `"1"` in first position,
onto internal sub-macros, which can call each other recursively, so
each layer only handles either a stand-alone `key` or a `(key alias)`
couple.

There's some nuances to this implementation.  First, if you're not
familiar with s-r macros, you may mistakenly worry that this solution
could mask a programmer error: What if we accidentally call the macro
with a variable bound to the string "1"?  Would this lead to a very
annoying bug that's hard to find?  No; remember that syntax-rules
patterns match *unevaluated* operands, so the internal sub-patterns
are only triggered by the appearance of a literal string constant of
`"1"` in the first position; a mistake that would be very apparent in
code you're reading, and is extremely unlikely to occur by accident.

As for a real pitfall of this implementation: The dispatcher pattern
*must* be in the final position; otherwise it will actually catch our
recursive calls starting with `"1"` and bind that string literal to
the `alist` pattern variable!  (Kind of the "reverse" of the fake
problem described in the previous paragraph, in a sense?)  If the
dispatcher pattern is in the first position, it will keep calling
itself with an increasing number of `"1"`s at the start, in an
infinite loop, until you forcibly stop it or it crashes.

As a side note, this brings me to a general s-r pitfall, that applies
to the original implementation as well in this case: Since patterns
are matched top to bottom, a simple `key` pattern variable *could*
actually match the form `(key alias)`, so you have to make sure that
the pattern for matching those key-alias couples comes before the one
matching plain keys.

Oh, and by the way, if you're questioning whether we even need those
internal helper patterns at all: Yes, it's the only way to ensure the
initial `<alist>` expression is only evaluated once, in an outermost
`let` wrapping everything.

Let's summarize the issues we've faced:

1. It's easy to forget that pattern variables can match arbitrary
   expressions, not just identifiers, and there's no way to say it
   should only match identifiers.

2. When an arbitrary expression is matched by the pattern variable,
   using it means repeating that expression every time, unless you
   explicitly use `let` to take care of that, which may require
   dispatching to another pattern immediately if you wanted to use
   recursive patterns.

3. You may accidentally put a more generic pattern first, causing it
   to match an input that was meant to be matched by a subsequent
   pattern with more deeper destructuring.

It may be interesting trying to solve 3 by specifying some way of
measuring the "specificity" of a pattern, and saying that those with
the highest specificity match first, but that may prove difficult.
Besides, solving 1 would basically solve 3 anyway.

Racket has syntax-parse, which solves the first problem through an
incredibly sophisticated specification of "syntax patterns" that take
the place of the humble generic pattern variable of syntax-rules.
It's cool and all, but the charm of s-r is the simplicity.  Can't we
use some of the ideas of syntax-parse patterns and add them to s-r?

In Racket, there's the concept of "syntax classes," and a pattern can
be a variable with `:syntax-class-id` appended to its name, which is
how you make it only match inputs of that syntax class, such as for
example, only identifiers.  Trying to find out what syntax class ids
are supported may send you down a rabbit hole of how you can actually
define your own syntax classes, but that just seems to be a weak spot
of the Racket online documentation; looking a bit closer, you should
find the list of built-in classes that are supported.  They are just
called "library" syntax classes for some reason:

[Library Syntax Classes and Literal Sets -- Racket Documentation](https://docs.racket-lang.org/syntax/Library_Syntax_Classes_and_Literal_Sets.html)

It would be great if there were classes for atoms (anything that's not
a list) and lists, though; then we could do this:

```scheme

(define-syntax alist-let*
  (syntax-rules ()

    ((_ <alist>:list bindings body body* ...)
     (let ((alist <alist>))
       (alist-let* alist bindings body body* ...)))

    ((_ alist (key:id ...) body body* ...)
     (let ((key (assq-ref alist 'key)) ...)
       body body* ...))

    ((_ alist ((key:atom alias:id) ...) body body* ...)
     (let ((alias (assq-ref alist 'key)) ...)
       body body* ...))

))

```

(The key could also be a non-symbol immediate value, like a fixnum,
boolean, etc.; anything that `assq-ref` can compare via `eq?`.  One
could also just not quote the key, and instead let it be an arbitrary
expression, which would probably make for a more useful macro, but
that's a different topic.)

Isn't that really neat?  But let's go one step further.  I believe
this strategy of binding an expression via `let` to ensure it's only
evaluated once is probably so common that it warrants a shortcut:

```scheme

(define-syntax alist-let*
  (syntax-rules ()

    ((_ alist:bind (key:id ...) body body* ...)
     (let ((key (assq-ref alist 'key)) ...)
       body body* ...))

    ((_ alist:bind ((key:atom alias:id) ...) body body* ...)
     (let ((alias (assq-ref alist 'key)) ...)
       body body* ...))

))

```

The idea here is: All pattern variables marked with `:bind` are first
collected, and if there is at least one that is not an identifier,
then the whole template (the part that produces the output of the s-r
macro) is wrapped in a `let` which binds those expressions to the name
of the pattern variable, and uses of that pattern variable within the
template refer to that binding.

I'm not entirely sure yet if this is an ingenious idea, or a hacky fix
for just one arbitrary issue you can face while using syntax-rules,
but I suspect it's a common enough pattern to make it desirable.

## Option 2

I said there were various hacks to solve the original problem; here's
the second variant.  It's actually almost the same thing, but we put
the helper patterns into a separate macro.

```scheme

;; Solution 2: Separate helper macro

(define-syntax alist-let*
  (syntax-rules ()

    ;; dispatch, ensuring alist-expr only eval'd once
    ((_ <alist> <bindings> <body> <body*> ...)
     (let ((alist <alist>))
       (%alist-let-helper alist <bindings> <body> <body*> ...)))

))

(define-syntax %alist-let-helper
  (syntax-rules ()

    ;; basically do here what the internal helpers did in solution 1,
    ;; but without the need for the "1" string literal hack

))

```

That's cleaner in terms of the patterns we have to write, but we had
to define a second top-level macro, which feels wrong.  It should be
properly encapsulated as part of the first.

This is where another improvement to s-r could come in handy, and
that's not making it evaluate to a syntax transformer (i.e., lambda)
directly, but rather making it more like syntax-case in that regard.
However, the additional lambda wrapping always really annoyed me, so
the following syntax may be desirable.

```scheme

(define-syntax (alist-let* . s)

  (define-syntax (helper . s)
    (syntax-rules s ()
      ((alist ((key alias) rest ...) body body* ...)
       (let ((alias (assq-ref alist 'key)))
         (alist-let* "1" alist (rest ...) body body* ...)))
  
      ((alist (key rest ...) body body* ...)
       (let ((key (assq-ref alist 'key)))
         (alist-let* "1" alist (rest ...) body body* ...)))
  
      ((alist () body body* ...)
       (begin body body* ...))
      ))

  (syntax-rules s ()
    ((<alist> <bindings> <body> <body*> ...)
     (let ((alist <alist>))
       (helper alist <bindings> <body> <body*> ...)))))

```

That looks a bit confusing at first sight, but we can actually do
something a lot better now, since we already get one stand-alone
pattern at the start, which fits our intention perfectly here:

```scheme

(define-syntax (alist-let* <alist> <bindings> <body> <body*> ...)

  (define-syntax (helper . s)
    (syntax-rules s ()
      ((alist ((key alias) rest ...) body body* ...)
       (let ((alias (assq-ref alist 'key)))
         (alist-let* "1" alist (rest ...) body body* ...)))
  
      ((alist (key rest ...) body body* ...)
       (let ((key (assq-ref alist 'key)))
         (alist-let* "1" alist (rest ...) body body* ...)))
  
      ((alist () body body* ...)
       (begin body body* ...))
      ))

  #'(let ((alist <alist>))
      (helper alist <bindings> <body> <body*> ...)))

```

To be honest, I don't like this solution nearly as much as the first,
and I now realize that there wouldn't be much point in keeping s-r if
it's going to be so close to syntax-case.  (The only difference, at
this point, would be that s-r implicitly puts `#'` in front of the
templates.  That's literally all it would do, if I'm not mistaken.)

## Or just implement syntax-parse?

Racket can actually give you the implicit lambda when you want it, by
offering `syntax-parser` as an alternative to `syntax-parse`:

```scheme

;; The following two are equivalent.

(define-syntax foo
  (lambda (s)
    (syntax-parse s ...)))

(define-syntax foo
  (syntax-parser ...))

```

(At least, I'm pretty sure that's how it's supposed to work; the docs
just bind the result of `syntax-parser` to an identifier via `define`
and call it as a procedure to showcase it, for whatever reason.)

Yes, syntax-parse is a lot more complex than syntax-rules, but to be
honest it seems mainly the fault of the documentation that it doesn't
showcase the simplest ways of using it, which look essentially the
same as using syntax-rules, so it's not clear why s-r should stay if
you have syntax-parse.

Maybe I would just make one change, which is to allow the following
syntax and thus make the additional `syntax-parser` unnecessary:

```scheme

(define-syntax (foo s)
  (syntax-parse s ...))

```

Note that this is different from my previous idea of making the first
operand to `define-syntax` a pattern.  The only thing I don't like
about this variant is that there will never be more than one argument,
but maybe that's fine?

In any case, I guess the only innovation I came up with here is the
special `:bind` syntax class id, assuming there isn't already a
similar thing in Racket or elsewhere.

Oh and this made me realize I should add `foo:bar` as reader syntax to
Zisp, turning it into `(#COLON foo . bar)` or such.
