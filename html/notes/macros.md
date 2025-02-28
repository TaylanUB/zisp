# Does the decoder implement macros?

I've written about the [parser/decoder dualism](reader.html) in a
previous article.  Long story short, the parser takes care of syntax
sugar, like turning `#(...)` into `(#HASH ...)`, and the decoder takes
care of turning that into a vector or whatever.

Now, since the job of the decoder seems superficially quite similar to
that of a macro expander, I've been agonizing for the past two days or
so whether it *is* the macro expander.

(Warning: This post is probably going to be very rambly, as I'm trying
to gather my thoughts by writing it.)

On one hand, sure:

    (define-syntax #HASH
      (syntax-rules ()
        (#HASH <element> ...)
        (vector '<element> ...)))

Or something like that.  You know what I mean?  I mean, in Scheme you
can't return a vector from a macro, but in Zisp the idea is that you
can very well do that if you want, because why not.

It's very much possible that I will eventually realize that this is a
bad idea in some way, but we'll see.  So far I really like the idea of
a macro just returning objects, like a procedure, rather than having
to return a syntax object that has a binding to that procedure.

This may be similar to John Shutt's "vau calculus" from his language
Kernel.  Maybe Zisp will even end up being an implementation of the
vau calculus.  But I don't know; I've never fully grokked the vau
calculus, so if I end up implementing it, it will be by accident.

In any case, I want the user to be able to bind transformers to runes,
and doing so feels like it's pretty much the same thing as defining a
macro, so maybe the decoder should also be the macro expander.

But then there's an issue with quoting.  Consider the following:

    (define stuff '(foo #(0 1 2)))

In Zisp, this would first of all be parsed into:

    (define stuff (#QUOTE foo (#HASH 0 1 2)))

Now, if #QUOTE didn't decode its operand, we'd end up seeing #HASH in
the result, never creating the vector we meant to create.

But if #QUOTE calls decode on its operand, and the decoder is also the
macro expander, whoops:

    (let-syntax ((foo (syntax-rules () ((_ x) (bar x)))))
      '(foo #(0 1 2)))

    ;; => (bar #(0 1 2))

I mean... MAYBE that should happen, actually?!  Probably not, though.
What Scheme does isn't gospel; Zisp isn't Scheme and it will do some
things differently, but we *probably* don't want anything inside a
quoted expression to be macro expanded.  Probably.

The thought that I might actually want that to happen sent me down a
whole rabbit whole, and made me question "runes" altogether.  If they
just make the decoder invoke a predefined macro, well, why not ditch
runes and have the parser emit macro calls?

So instead of:

    #(x y z)  ->  (#HASH x y z)

(Which is then "decoded" into a vector...)  Why not just:

    #(x y z)  ->  (VECTOR x y z)

And then `VECTOR` is, I don't know, a macro in the standard library I
guess.  If the decoder is the macro expander, then sure, it will know
about the standard library; it will have a full-blown environment that
it uses to macro expand, to look up macro names.

But no, I think this conflates everything too much.  Even just on the
level of comprehensibility of code containing literals, I think it's
good for there to be something that you just know will turn into an
object of some type, no matter what; that's what a literal is.

(In Zisp, it's not the reader that immediately turns the literal into
an object of the correct type, but the decoder still runs before the
evaluator so it's almost the same.)

Then again, maybe this intuition just comes from having worked with
Scheme for such a long time, and maybe it's not good.  Perhaps it's
more elegant if everything is a macro.  Don't pile feature on top of
feature, remember?

Booleans, by the way, would just be identifier syntax then.  Just
`true` and `false` without the hash sign.  In Zisp, you can't shadow
identifiers anyway, so now they're like keywords in other languages,
also a bit like `t` and `nil` in CL and Elisp.

IF we are fine with the quote issue described above, then I *think*
everything being a macro would be the right thing to do.  Although
I've said the decoder could be used for things other than code, like
for configuration files containing user-defined data types, you could
still do that by defining macros and calling the macro expander on the
config file.

It's just that you would either not be able to have stuff like vectors
in a quoted list (you'd just get a list like `(VECTOR ...)` in it if
you tried), or you'd have to be expanding any macros encountered
within the quoted list.  Either both, or neither.

Not getting a choice, you say...  That's not very expressive.  That
seems like a limitation in the language.  Remember: remove the
limitations that make additional features seem necessary.

Next thing we will have two variants of quote: One which quotes for
real, and one that expands macros.  Or maybe some mechanism to mark
macros as being meant to be run inside a quote or not, but then we
re-invented runes in a different way.

Which brings me back to runes, and how `#QUOTE` could handle them,
even if the decoder is the macro expander.

Encountering `#QUOTE` could tell the decoder that while decoding the
operand, it should only honor runes, not macros bound to identifiers.

That would probably be a fine way to solve the quote problem, should
the decoder also be the macro expander: Macros are bound to runes or
identifiers, and the rune-bound macros are those that are expanded
even inside a quote.

I think that would be the same as having completely separate decode
and macro-expand phases.

(The reason we would want them merged, by the way, is that it would
presumably prevent duplication of code, since what they do is so
similar.)

It's possible that I'm agonizing for no reason at all because maybe
the decoder cannot be the macro expander anyway.

We will see.

For now, I think it's best to proceed by implementing the decoder, and
once I've come to the macro expander I can see if it makes sense to
merge the two or not.

But I'll probably keep runes one way or another, since they're a nice
way of marking things that should be processed "no matter what" such
that they can function as object literals within code.
