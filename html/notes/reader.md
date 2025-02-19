# Reader? Decoder? I barely know 'er!

*This started from an expansion to the following, then became its own
article:*

[Symbols are strings are symbols](symbols.html)

OK but hear me out... What if there were different reader modes, for
code and (pure) data?

I want Zisp to have various neat [syntactic extensions](sugar.html)
for programming purposes anyway, like the lambda shorthand, and these
shouldn't apply to configuration files, either.  (Although they seem
unlikely to occur by accident.)

So what if the transform from string literal to quoted string literal
only occurred in code reading mode?

At least one problem remains, which is that `'(foo "bar")` would turn
into `(quote (foo (quote bar)))` because the reader would be in code
mode while reading it...

This reminds me of the long-standing annoyance in Scheme that "quote
is unhygienic" and maybe we can tackle this problem as well now.

Also, expressions like `'(foo '(bar))` always seemed weird to me, and
probably have no place in Scheme, because we don't generate code via
quote; we generate it with macros that operate on explicit syntax
objects rather than pure data.

I want to experiment with an idea like this:

    ;; "code reader mode" transformations

    '(foo bar)     -> (#quote foo bar)
    
    '(foo 'bar)    -> ERROR

    "foo"          -> (#quote . foo)

    "foo bar"      -> (#quote . "foo bar")

    '(foo "bar")   -> (#quote foo bar)

    '(foo "x y")   -> (#quote foo "x y")

The right-hand side shows what you would get if you read the form on
the left in code reader mode, then wrote it back out in data mode.

The writer could also have a code writer mode, which applies the
reverse transformations.  There should be a one to one mapping,
unambiguous, so this always works.  A "hygienic" way to quote is
imperative here, since the writer could otherwise not know whether
some `quote` keyword in a list is *the* quote special form, or just
part of some data.

We've made quote into a special token, `#quote`, to solve that.
Instead of adding a separate symbol data type that's a subtype of
strings, I think I'll add something called a "rune" or such that's
represented like `#foo` and allows for custom reader extensions, or
rather, writer extensions.

Essentially, these runes would be bound to a pair of the following:

1. A procedure that accepts a datum and returns some type of value.

2. A procedure that takes values of that type, and turns them back
   into written format.

For `#quote`, the reader procedure would be the identity function.
The writer procedure would need to be a little more sophisticated.

Note that the first procedure would not actually be called during
reading of data.  Somewhat confusingly, it would only be called in
evaluation of code.

Let's recap.  Starting with pure data reading and writing:

1. There is no special reader syntax.  This s-expression format is a
bare minimum of what's needed to represent sequential data i.e. lists
and lists are the only compound data type recognized by the reader.
Anything that isn't a list is either an atomic value, or a string
which may or may not be considered atomic depending on how pedantic
you want to be.  Oh and runes are allowed.

   A. This is basically "classic" s-expressions with runes added.
   Only lists, numbers, and strings/symbols are recognized.

   B. Heck, numbers may not be recognized.  Or maybe they will be
   limited to integers and floats, but no rationals or such, and
   reading a float will guarantee no loss of precision?

2. Writing data returned by the data reader back out, in data form,
will produce exactly what was read, with the sole exception being
whitespace differences.  The data is not allowed to contain any
non-atomic values other than proper lists.

   A. It's important not to allow floats that IEEE 754 doubles can't
   represent, since then differences between input and output would
   occur.  But what about numbers like "10.00"?  That would also
   become something else when written back out.

   B. OK, maybe numbers represented in a non-canonical way are a
   second source of difference between reading and writing back out,
   but let's at least guarantee there's no loss of precision.

(I've not considered comments.  Maybe they will be preserved?  Maybe
they should be implemented as code reader syntax sugar as well??)

And now code reading and writing:

1. Various syntax sugar is internally transformed into runes, with
non-list compound data literals (vectors, hash tables, etc.) needing
this type of representation to appear in code.

   A. Writing that data back out in data mode will reveal the inner
   workings of the language, producing output containing runes.

   B. Direct use of runes may be forbidden; not sure about this.

   C. Evaluating this data containing runes will produce, in-memory,
   the actual values being represented.  The "reader procedure" tied
   to the rune is responsible for this, though the fact that it's
   evaluation and not reading that calls that procedure makes it
   confusing so a better name is needed.  Maybe just "decoder."

2. For every data type that falls outside the pure data syntax, there
is a procedure that turns it into a canonical data representation
based on lists and atomics, always using the format `(#rune ...)`.

   A. Another procedure is capable of turning that back into reader
   sugar, but this is not terribly important.  Although it would be
   neat to be able to write out code that looks like hand-written
   program code, this really is just a bonus feature.

   B. For some types, turning them back into code without any runes
   may be highly complicated; procedures, in particular, would need
   decompilation to make this work.


## Recap (or not?)

Wow, that was a long "recap."  I actually came up with new ideas in
writing that.  Let's recap the recap.  I'll represent the mechanisms
as different pipelines that can happen using the various features.

Typical pipeline when reading and evaluating code:

    code-file --[code-reader]--> code-data --[eval]--> values
                 ^^^^^^^^^^^                  ^^^^
              turns sugar into        calls rune decoders
                 rune calls            to produce values
            i.e. desugars code          & compiles code

Reading in a [serialized program](compile.html):

    data-file --[data-reader]--> data --[eval]--> values
                                         ^^^^
                                    fairly trivial
                                (no lambdas, only runes)

Reading pure and simple data like a config file:

    data-file --[data-reader]--> data (no runes to eval)

Note that "data" is a subset of "values" basically.  And the term
"code-data" which was used above just means data that is meant to be
evaluated as code, but is totally valid as pure data.  This is not to
be confused with the "data" that existed in the intermediate step
while we were reading a serialized program; that was absent of any
forms like lambdas that need compilation.

OK, that last bit was a bit confusing, and I realize it stems from
conflating rune decoding with code compilation, so let's split that
further up.  Above, "eval" is "decode + compile" basically, but it's
possible to separate them, for example if we want to read a file of
serialized values that should not contain any code:

    values-file --[data-reader]--> values-data --[decode]--> values

This is a secure way to read complex data even if it comes from an
untrusted source.  It may contain runes that represent code, such as
in the form of `(#program "binary")` (compiled procedure) or even
`(#lambda (x) (do-things))` but so long as you don't actually call
those things after having decoded them, they can't do anything.
Decoding runes can't define macros or register new rune decoders,
meaning there's no way to achieve arbitrary code execution.

Heck, although `#lambda` exists to represent the desugaring of the
`{...}` convenience syntax, it wouldn't actually work here because
decoding runes would happen in a null-environment without any bound
identifiers, meaning that e.g. `(#lambda (x) (+ x x))` would just
raise an error during decoding, because the compiler would consider
`+` unbound.

Alternatively, instead of calling the compiler, the `#lambda` decoder
could just be a no-op that returns the same form back, but without the
rune, like `(lambda (x) (+ x x))`, because the compiler will take care
of that later.  Yeah, I think this makes more sense.  Why doesn't the
code reader directly give `(lambda ...)` for the `{...}` sugar?  Well,
actually, the `#lambda` decoder may yield a syntax object where the
first element specifically refers to the binding of `lambda` in the
default environment, so you could use `{...}` in an environment where
`lambda` is bound to something else, and you would still hygienically
get the default lambda behavior from `{...}`.  Yay!

(Wow, it's rabbit hole after rabbit hole today.  This is good though.
I'm coming up with some crazy stuff.)

It would be possible to decode "code-data" and get an internal memory
representation of an uncompiled program which however already has
various data structure literals turned into values.  This is super
obscure but for sake of completeness:

    code-file --[code-reader]--> code-data --[decode]--> code-values

(These so-called "code-values" would only ever be useful for piping
them into the compiler.  By the way, I initially used "eval" in the
example of reading a serialized program, but "decode" would have been
sufficient there.)


## Here's a well-deserved break

(There wasn't a new header in a while.  This seemed a good spot.)

Now writing pipelines.  Let's reverse the above pipelines, from the
bottom back towards eventually the first...

The reverse of the super obscure thing above:

    code-values --[encode]--> code-data --[code-writer]--> code-file

That would only ever be useful for debugging things.  Now writing a
data structure into a serialized file, without unnecessarily invoking
the decompiler:

    values --[encode]--> values-data --[data-writer]--> data-file

That gives you a file containing only data, but the data is the
encoded format of various data structures Zisp recognizes...
Actually, that may include compiled procedures as well.

Now the simple config file case, being serialized:

    data -[data writer]-> data-file

Now serializing a compiled program to a file, without decompilation:

    values --[encode]--> values-data --[data-writer]--> data-file
              ^^^^^^                    ^^^^^^^^^^^
         data structures             no decompilation
        become rune calls            or "re-sugaring"

Oh, look at that.  It's the same as writing out data structures, as
we've already seen previously...  This recap of a recap will need
another recap for sure.

And now, the full decompiler:

    values --[uneval]--> code-data --[code-writer]--> code-file
              ^^^^^^
          decompilation

Actually, just like "eval" is "decode + compile", the "uneval" here
really is "decompile + encode".


## The Revised Recap of the Recap

The following exist:

1. Readers:

   1. Data reader: Reads lists, strings/symbols, runes, integers, and
   IEEE 754 double-precision floats without loss of precision.

   2. Code reader: Reads code that can contain various syntax sugar,
   all of which has an equivalent representation with runes.

2. In-memory transformers:

   1. Decoder: Calls decoders for runes in data, to yield values.

   2. Evaluator: [Executes aka compiles](compile.html) decoded values
   into other values.[*]

3. Reverse in-memory transformers:

   1. Encoder: Reverse of the decoder. (Lossless?)

   2. Unevaluator: Reverse of the evaluator. (Lossy.)

4. Writers:

   1. Data writer: Reverse of data reader. (Lossless.)

   2. Code writer: Reverse of code reader. (Lossy?)

(*) This needs decoding to run first, because otherwise it wouldn't
    realize that you're e.g. calling `+` on a pair of rational number
    constants represented through runes, so constant folding wouldn't
    work.  Same with `vector-ref` on a vector literal represented as a
    rune, and so on.


## How in the seven hells did I arrive at this point?

Jesus Christ!

This was about symbols and strings being the same thing.

But I love these rabbit holes.  They're mind expanding and you find
out so many new things you never thought about.

Did you notice, by the way, that the code reader/writer above is
essentially a parser (and unparser) you would have in a regular
programming language, where syntax becomes an AST?  The pure data
format is basically our AST!

But this doesn't mean we lost homoiconicity.  No, we merely expanded
upon it by providing a more detailed explanation of the relationship
between textual representation of code and in-memory data that exists
at various stages before ultimate compilation.

Oh, and did we achieve our strategy of strings = symbols now, or does
it need to be dropped?  I think we achieved it.  The code reader, as
described all the way up where the section "Reconsidering AGAIN"
begins --in the original article; see top-- will desugar string
literals into:

    "foo" -> (#quote foo)

(As already described originally...)

And the `#quote` rune?  Well, it will not actually just return its
operand verbatim, no!  It will return a syntax object that's a list
with the first element specifically refers to the binding of `quote`
from the standard library.  In other words, it's the evaluator that
actually implements quote, not the decoder.

Oh yes, this is very satisfying.  Everything is coming together.

Syntax objects, by the way, will also have a rune-based external
representation, so you can inspect the result of macro expansion.

And yes, I think using runes directly in code mode should be illegal,
because it allows referring to bindings in the standard library, or
even bindings in arbitrary libraries by crafting syntax objects
represented via runes, to bypass environment limits.

That bug actually existed in Guile at some point, where one could
craft syntax objects, represented as vector literals, to refer to
bindings in other modules, making it impossible to run code in a
sandboxed environment.  (It was fixed long ago, I believe.)

Oh, but what about `#true` and `#false`?  OK, maybe there will be a
whitelist of runes that are allowed in code.  That makes sense.

We will see.  Still more details to be fleshed out.

In any case, some runes must be able to declare that they don't take
arguments, in which case `(#rune ...)` isn't decoded by passing the
entire form to the decoder of `#rune`, but rather treated as a normal
list whose first element is decoded as a nullary rune.  That's how
boolean literals in code will be implemented.


## Looking at more of the initial problems

What happened to `'(quote "foo")` in code mode being weird?  Well,
encountering an apostrophe tells the code reader that the next
expression is a datum, so it switches to data mode for that.

Wow, that was easy.

This also means you can't use syntax sugar inside it, which is good
because as we said previously, we don't want to use quoting to create
code; we want to use syntax objects for that.

This is really orthogonal to the whole runes issue, and could have
been solved without that mechanism, but I'm happy I came up with it
because it resolves hygiene issues.

The syntax `#'(quote "foo")` would be sugar for a different rune, and
the reader would remain in code mode, further desugaring any sugar
found within, so this works: `#'{x (+ x x)}`

Oh and I mentioned reader extensions (for code mode) but then didn't
expand on that.  Well, whenever the code reader encounters this:

    #foo(blah blah blah)

It will turn that into:

    (#foo blah blah blah)

After which the decoder for `#foo` will be invoked, which could have
been registered by the programmer.

Can that registration be done in the same file though?  Normally, the
execution step comes after decoding, and we decided that we don't want
to allow arbitrary code execution to happen just when reading a data
file and decoding it.  So something exceptional would need to happen
for this to work.  Or maybe not.

Remember that [compilation is execution](compile.html) in Zisp,
meaning that compiling a file looks like this in pseudo-Scheme:

    (define env (null-environment))    ;start out empty
    
    (while (not (eof? input))
      (let* ((datum (read-code input)) ;desugar
             (value (decode datum)))   ;decode
        (eval! value env)))            ;eval in mutable env

    (write (env-lookup env 'main))     ;serialize

I've called eval `eval!` to indicate that it can mutate the env it
receives, which is what import statements and defines would do.

Let's modify that a little further to indicate the fact that reader
macros, or in our terms, custom rune decoders, can be defined in the
middle of the code file by affecting the environment:

    (define env (null-environment))      ;start out empty
    
    (while (not (eof? input))
      (let* ((datum (read-code input))   ;desugar
             (value (decode datum env))) ;decode in env
        (eval! value env)))              ;eval in mutable env

    (write (env-lookup env 'main))       ;serialize

Since the `decode` procedure is given an environment, it will look up
decoders from therein.  So, after the evaluation of each top-level
expression, the expressions coming after it could be using a custom
decoder.

What our reader macros cannot do is completely affect the lexical
syntax of the language, as in, add more sugar.  You must rely on the
global desugaring feature of `#x(...) -> (#x ...)` which, now that I
think about it, is completely useless because a regular macro could
have achieved exactly the same thing.

OK, let's try that again.  The global desugaring wouldn't work on
lists only, it would work on a number of things:

    #x"foo"    -> (#x #string . foo)

    #x[foo]    -> (#x #square . foo)

    #x{foo}    -> (#x #braces . foo)

You get the idea!

(I've changed my mind that `"foo"` should desugar into a call to the
regular `#quote` rune; it should be `#string` instead to disambiguate
from the apostrophe if needed.)

Also, all those would work without a rune as well, to allow a file to
change the meaning of some of the default syntax sugar if desired:

    "foo"    -> (#string . foo)

    [foo bar]    -> (#square foo bar)

    {foo bar}    -> (#braces foo bar)

Or something like that.  I'm making this all up as I go.
