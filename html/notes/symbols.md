# Symbols are strings are symbols

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


## But but but

(Late addition because I didn't even notice this problem at first.
How embarrassing!)

But if symbols and strings are the same thing at the reader level,
then how on earth would you have a string literal in source code,
without it being evaluated as a variable?

    (display "bar")  ;should this look up the variable 'bar'?!
    (display bar)    ;should this display the string 'bar'?!

There's actually a simple solution.

The syntax `"string"`, with double-quotes and nothing else, becomes
reader syntax akin to the apostrophe, and expands to:

    (quote #"string")

And `#"string"` is the real syntax for string literals, which are
always treated as identifiers by the evaluator.

Bare identifiers like `foo` instead directly become `#"foo"`, without
the wrapping `(quote ...)`, and are thus evaluated.

This also means that manually writing `#"string"` in your source code
allows that to be used as an identifier regardless of whether it has
illegal characters in it, essentially doing what `|string|` does in
R7RS-small.

Let's sum it up; here's the reader transformations:

    foo        -> #"foo"
    "foo"      -> (quote #"foo")
    "foo bar"  -> (quote #"foo bar")
    #"foo bar" -> #"foo bar"

Some pseudo-code based on Scheme:

    (let ((#"all your" "base ")
          (#"are belong" "to us"))
      (display
       (string-append #"all your" #"are belong")))

That prints: "base to us"

I'm not married to the syntax `#"string"` and may end up using the
simpler `|foo|` in the end.  It doesn't really matter.


## More problems

Dangit, couldn't have been so easy could it?

What if you have a configuration file with these contents:

    ((job-name "Clear /tmp directory")
     (interval reboot)
     (command "find /tmp -mindepth 1 -delete"))

Now all those "string constants" will turn into lists with the string
"quote" at its head.  Terrible.  One could write it with the explicit
string literal syntax `#"foo"` for strings, but that's also terrible.


## Salvageable

I'm not yet done with this idea.  What if strings simply have a flag
that says whether they are intended as a symbol or not?

While reading, it would be set automatically.  Instead of `intern`,
one would call a function like `symbol`, which would return a string
with the flag set, after interning it if necessary; it would simply
return the original string if it already had the flag set.

Another way to look at this is that strings and symbols are sort of
"polymorphic" and can be used interchangeably.  I don't want to get
into JavaScript style automatic type conversions (yuck) but this may
simply be a flag that's set on a string, which makes it a subtype of
regular strings.

Yes yes, I think that's good.  I even still have enough space left in
the NaN-packing strategy to put a tag on "short strings" which are our
6-byte immediate strings.


## Reconsidering AGAIN

*This got too long and off-topic so it continues here:*

[Reader? Decoder? I barely know 'er!](reader.html)
