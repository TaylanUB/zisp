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
