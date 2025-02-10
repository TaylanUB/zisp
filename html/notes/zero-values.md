# Return zero values when there's nothing to return

This is only a minor point:

It's a long-running pet peeve of mine that R5RS Scheme specifies "an
unspecified value" to be returned when there's nothing meaningful to
return.  It's a remnant from before we had the ability to return
multiple values, and should be eliminated.

Any operation that has nothing meaningful to return, will return zero
values in Zisp, and no more.
