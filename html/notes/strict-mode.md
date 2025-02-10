# Strict mode to disallow ignoring returned values

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
