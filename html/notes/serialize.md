# Everything can be serialized

Let's look at the code mentioned in [compilation](compile.html) again:

```scheme

;; The fictional file `lookup-table.dat` would be in the source code
;; repository, and the fictional procedure `process-data` would read
;; it and return a data structure.
(define lookup-table (process-data "lookup-table.dat"))

```

(In Zisp, this would be executed at compile-time, so the lookup table
becomes part of the compiled program.)

If you're familiar with Guile --and I suspect most implementations of
Scheme have a similar limitation-- then you may have noticed an issue.

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
