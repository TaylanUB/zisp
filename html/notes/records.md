# Subtyping of record types

It's a serious limitation of SRFI 9 that it doesn't allow creating
subtypes with additional fields.  This is an invaluable strategy for
representing a hierarchy of types, which are ubiquitious in real life
and thus in programming.

Sadly, this brings with it some significant complications if records
are to be initialized with default values to ensure invariants.  The
R6RS solves this with an incredibly sophisticated system, which we
might need to adopt.  (Search for "protocol" and "record-constructor
descriptor" in the R6RS.)

However, we may be able to get away with a simpler approach...

UNDER CONSTRUCTION

Are constructor protocols really that important?  Consider that all we
can do is add additional fields in the subtype.  What if we separated
allocation from initialization:

```scheme
(define-record r1
  (parent #f)
  (fields a b))

(define (init-r1! r a b)
  (set-r1-a! a)
  (set-r1-b! b))


(define-record r2
  (parent r1)
  (fields c d))

(define (init-r2! r a b c d)
  (init-r1! r a b)
  (set-r2-c! c)
  (set-r2-d! d))


(define-record r3
  (parent r2)
  (fields e f))

(define (init-r3! r a b c d e f)
  (init-r2! r a b c d)
  (set-r3-e! e)
  (set-r3-f! f))


(define r (make-r3))
(init-r3! r 1 2 3 4 5 6)
```
