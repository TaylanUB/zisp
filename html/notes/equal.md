# A novel approach to object equivalence

## Story time

In my past 5 years of developing a warehouse management application,
I've frequently found myself implementing equality the following way.

(Code massively reduced to get to the point.  Implementations of the
`hashCode` method, which must be compatible with `equals`, have been
left out for brevity.)

```java

class Warehouse {
  String code;
  String name;
  // ... more fields, many mutable

  public boolean equals(Object obj) {
    if (obj == this) {
      return true;
    }
    if (obj instanceof Warehouse w) {
      return code.equals(w.code);
    }
    return false;
  }
}

class Product {
  int id;
  LocalizedString name;
  LocalizedString description;
  // ... more fields, many mutable

  public boolean equals(Object obj) {
    if (obj == this) {
      return true;
    }
    if (obj instanceof Product p) {
      return id == p.id;
    }
    return false;
  }
}

```

And so on.  A type may have a code that is a String, an id that is an
int, or some other field that uniquely identifies members of it within
our application domain.

I'm speaking of types and their members, not classes and instances,
because I mean the application domain.  The user of our program has a
number of warehouses and a number of products, in reality, which we
represent via these classes, but there may be multiple instances of a
class representing the same real entity.

Oftentimes, there will be a database that contains unique entries for
these.  For example, an SQL table for products where the id is the
primary key, and an SQL table for warehouses where the code is the
primary key.

This way of implementing `equals()` and `hashCode()` may seem wrong to
some developers.  Since the classes have mutable fields, we may end up
with two `Product` instances which are not equal in their state, yet
`equals()` returns true for them since they represent the same real
life entity, just with different state in our program.  Perhaps one
has just been fetched from the database, whereas the other has been
modified to represent changes that are yet to be committed.

I've never found this strategy to lead to any problems.  Never have I
had a need to know whether two `Product` instances have the same state
right now.  What I care about is which product is being represented.
This is useful in various ways.

### Map keys, sets, and so on

First of all, it allows us to create maps where the keys are instances
of Product or Warehouse.  For example, the application may want to
create a map of which products each warehouse contains:

```java

Map<Warehouse, List<Product>> productsByWarehouse;

```

Given the `equals()` implementation of the Warehouse class, this map
can now be given any instance of Warehouse as a key, and we need not
ensure that we have one canonical instance used as the key.

There are of course many alternatives to this.  One may not want this
map to keep Warehouse instances alive, in which case it would be more
sensible to use the codes (Strings) for the keys.  One could also add
a field to Warehouse such as `List<Product> products;`.  However, in
some circumstances, a map such as the above may be most natural.

Other data structures and operations depend on equality checks as
well.  For example: finding duplicates in a collection, checking
whether a collection already contains a given item, managing a set
data type, and so on.  The way we implement equality is also useful
for such purposes.

### Elegance and encapsulation

Secondly, we may have subsystems in the application that communicate
by passing Product or Warehouse instances to each other.  For example,
the user may be looking at a user interface displaying the products in
a warehouse with their current quantities.  The user may then click on
an "update quantities" button which opens a sub-interface where it's
possible to add an arbitrary number of entries (products) with the
desired quantity change for each.  This sub-interface may then return
a list of Product instances with the associated quantity change.  The
code of the warehouse overview interface can now use `equals()` to
determine which instance of Product it received corresponds to which
Product instance it's already holding on to.

Again, there are of course many alternative strategies which don't
require such a use of `equals()`.  One may explicitly compare the id
fields, or the sub-interface may return ids associated with quantity
changes.  However, the `equals()` based implementation may offer the
cleanest possible code:

```java

public void receiveQuantityChanges(Map<Product, Integer> changes) {
  for (Row<Product> row : this.productRows) {
    Integer change = changes.get(row.product);
    row.quantity += change;
  }
}

```

As far as readability and elegance is concerned, this can hardly be
improved on, as far as one is familiar with Java.  Although using a
`Map<Integer, Integer>` would hardly make the code any more verbose,
it immediately causes it to lose out on self-documentation:

```java

// Quantity changes of what?  Should we rename this to the awfully
// long name receiveProductQuantityChanges to make it clear?
public void receiveQuantityChanges(Map<Integer, Integer> changes) {
  for (Row<Product> row : this.productRows) {
    Integer change = changes.get(row.product.id);
    row.quantity += change;
  }
}

```

This is a minor change as far as readability is concerned, but it also
decreases encapsulation.  The code needs to be aware that products are
represented uniquely by an id that is an integer, and changing this
implementation detail of the class may have far-reaching consequences
throughout the code base.

The self-documenting property may not apply to a Scheme-based language
without static types, but it makes the encapsulation aspect all the
more significant, since we won't have a compiler or static analyzer
which immediately tells us about a change in the type of `id`.  We
must hope that the field `id` has been removed and replaced with a
different one, not reusing the `id` identifier.  (For example, this
would mean a procedure like `product-id` stops existing, so a static
analyzer or compiler can immediately warn us about its absence.  Had
we reused the field but changed its type, we would have a very hard
time finding all the places in the code we now need to fix.)

## Get to the point!

I've explained all this to arrive at one simple conclusion:

Perhaps it would be a good idea for a language to have a mechanism in
which compound types like records or classes can simply declare that
one of the defined fields is the "unique identifier" of the type.

Imitating SRFI 9:

```scheme

(define-record-type <product>
  (make-product id name description ...)
  product?
  (identity: id)   ; <- see here
  (id product-id)
  (name product-name)
  (description product-description)
  ...)

(define-record-type <warehouse>
  (make-warehouse code name ...)
  warehouse?
  (identity: code)  ; <- see here
  (code warehouse-code)
  (name warehouse-name)
  ...)

```

Now the million dollar question is whether it should be `equal?` that
makes use of this information, or `eqv?`, or both.

Although `eqv?` is intended to approximate operational equivalence,
and should therefore not consider separate mutable objects to be the
same, it's not clear to me how useful this is on user-defined types.

The rationale for defining `eqv?` in terms of operational equivalence
is that this allows implementing memoization.  If a memoized function
depends on one of the mutable fields of a record, yet `eqv?` returns
true on separate instances with different state, then the function
will return an incorrect result.  But I'm skeptical as to whether a
function like that would see much practical use.  It seems to me like
a memoized function that is used to compute some property of an entity
in our application domain should probably depend only on immutable
properties of that object.

Another way to look at `eqv?` is that it implements "mostly constant
time" equality.  (Strictly speaking, it's a linear time operation on
numbers, assuming the implementation offers arbitrary-size numbers.
This rarely matters, as few programs make use of numbers larger than
what fits in a few dozen bytes.)  Conversely, `equal?` is responsible
for deep equality testing, which is inherently linear time, though
record types were overlooked in its definition in R7RS-small.

## "Why not neither?"

(I'm coining this as a new meme, in contrast to "why not both?")

I think I want Zisp to support the following:

* `eq?`: As in Scheme, but maybe allowed to fail on procedures.
* `eqv?`: As in Scheme.
* `equiv?`: An even closer approximation of operational equivalence,
  by diving into immutable compound data types that `eqv?` doesn't
  handle due to trying to be constant-time.  I believe this is the
  same as `equal-always?` in Racket.
* `equal?`: As in Scheme, but also dives into records because why not?

The reason `eq?` is allowed to fail on procedures is an optimization
strategy that duplicates/inlines procedures.  (citation needed)

And which one should make use of the `identity` field of user types?
Only `equiv?`, because the identity object may be a string or other
immutable but compound object.  (Thinking back to my Java programs,
I've had an extremely common type that was identified by a pair of
integers, for example: document and line number.  One can also
conceive of N-dimensional coordinates and other such examples.)

We want `eqv?` to remain "pretty much constant time" with the only
exception being numbers, which seem highly unlikely to reach such a
large size that it would matter.  Recursive data structures, and
uninterned strings, on the other hand, could become quite large and
costly to compare.
