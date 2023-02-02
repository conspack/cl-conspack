# News

**Recent changes**:

* Decoder now handles more uses of circular reference.
* Properties now require `WITH-PROPERTIES` if used outside an `ENCODE`
  or `DECODE`, see below.

# cl-conspack

CONSPACK was inspired by MessagePack, and by the general lack of
features among prominent serial/wire formats:

* JSON isn't terrible, but can become rather large, and is potentially
  susceptible to READ exploits (as per the recently-fixed
  "10e99999999" bug in SBCL).

* BSON (binary JSON) doesn't really solve much; though it encodes
  numbers, it's not particularly smaller or more featureful than JSON.

* MessagePack is small, but lacks significant features; it can
  essentially encode arrays or maps of numbers, and any interpretation
  beyond that is up to the receiver.

* Protobufs and Thrift are static.

It should be noted that, significantly, **none** of these support
references.  Of course, references can be implemented at a higher
layer (e.g., JSPON), but this requires implementing an entire
additional layer of abstraction and escaping, including rewalking the
parsed object hierarchy and looking for specific signatures, which can
be error-prone and hurts performance.

Additionally, none of these appear to have much in the way of
security, and communicating with an untrusted peer is probably not
recommended.

CONSPACK, on the other hand, attempts to be a more robust solution:

* Richer set of data types, differentiating between arrays, lists,
  maps, typed-maps (for encoding classes/structures etc), numbers,
  strings, symbols, and a few more.

* Very compact representation that can be smaller than MessagePack.

* In-stream references, including optional forward references, which
  can allow for shared or circular data structures.  Additionally,
  remote references allow the receiver the flexibility to parse and
  return its own objects without further passes on the output.

* Security, including byte-counting for (estimated) maximum output
  size, and the elimination of circular data structures.

* Speed. Using [fast-io](https://github.com/rpav/fast-io) encoding
  and decoding can be many times faster than alternatives, even
  *while* tracking references (faster still without!).

See [SPEC](https://github.com/conspack/cl-conspack/blob/master/doc/SPEC) for
complete details on encoding.

## Usage

`cl-conspack` is simple to use:

```lisp
(encode '(1 2 3)) ;; => #(40 4 16 1 16 2 16 3 0)

(decode (encode '(1 2 3))) ;; => (1 2 3)

;; Smaller if the element-type is known:
(encode (fast-io:octets-from '(1 2 3)))
  ;; => #(36 3 20 1 2 3)
```

### CLOS and general objects

Conspack provides the ability to serialize and deserialize objects of
any kind.

The easiest way, for the common case:

```lisp
(conspack:defencoding my-class
  slot-1 slot-2 slot-3)
```

This expands to the more flexible way, which specializes
`ENCODE-OBJECT` and `DECODE-OBJECT-INITIALIZE`:

```lisp
(defmethod conspack:encode-object append
    ((object my-class) &key &allow-other-keys)
  (conspack:slots-to-alist (object)
    slot-1 slot-2 slot-3 ...))

(defmethod conspack:decode-object-initialize progn
    ((object my-class) class alist &key &allow-other-keys)
  (declare (ignore class))
  (alist-to-slots (alist object)
    slot-1 slot-2 slot-3))
```

`ENCODE-OBJECT` should specialize on the object and return an alist.
The alist returned will be checked for circularity if `tracking-refs`
is in use.

`DECODE-OBJECT-ALLOCATE` should specialize on `(eql 'class-name)`, and
produce an object *based* on the class and alist.

`DECODE-OBJECT-INITIALIZE` should specialize on the object (which has
been produced by `DECODE-OBJECT-ALLOCATE`), and initializes it.
This two step process is necessary to handle circularity correctly.

As you can see, this does not require objects be in any particular
format, or that you store any particular slots or values.  It does not
specify how you restore an object.

But for the "normal" case, `SLOTS-TO-ALIST` and `ALIST-TO-SLOTS` are
provided to build and restore from alists, and `DEFENCODING` can
define all of this in one simple form.

### Circularity and References

Circularity tracking is not on by default, you can enable it for a
particular block of `encode`s or `decode`s by using `tracking-refs`:

```lisp
(tracking-refs ()
  (decode (encode CIRCULAR-OBJECT)))
```

"Remote" references are application-level references.  You may encode
a reference using an arbitrary object as a descriptor:

```lisp
(encode (r-ref '((:url . "http://..."))))
```

When decoding, you may provide a function to handle these:

```lisp
(with-remote-refs (lambda (x) (decode-url x))
  (decode OBJECT))
```

### Indexes

If you have a relatively small static set of symbols you will always
use for a particular encoding/decoding, you may want to use
*indexes*.  These allow symbols to be very-tightly-packed: for up to
15 symbols, a single byte can encode the symbol!  For up to 256, two
bytes, and so on.

Trivially:

```lisp
(cpk:with-index (specifier-1 specifier-2 specifier-3)
  (cpk:encode '(specifier-1 specifier-2 specifier-3)))

;; => #(40 4 176 177 178 0)

;; Contrast this with:

(cpk:encode '(specifier-1 specifier-2 specifier-3))
;; #(40 4 130 64 11 83 80 69 67 73 70 73 69 82 45 49 129 64 16 67 79
;; 77 77 79 78 45 76 73 83 80 45 85 83 69 82 130 64 11 83 80 69 67 73
;; 70 73 69 82 45 50 129 64 16 67 79 77 77 79 78 45 76 73 83 80 45 85
;; 83 69 82 130 64 11 83 80 69 67 73 70 73 69 82 45 51 129 64 16 67 79
;; 77 77 79 78 45 76 73 83 80 45 85 83 69 82 0)
```

(This is a somewhat excessive example, since long non-keyword symbols
are used.  Shorter keyword symbols would be relatively shorter, but
this is the general case.)

For more "realistic" use, you may *define* an index and refer to it:

```lisp
(define-index index-name
  symbol-1 symbol-2 ...)

(with-named-index 'index-name
  (encode ...))
```

For instance, you may define multiple indexes for multiple different
format versions, read the version, and use the appropriate index:

```lisp
(define-index version-1 ...)
(define-index version-2 ...)

(let ((version (decode-stream s)))
  (with-named-index version
    ;; Decode the rest of the stream appropriately.  You may want to
    ;; do more checking on VERSION if security is required...
    (decode-stream s)))
```

Note that using `tracking-refs` will *also* help encode symbols
efficiently, but not *quite* as efficiently:

* The full string for the symbol (and if necessary, package), will be
  encoded at least once, when first encountered
* Refs are tracked in-order, and may lead to longer tags than a
  comparable index would use

However, `tracking-refs` is a perfectly suitable option, especially if
flexibility is desired, since all symbol information is encoded, and
nothing special is needed for decoding.

### Properties

(Properties now require a `WITH-PROPERTIES` block in some
circumstances, see below.)

Properties are a way to specify additional information about an object
that may be useful at decode-time.  For instance, while hash tables
are supported as maps, there are no bits to specify the `:test`
parameter, so decoding a hash table of strings would produce a useless
object.  In this case, the `:test` property is set when encoding and
checked when decoding hash tables.

You may specify arbitrary properties for arbitrary objects; the only
restriction is the objects must test by `EQ`.

```lisp
(conspack:with-properties ()
  (let ((object (make-instance ...)))
    (setf (property object :foo) 'bar)
    (property object :foo))) ;; => BAR
```

This sets the `:foo` property to the symbol `bar`, and it is encoded
along with the object.  Note this will increase the object size, by
the amount required to store a map of symbols-to-values.

When decoding, you can access properties about an object via
`*current-properties*`:

```lisp
(defmethod decode-object-initialize (...)
  (let ((prop (getf *current-properties* NAME)))
    ...))
```

You may remove them with `remove-property` or `remove-properties`.

**Properties are now only available within a `WITH-PROPERTIES`
block.** This has a number of benefits, including some thread safety,
and ensuring properties don't stick around forever.

`ENCODE` and `DECODE` have **implicit** `WITH-PROPERTIES` blocks: you
don't need to specify `WITH-PROPERTIES` if you use properties inside
`ENCODE-OBJECT`, `DECODE-OBJECT`, or encode and decode any objects
that have implicit properties.  You only need this if you wish to
access properties *outside* of the encode or decode (e.g.,
preassigning properties to be encoded).

### Allocation Limits and Security

Conspack provides some level of "security" by *approximately* limiting
the amount of bytes allocated when reading objects.

By default, because format sizes are prespecified statically, it's
possible to specify extremely large allocations for e.g. arrays with
only a few bytes.  Obviously, this is not suitable for untrusted
conspack data.

The solution is simply to cap allocations:

```lisp
(with-conspack-security (:max-bytes 200000)
  (decode ...))
```

Since actual allocation sizes are rather difficult to get in most
lisps, this *approximates* the allocation based on how big each object
might be, e.g.:

* `pointer-size * array-size`
* `string-length`
* `number-size`
* etc.

Each object header is tallied against the limit just prior to its
decoding; if the object would exceed the allowed bytes, decoding halts
with an error.

Further options may be added in the future.

### Interning

By default, Conspack does not intern symbols, on the assumption that
the producer and consumer have agreed on what symbols to use
beforehand. If the decoder finds a symbol that has not already been
interned, it will ignore the symbol's package and make an uninterned
symbol instead.

The `with-interning` macro can be used if the decoder should instead
intern symbols:

```lisp
(with-interning ()
  (decode ...))
```

Interning symbols from untrusted data could lead to denial-of-service
attacks via interning long-lived symbols in memory, so be careful.

## Explaining

Since conspack is a binary format, it's rather difficult for humans to
read just looking at the stream of bytes.  Thus an `EXPLAIN` feature
is provided.  This is mostly useful for debugging the format; however
it may be of interest otherwise and certainly may be helpful when
creating other implementations.

For instance:

```lisp
(explain (encode '(1 2 3)))

;; =>
((:LIST 4
  ((:NUMBER :INT8 1) (:NUMBER :INT8 2) (:NUMBER :INT8 3) (:BOOLEAN NIL)))
 END-OF-FILE)
```
