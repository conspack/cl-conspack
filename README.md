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
layer (e.g., JSPON), but this requires implemeting an entire
additional layer of abstraction and escaping, including rewalking the
parsed object hierarchy and looking for specific signatures, which can
be error-prone, and hurt performance.

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

* Speed, using [fast-io](https://github.com/rpav/fast-io), encoding
  and decoding can be many times faster than alternatives, even
  *while* tracking references (faster still without!).

See [SPEC](https://github.com/conspack/cl-conspack/doc/SPEC) for
complete details on encoding.

## Example

`cl-conspack` is simple to use:

```lisp
(encode '(1 2 3)) ;; => #(40 4 16 1 16 2 16 3 0)

(decode (encode '(1 2 3))) ;; => (1 2 3)

;; Smaller if the element-type is known:
(encode (fast-io:octets-from '(1 2 3)))
  ;; => #(36 3 20 1 2 3)
```
