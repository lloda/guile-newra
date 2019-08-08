# ![(travis build status)](https://travis-ci.org/lloda/guile-newra.svg?branch=master) #

## Newra

guile-newra (`newra`) wants to replace the old (2.2/3.0) Guile array system, which is mostly implemented in C.

The new implementation should be at least as fast. I think this is feasible once the Scheme compiler goes native, because for the most part the array functions are used to call back to Scheme, and a Scheme implementation could get rid of the back and forth, optimize the type dispatches, etc.

The C API shouldn't be affected. Once you get an array handle it makes no sense to use the Scheme array functions anyway, since the element layout is totally transparent.

Except for the tests and for the pair of functions `ra->array` / `array->ra`, `newra` is independent of the old array system.

## Status

The old array compatibility layer is mostly finished, with only a naming change (`array-xxx` becomes `ra-xxx`). The `newra` versions of the `map` and `for-each` functions are significantly faster already, but the `-ref` / `-set!` functions are a bit slower and some of the functions that have a fast path in C, such as `array-fill!` or `array-copy!`, can be a lot slower in `newra`, depending on the types of the arguments.

These issues seems fixable, and besides, the Scheme compiler is only improving as Guile 3.0 aproaches.

Compared with the old arrays, `newra` offers a growing list of features:

* Applicable arrays: `((array->ra #(1 2 3 4)) 3)` returns `4`.
* Settable arrays: `(set! ((make-ra #f 2 3) 1 1) 99)` returns `#%2:2:3((#f #f #f) (#f 99 #f))`.
* Lazy index vectors (`ra-iota`, `ra-i`). These may be infinite: `((ra-iota #f 1) (- #e1e20 1))` returns `100000000000000000000`.
* Rank extension by prefix matching: `(ra-map! (make-ra 'x 2 3) + (ra-i 2 3) (ra-iota 2 0 10))` returns `#%2:2:3((0 1 2) (13 14 15))`. Prefix matching supports undefined dimensions; the previous expression and `(ra-map! (make-ra 'x 2 3) + (ra-i #f 3) (ra-iota #f 0 10))` are equivalent.
* Generalized transpose: axes not mentioned in the transposed axis list become axes with undefined size and zero step (‘dead’ axes). This can be used for broadcasting. For example, given `(define I (ra-iota #f 1))` and `(define J (ra-transpose (ra-iota #f 1) 1))`, then `(ra-map! (make-ra 'x 10 10) * I J)` is a multiplication table.
* Since `newra` is written entirely in Scheme, if a `newra` operation takes too long, you can actually interrupt it, which is not the case in the old system.

I'm now drafting some higher level functionality, which can be tracked in the `TODO` file.

## Transition guide

Originally I wanted `newra` to be a drop-in replacement for the old array system, reusing the same function names and all. Now I think it's better to have a parallel system where some of the flaws of old system can be cleaned up. Still it's important that programs can be easily ported to the new system.

With that in mind, here is what you'd have to change. Note that the `ra-` names are not final, and neither is the `#%` read syntax. I'm not sure yet how the old array syntax will be absorbed — maybe old array objects will be converted transparently for a while. Some of these are bugs that will eventually be fixed.

* The functions `ra->array` and `array->ra` are provided to convert between the old and the new array types. Neither of these functions copy the contents of the array, so `(let ((o (make-array 3))) (eq? (shared-array-root o) (shared-array-root (ra->array (array->ra o)))))` returns `#t`. Note that some of the new `ra` types aren't convertible in this manner; for example `(ra->array (ra-iota 3))` is an error.

* The new system matches sizes strictly. For instance `(array-map! (make-array #f 2) - (make-array #t 3))` succeeds, but `(ra-map! (make-ra #f 2) - (make-ra #t 3))` fails with a shape mismatch error.

* The new system still supports non-zero base indices, but I'd advise against using them, because they aren't worth what they cost and I'm tempted to get rid of them.

* For most of the old functions `array-xxx`, the equivalent function in `newra` is `ra-xxx`. Exceptions:

  + The equivalent of `shared-array-root` is `ra-root`.
  + The equivalent of `shared-array-offset` is `ra-offset`.
  + The equivalent of `make-shared-array` is `make-ra-shared`.
  + The equivalent of `transpose-array` is `ra-transpose`.
  + The equivalent of `(array-copy! src dst)` is `(ra-copy! dst src)`. This follows `array-map!` / `ra-map!` and `array-fill!` / `ra-fill!` which both use the first argument as destination.

* Most `ra-` functions try to return something useful even when the corresponding `array-` functions do not. For example `(array-fill! (make-array 0 3) 4)` returns `*unspecified*`, but `(ra-fill! (make-ra 0 3) 4)` returns `#%1:3(4 4 4)`.

* The default writer defaults to printing all the sizes of the array, so `(ra-i 3 2)` prints as `#%2:3:2((0 1) (2 3) (4 5))`. Note that `#2:3:2((0 1) (2 3) (4 5))` is a valid read syntax in the old system, just not the default.

## Some obvious defects

* The read syntax is like that of the old system except for an extra `%`, so `#2f64((1 2) (3 4))` becomes `#%2f64((1 2) (3 4))`. The compiler doesn't support the new literal type yet. You can work around this using the reader (`(call-with-input-string "#%2f64((1 2) (3 4))" read)`) or e.g. `(list->ra 'f64 2 '((1 2) (3 4)))`.

* The default reader and printer don't handle undefined size arrays as well as they could. For example `(ra-transpose (ra-i 2) 1)` prints as `#%2:d:2((0 1))`, but this cannot be read back. `(ra-iota #f)` prints as `#%1:f`.

* `truncated-print` doesn't support `newra` types, you'll get a lone `#` if truncation is necessary at all.

* `equal?` doesn't support `newra` types, so it does just `eqv?`.
