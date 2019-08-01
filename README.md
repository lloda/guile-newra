
# ![(travis build status)](https://travis-ci.org/lloda/guile-newra.svg?branch=master) #

## Newra

guile-newra (`newra`) wants to replace the old (2.2/3.0) Guile array system, which is mostly implemented in C.

The new implementation should be at least as fast. I think this is feasible once the Scheme compiler goes native, because for the most part the array functions are used to call back to Scheme, and a Scheme implementation could get rid of the back and forth, optimize the type dispatches, etc.

The C API shouldn't be affected. Once you get an array handle it makes no sense to use the Scheme array functions anyway, since the element layout is totally transparent.

## Status

The old array compatibility layer is mostly finished, with only a naming change (`array-xxx` becomes `ra-xxx`). The `newra` versions of the `map` and `for-each` functions are significantly faster already, but the `-ref` functions are a bit slower and some of the functions that have a fast path in C, such as `array-fill!` or `array-copy!`, can be lot slower in `newra`, depending on the types of the arguments.

These issues seems fixable, and besides, the Scheme compiler is only improving as Guile 3.0 aproaches.

Compared with the old arrays, `newra` already offers free index vectors and applicable-settable arrays. And if a `newra` operation takes too long, you can actually interrupt it :p

I'm now drafting some higher level functionality, which can be followed in the `TODO` file.

## Transition guide

Originally I wanted `newra` to be a drop-in replacement for the old array system, reusing the same function names and all. Now I think it's better to have a parallel system where some of the flaws of old system can be cleaned up. Still it's important that programs using the old system can be easily ported to the new one.

With that in mind, here is what you'd have to change. Note that the `ra-` names are not final, and neither is the `#%` read syntax. I'm not sure yet how the old array syntax will be absorbed — maybe old array objects will be converted transparently for a while.

* The read syntax is like that of the old system except for an extra `%`, so `#2f64((1 2) (3 4))` becomes `#%2f64((1 2) (3 4))`. The compiler doesn't support the new literal type yet. You can work around this like `(string->ra "#%2f64((1 2) (3 4))")`.

* The functions `ra->array` and `array->ra` are provided to convert between the old and the new array types. Neither of these functions copy the contents of the array, so `(let ((o (make-array 3))) (eq? (shared-array-root o) (shared-array-root (ra->array (array->ra o)))))` returns `#t`. Note that some of the new `ra` types aren't convertible in this manner; for example `(ra->array (ra-iota 3))` is an error.

* The new system matches sizes strictly. For instance `(array-map! (make-array #f 2) - (make-array #t 3))` succeeds, but `(ra-map! (make-ra #f 2) - (make-ra #t 3))` fails with a shape mismatch error.

* The new system still supports non-zero base indices, but I'd advise against using them, because they aren't worth what they cost and I'm tempted to get rid of them.

* For most of the old functions there is an equivalent new one where you just rename `array-xxx` to `ra-xxx`. Two exceptions:

  + The equivalent of `transpose-array` is `ra-transpose`.
  + The equivalent of `(array-copy! src dst)` is `(ra-copy! dst src)`. This follows `array-map!`/`ra-map!` and `array-fill!`/`ra-fill!` which both use the first argument as destination.

* Most `ra-` functions try to return something useful even when the corresponding `array-` functions do not. For example `(array-fill! (make-array 0 3) 4)` returns `*unspecified*`, but `(ra-fill! (make-ra 0 3) 4)` returns `#%1:3(4 4 4)`.

* `truncated-print` doesn't support `ra` types, you'll get a lone `#` if truncation is necessary at all.

* The default writer defaults to printing all the sizes of the array, so `(ra-i 3 2)` prints as `#%2:3:2((0 1) (2 3) (4 5))`. Note that `#2:3:2((0 1) (2 3) (4 5))` is a valid read syntax in the old system, just not the default.
