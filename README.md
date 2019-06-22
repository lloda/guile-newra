
guile-newra (`newra`) wants to replace the current (2.2/3.0) Guile array system, which is mostly implemented in C.

The new implementation should be at least as fast. I think this is feasible once the Scheme compiler goes native, because for the most part the array functions are used to call back to Scheme, and a Scheme implementation could get rid of the back and forth, optimize the type dispatches, etc.

The C API shouldn't be affected. Once you get an array handle it makes no sense to use the Scheme array functions anyway, since the element layout is totally transparent.

The current status is that the old array compatibility layer is mostly finished, with only a naming change (`array-etc` becomes `ra-etc`). The `newra` versions of the `map`/`for-each` functions are significantly faster already, but the `-ref` are slower and some of the functions that have a fast path in C, such as `array-fill!` or `array-copy!`, are a lot slower in `newra`. This all seems fixable, and besides the Scheme compiler is only improving as Guile 3.0 aproaches.

Compared with the old arrays, `newra` already offers free index vectors and applicable-settable arrays. And if a `newra` operation takes too long, you can actually interrupt it.

I'm now drafting some higher level functionality, which can be followed in the `TODO` file. 
