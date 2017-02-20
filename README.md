
guile-newra wants to replace the current (2.2) Guile array system, which is mostly implemented in C.

The new implementation should be at least as fast. I think this is feasible once the Scheme compiler goes native, because for the most part the array functions are used to call back to Scheme, and a Scheme implementation could get rid of the back and forth, optimize the type dispatches, etc.

On top of that I want some convenience features such as arrays being applicable/settable, 'nonstrict arrays' (a form of drag-along, cf https://docs.racket-lang.org/math/array_nonstrict.html), etc.

The C API shouldn't be affected. Once you get an array handle it makes no sense to use the Scheme array functions anyway, since the element layout is totally transparent.

It's early days, nothing usable yet.