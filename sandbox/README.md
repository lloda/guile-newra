
```
$FORTRAN -shared -fPIC -o libexample.so example.f90 -std=f2018 -fcheck=all -Wall
$CXX -o ffi ffi.cc -Wl,-rpath . -L . -lexample -std=c++20 -Wall
./ffi
$GUILE -L ../mod ffi.scm
```
