
```
$FORTRAN -shared -fPIC -o libexample.so example.f90 -std=f2018 -fcheck=all -Wall
$CXX -o ffi ffi.cc -std=c++20 -Wall -Wl,-rpath . -L . -lexample -lgfortran
./ffi
$GUILE -L ../mod ffi.scm
```
