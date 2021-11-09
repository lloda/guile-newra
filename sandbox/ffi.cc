// -*- mode: c++; coding: utf-8 -*-
// Fortran FFI sandbox, sanity checks

// $CXX -o ffi ffi.cc -Wl,-rpath . -L . -lexample -std=c++20

// (c) Daniel Llorens - 2021
// This library is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3 of the License, or (at your option) any
// later version.

#include <ISO_Fortran_binding.h>
#include <iostream>
#include <type_traits>

using std::cout, std::endl;

extern "C"
{
    double lookup_xy (double const * x, double const * y, CFI_cdesc_t * cdesc);
}

int
main()
{
    constexpr int n0 = 3;
    constexpr int n1 = 4;
    double data[n0][n1] = {{ 0, 1, 2, 3 }, { 4, 5, 6, 7 }, { 8, 9, 10, 11 }};

    CFI_CDESC_T(CFI_MAX_RANK) cdesc;
    CFI_cdesc_t * pcdesc = (CFI_cdesc_t *) &cdesc;

    pcdesc->base_addr = data;
    pcdesc->elem_len = sizeof(double);
    pcdesc->version = CFI_VERSION;
    pcdesc->rank = 2;
    pcdesc->attribute = CFI_attribute_pointer;
    pcdesc->type = CFI_type_double;
    pcdesc->dim[0] = { .lower_bound = 0, .extent = n0, .sm = n1*sizeof(double) };
    pcdesc->dim[1] = { .lower_bound = 0, .extent = n1, .sm = 1*sizeof(double) };

    cout << "type value " << pcdesc->type << endl;

    cout << pcdesc << endl;
    cout << "base_addr " << ((char*)(&(pcdesc->base_addr)) - (char *)(pcdesc)) << endl;
    cout << "elem_len " << ((char*)(&(pcdesc->elem_len)) - (char *)(pcdesc)) << endl;
    cout << "version " << ((char*)(&(pcdesc->version)) - (char *)(pcdesc)) << endl;
    cout << "rank " << ((char*)(&(pcdesc->rank)) - (char *)(pcdesc)) << endl;
    cout << "attribute " << ((char*)(&(pcdesc->attribute)) - (char *)(pcdesc)) << endl;
    cout << "type " << ((char*)(&(pcdesc->type)) - (char *)(pcdesc)) << endl;
    cout << "dim0 " << ((char*)(&(pcdesc->dim[0])) - (char *)(pcdesc)) << endl;
    cout << "dim1 " << ((char*)(&(pcdesc->dim[1])) - (char *)(pcdesc)) << endl;

    cout << pcdesc->rank << endl;
    double x(1);
    double y(2);
    cout << lookup_xy(&x, &y, pcdesc) << endl;

    return 0;
}
