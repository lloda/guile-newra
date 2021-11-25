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
#include <cassert>

using std::cout, std::endl;

extern "C"
{
    double lookup_xy(double const * x, double const * y, CFI_cdesc_t * cdesc);
    void fillerf32(CFI_cdesc_t *);
}

int
main()
{
    cout << "---- check constants" << endl;
    {
        cout << "CFI_type_float_Complex " << CFI_type_float_Complex << endl;
        cout << "CFI_type_double_Complex " << CFI_type_double_Complex << endl;
    }
    cout << "---- rank 2 args, scalar return" << endl;
    {
        constexpr int n0 = 3;
        constexpr int n1 = 4;
        double data[n0][n1] = {{ 0, 1, 2, 3 }, { 4, 5, 6, 7 }, { 8, 9, 10, 11 }};

        CFI_CDESC_T(CFI_MAX_RANK) cdesc;
        auto pcdesc = (CFI_cdesc_t *) &cdesc;

        pcdesc->base_addr = data;
        pcdesc->elem_len = sizeof(double);
        pcdesc->version = CFI_VERSION;
        pcdesc->rank = 2;
        pcdesc->attribute = CFI_attribute_pointer;
        pcdesc->type = CFI_type_double;
        pcdesc->dim[0] = { .lower_bound = 0, .extent = n0, .sm = n1*sizeof(double) };
        pcdesc->dim[1] = { .lower_bound = 0, .extent = n1, .sm = 1*sizeof(double) };

        double x(1);
        double y(2);
        cout << "in C: " << lookup_xy(&x, &y, pcdesc) << endl;
    }
// after https://fortran-lang.discourse.group/t/iso-c-binding-pass-an-array-from-c-to-fortran-edit-python-interop-content/514/4
    cout << "---- inout array " << endl;
    {
        int N = 4;
        float data[N] = { 9, 8, 7, 6 };

        CFI_CDESC_T(1) cdesc;
        CFI_index_t ext[1] = { 4 };
        auto pcdesc = (CFI_cdesc_t *) &cdesc;
        assert(CFI_SUCCESS == CFI_establish(pcdesc, data, CFI_attribute_pointer, CFI_type_float, 0, 1, ext));

        cout << "in C, pre call " << (float *)data << endl;
        for (int i=0; i<N; ++i) {
            cout << data[i] << ", ";
        }
        cout << endl;
        fillerf32(pcdesc);
        cout << "in C, post call "  << (float *)data << endl;
        for (int i=0; i<N; ++i) {
            cout << data[i] << ", ";
        }
        cout << endl;
    }
    cout << "---- inout array, filling the descriptor manually" << endl;
    {
        int N = 4;
        float data[N] = { 9, 8, 7, 6 };

        CFI_CDESC_T(CFI_MAX_RANK) cdesc;
        auto pcdesc = (CFI_cdesc_t *) &cdesc;

        pcdesc->base_addr = data;
        pcdesc->elem_len = sizeof(float);
        pcdesc->version = CFI_VERSION;
        pcdesc->rank = 1;
        pcdesc->attribute = CFI_attribute_pointer;
        pcdesc->type = CFI_type_double;
        pcdesc->dim[0] = { .lower_bound = 0, .extent = 4, .sm = sizeof(float) };

        cout << "in C, pre call " << (float *)data << endl;
        for (int i=0; i<N; ++i) {
            cout << data[i] << ", ";
        }
        cout << endl;
        fillerf32(pcdesc);
        cout << "in C, post call "  << (float *)data << endl;
        for (int i=0; i<N; ++i) {
            cout << data[i] << ", ";
        }
        cout << endl;
    }
    return 0;
}
