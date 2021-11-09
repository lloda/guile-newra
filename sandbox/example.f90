! (example.f90) -*- coding: utf-8; mode: f90 -*-
! example for guile-newra Fortran ffi

! $FORTRAN -shared -fPIC -o libexample.so example.f90 -std=f2018 -fcheck=all

module example

  use iso_c_binding

contains

  ! Bilinear interpolate in x, y table. x and y are indices in the sizes of the table
  ! [0 ... n], so the steps of x and y are accounted for in the caller.

  real(C_DOUBLE) function lookup_xy(x, y, table) &
       bind(c, name='lookup_xy') &
       result(z)

    real(C_DOUBLE), intent(in) :: x, y
    real(C_DOUBLE), intent(in), dimension(:, :) :: table

    real(C_DOUBLE) :: dx, dy
    integer :: ix, iy

    ix = max(0, min(size(table, 1)-2, int(floor(x))))     ! x = end-of-table will use dx = 1.
    iy = max(0, min(size(table, 2)-2, int(floor(y))))     ! y = end-of-table will use dy = 1.
    dx = x-ix
    dy = y-iy

    z =  &
         + table(ix+1, iy+1)*(1-dx)*(1-dy) &
         + table(ix+2, iy+1)*dx*(1-dy) &
         + table(ix+1, iy+2)*(1-dx)*dy &
         + table(ix+2, iy+2)*dx*dy

  end function lookup_xy

end module example
