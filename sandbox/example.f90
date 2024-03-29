! (example.f90) -*- coding: utf-8; mode: f90 -*-
! examples/tests for guile-newra Fortran ffi

! $FORTRAN -shared -fPIC -o libexample.so example.f90 -std=f2018 -fcheck=all

module example

  use iso_c_binding
  implicit none

contains

  ! Bilinear interpolate in x, y table. x and y are indices in the sizes of the table
  ! [0 ... n], so the steps of x and y are accounted for in the caller.

  function lookup_xy(x, y, table) bind(c) result(z)
    real(C_DOUBLE), intent(in) :: x, y
    real(C_DOUBLE), intent(in) :: table(:, :)
    real(C_DOUBLE) :: z

    real(C_DOUBLE) :: dx, dy
    integer :: ix, iy

    ix = max(0, min(size(table, 1)-2, int(floor(x))))     ! x = end-of-table will use dx = 1.
    iy = max(0, min(size(table, 2)-2, int(floor(y))))     ! y = end-of-table will use dy = 1.
    dx = x-ix
    dy = y-iy

    z =  + table(ix+1, iy+1)*(1-dx)*(1-dy) &
         + table(ix+2, iy+1)*dx*(1-dy) &
         + table(ix+1, iy+2)*(1-dx)*dy &
         + table(ix+2, iy+2)*dx*dy

  end function lookup_xy


  function lookup_xy_complex(x, y, table) bind(c) result(z)
    real(C_DOUBLE), intent(in) :: x, y
    complex(C_DOUBLE_COMPLEX), intent(in) :: table(:, :)
    complex(C_DOUBLE_COMPLEX) :: z

    real(C_DOUBLE) :: dx, dy
    integer :: ix, iy

    ix = max(0, min(size(table, 1)-2, int(floor(x))))     ! x = end-of-table will use dx = 1.
    iy = max(0, min(size(table, 2)-2, int(floor(y))))     ! y = end-of-table will use dy = 1.
    dx = x-ix
    dy = y-iy

    z =  + table(ix+1, iy+1)*(1-dx)*(1-dy) &
         + table(ix+2, iy+1)*dx*(1-dy) &
         + table(ix+1, iy+2)*(1-dx)*dy &
         + table(ix+2, iy+2)*dx*dy

  end function lookup_xy_complex


  function conjugate(w) bind(c) result(z)
    complex(C_DOUBLE_COMPLEX), intent(in) :: w
    complex(C_DOUBLE_COMPLEX) :: z

    z = conjg(w)

  end function conjugate


  function ranker(arg) bind(c) result(z)
    real(C_DOUBLE), intent(in) :: arg(..)
    integer(C_INT32_T) :: z

    z = rank(arg)

  end function ranker


  function lbounder(arg) bind(c) result(z)
    real(C_DOUBLE), intent(in) :: arg(:)
    integer(C_INT32_T) :: z

    z = lbound(arg, 1)

  end function lbounder


  function valuer(arg) bind(c) result(z)
    real(C_DOUBLE), intent(in) :: arg(..)
    real(C_DOUBLE) :: z

    select rank(arg)
    rank(0) ; z = arg
    rank default ; z = 99
    end select

  end function valuer


  subroutine fillerf32(x) bind(c)
    real(C_FLOAT), intent(inout) :: x(:)

    x = x**2

  end subroutine fillerf32


  subroutine fillerf64(x) bind(c)
    real(C_DOUBLE), intent(inout) :: x(:)

    x = x**2

  end subroutine fillerf64


  function dgemv(a, v, w) bind(c) result(err)
    real(C_DOUBLE), intent(in) :: a(:, :)
    real(C_DOUBLE), intent(in) :: v(:)
    real(C_DOUBLE), intent(inout) :: w(:)
    integer(C_INT8_T) :: err

    integer :: i
    integer :: n
    if (size(w, 1) /= size(a, 1)) then
       err = 1
    else if (size(v, 1) /= size(a, 2)) then
       err = 2
    else
       n = size(v)
       w = 0.0
       do i = 1, n
          w = w + v(i) * a(:, i)
       end do
       err = 0
    end if

  end function dgemv

end module example
