module chi_quadrato
use precisione
implicit none
contains

  function chi_(x,k) result(chi)
    implicit none
    integer :: k
    real (kind=rk) :: y, halfk
    real :: x, chi
    halfk = k/2.
      y = x
      chi = y**(halfk-1)*exp(-y/2.)/( 2**(halfk)*gamma(halfk) )
      x = y

  end function chi_

!___________________________________________________________________

  function gauss_(x,mu,sigma) result(gauss)
    implicit none
    real :: x, gauss, sigma, mu
    real (kind=rk) :: y
    real, parameter :: pi=4.*atan(1.0)

    y=x
    gauss = exp(-1/2.* ( (y-mu)/sigma)**2 ) / ( sqrt(2*pi*sigma**2) )
    x=y
  end function

end module chi_quadrato
