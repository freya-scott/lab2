module minimi_quadrati
use precisione
implicit none
contains

  function erlang_(t,tau) result(erlang)
    implicit none
    real(kind=rk) :: tau, erlang, t

    erlang = exp(-t/tau)/tau

  end function


  function Q_partial_sum(mu,nk) result(q_partial)
    implicit none
    integer :: nk
    real(kind=rk) :: q_partial, mu

    q_partial = (nk-mu)**2/nk

  end function Q_partial_sum


  subroutine MMQ(nbin,x,dx,size,nk)
    implicit none
    integer :: nbin, i, j, size
    integer, parameter :: step=1000
    real(kind=rk), dimension(nbin+1) :: x
    real(kind=rk) :: dx, mu, tau_test, tau_min, q, qmin=10**7, a=0.02, b=0.12
    integer, dimension(nbin) :: nk

    tau_test = a
    open(unit=3, file='q.txt', action='write')
    do i=1,step
      q = 0.
      tau_test = tau_test + (b-a)/step
      do j=1,nbin
          mu = dx*size* erlang_( (x(j)+x(j+1))/2., tau_test)
          q = q + Q_partial_sum(mu,nk(j))
      end do
      if (q.lt.qmin) then
        qmin = q
        tau_min = tau_test
      end if
      print*,q
      write(3,*)  tau_test, q
    end do
    print*, tau_min, qmin
  end subroutine MMQ

end module minimi_quadrati
