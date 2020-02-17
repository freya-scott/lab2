module maximum_likelihood
use precisione
use minimi_quadrati
implicit none
contains

  function fact(n) result(fact_n)
    implicit none
    integer :: i, n, fact_n

    fact_n = 1
    do i=1,n
      fact_n = fact_n*i
    end do

  end function fact

  function lnL_partial_sum(nk,size,mu) result(lnL_partial)
    implicit none
    integer :: nk,size
    real(kind=rk) :: lnL_partial, mu

    lnL_partial = nk*log(size*mu) - log(real(fact(nk))) - size*mu

  end function lnL_partial_sum

!_______________________________________________________________

  function lnL_partial_sum2(nk,size,mu) result(lnL_partial)
    implicit none
    integer :: nk,size
    real(kind=rk) :: lnL_partial, mu

    lnL_partial = nk*log(size*mu) - (nk+0.5)*log(real(nk)) - nk+log( 2*acos(-1.) )/2 - size*mu

  end function lnL_partial_sum2

!________________________________________________________________

  subroutine MML(nk,nbin,x,size,dx)
    implicit none
    integer :: nbin, i, j, size
    integer, parameter :: step=1000
    real(kind=rk), dimension(step) :: tau_test, lnL
    integer, dimension(nbin) :: nk
    real(kind=rk), dimension(nbin+1) :: x
    real(kind=rk) :: lnL_partial, mu, dx, lnL_max=0., tau_min, a=0.03,b=0-09

    open(unit=8, file='maxlike.txt', action='write')

    tau_test(1) = a
    do i=1,step
      lnL_partial = 0.
      tau_test(i) = tau_test(1) + (i-1)*(b-a)/step
      do j=1,nbin
        mu = dx/tau_test(i) * erlang_( (x(j)+x(j+1))/2., tau_test(i))

        if(nk(j).le.20)then
          lnL_partial = lnL_partial + lnL_partial_sum(nk(j),size,mu)
        else
          lnL_partial = lnL_partial + lnL_partial_sum2(nk(j),size,mu)
        end if
      end do
      lnL(i) = lnL_partial

      if (lnL(i).gt.lnL_max) then
        lnL_max  = lnL(i)
        tau_min = tau_test(i)
      end if

      write(8,*) tau_test(i), lnL(i)
    end do
    PRINT*, 'MML', tau_min
  end subroutine MML
end module maximum_likelihood
