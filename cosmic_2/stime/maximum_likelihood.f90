module maximum_likelihood
use precisione
use gnuplot
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

!__________________________________________________________________

  subroutine fact_check()
    integer :: i, fact_n

    do i=1,5
      fact_n = fact(i)
      PRINT*, I, FACT_N
    end do

  end subroutine fact_check

!___________________________________________________________


  function lnL_partial_sum(nk,size,pk) result(lnL_partial)
    implicit none
    integer :: nk,size
    real(kind=rk) :: lnL_partial, pk

    lnL_partial = nk*log(size*pk) - log(real(fact(nk))) - size*pk

  end function lnL_partial_sum

!_______________________________________________________________

  function lnL_partial_sum2(nk,size,pk) result(lnL_partial)
    implicit none
    integer :: nk,size
    real(kind=rk) :: lnL_partial, pk

    lnL_partial = nk*log(size*pk) - (nk+0.5)*log(real(nk)) - nk+log( 2*acos(-1.) )/2 - size*pk

  end function lnL_partial_sum2

!________________________________________________________________________________

  subroutine MML(nk,nbin,x,size,dx)
    implicit none
    integer :: nbin, i, j, size
    integer, parameter :: step=1000
    real(kind=rk), dimension(step) :: tau_test, lnL
    integer, dimension(nbin) :: nk
    real(kind=rk), dimension(nbin+1) :: x
    real(kind=rk) :: lnL_partial, pk, dx, lnL_max=-10**7, tau_max, a=0.03,b=0.12

    open(unit=8, file='maxlike.txt', action='write')

    tau_test(1) = a
    do i=1,step
      lnL_partial = 0.
      tau_test(i) = tau_test(1) + (i-1)*(b-a)/step

      do j=1,nbin
        pk = dx/tau_test(i) * exp( -(x(j)+x(j+1))/2./tau_test(i) )

        if(nk(j).le.20)then
          lnL_partial = lnL_partial + lnL_partial_sum(nk(j),size,pk)
        else
          lnL_partial = lnL_partial + lnL_partial_sum2(nk(j),size,pk)
        end if
      end do

      lnL(i) = lnL_partial

      if (lnL(i).gt.lnL_max) then
        lnL_max  = lnL(i)
        tau_max = tau_test(i)
      end if

      write(8,*) tau_test(i), lnL(i)
    end do
    call find_errorMML(tau_test,lnL,step,lnL_max,tau_max)

  end subroutine MML


  subroutine find_errorMML(tau_test,lnL,step,lnL_max,tau_max)
    implicit none
    integer :: step, i
    real(kind=rk) :: lnL_max, sigma_dx, sigma_sx, tau_max, diff_sx=0.1_rk, diff_dx=0.1_rk
    real(kind=rk), dimension(step) :: lnL, tau_test

    do i=1,step
      if (tau_test(i).lt.tau_max .and. abs(lnL_max-0.5_rk-lnL(i)).le.diff_sx) then
        sigma_sx = tau_test(i)
        diff_sx=abs(lnL_max-0.5_rk-lnL(i))

      else if (tau_test(i).gt.tau_max .and. abs(lnL_max-0.5_rk-lnL(i)).le.diff_dx) then
        sigma_dx = tau_test(i)
        diff_dx = abs(lnL_max-0.5_rk-lnL(i))
      end if
    end do

  call plot_commands_MML(lnL_max,sigma_dx,sigma_sx,tau_max)
  print*, 'MML', tau_max, 'sigma_sx', tau_max-sigma_sx, 'sigma_dx', sigma_dx-tau_max
  end subroutine find_errorMML

end module maximum_likelihood
