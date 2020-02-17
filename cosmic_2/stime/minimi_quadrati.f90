module minimi_quadrati
use precisione
use gnuplot
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
    real(kind=rk), dimension(step) :: tau_test, q
    real(kind=rk) :: dx, mu, tau_min, qmin=10**7, a=0.02, b=0.12, q_partial
    integer, dimension(nbin) :: nk

    tau_test(1) = a
    open(unit=3, file='q.txt', action='write')
    do i=1,step
      q_partial = 0.
      tau_test(i) = tau_test(1) + (i-1)*(b-a)/step
      do j=1,nbin
        if (nk(j).gt.3) then
          mu = dx*size* erlang_( (x(j)+x(j+1))/2., tau_test(i))
          q_partial = q_partial + Q_partial_sum(mu,nk(j))
        end if
      end do

      q(i) = q_partial
      if (q(i).lt.qmin) then
        qmin = q(i)
        tau_min = tau_test(i)
      end if
      write(3,*)  tau_test(i), q(i)
    end do

    print*, 'MMQ', tau_min
    call find_error(tau_test,q,step,qmin,tau_min)
  end subroutine MMQ


  subroutine find_error(tau_test,q,step,qmin,tau_min)
    implicit none
    integer :: step, i
    real(kind=rk) :: qmin, sigma_dx, sigma_sx, tau_min, diff_sx=0.1_rk, diff_dx=0.1_rk
    real(kind=rk), dimension(step) :: q, tau_test

    do i=1,step
      if (tau_test(i).lt.tau_min .and. abs(qmin+1-q(i)).le.diff_sx) then
        sigma_sx = tau_test(i)
        diff_sx = abs(qmin+1-q(i))
      else if (tau_test(i).gt.tau_min .and. abs(qmin+1-q(i)).le.diff_dx) then
        sigma_dx = tau_test(i)
        diff_dx = abs(qmin+1-q(i))
      end if
    end do

  call plot_commands_q(qmin,sigma_dx,sigma_sx,tau_min)
  end subroutine find_error

end module minimi_quadrati
