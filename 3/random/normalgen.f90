module normalgen
use randomseed
implicit none
contains

  subroutine rand_normal(mu,sigma,z0)
    real :: mu, sigma, u1=0., u2, z0, z1,seed
    real, parameter :: twopi=2.*4*atan(1.)

      call random_number(u1)
      call random_number(u2)

      z0 = sqrt(-2.*log(u1))*cos(twopi*u2)
      z1 = sqrt(-2.*log(u1))*sin(twopi*u2)

    z0 = z0*sigma + mu

  end subroutine rand_normal

!___________________________________________________________________
! normalgen tests
  subroutine test(z0)
    implicit none
    real :: z0
    call rand_normal(0.,1.,z0)
  end subroutine test

!___________________________________________________________________

  subroutine mean_sd_check(size)
  implicit none
  integer :: i, j, k
  integer :: size
  real :: array(size), z0, mean, sd

  ! generate array
  call init_random_seed()
  do i=1,size
    call rand_normal(0.,1.,z0)
    array(i) = z0
  end do

  ! check mean and std dev
    mean = SUM(array)/size
    sd = SQRT(SUM((array - mean)**2)/size)
  end subroutine mean_sd_check

!___________________________________________________________________

  function arraygenerator(k,size) result(array)
    implicit none
    integer :: i, j, k, size
!    integer, PARAMETER :: size
    real :: array(size), z0, mean, sd, somma

    ! generate array s
    call init_random_seed()
    do i=1,size
    somma = 0.
      do j=1,k
        call rand_normal(0.,1.,z0)
        somma = somma + z0**2
      end do
      array(i) = somma  ! array(i)=sum of k random numbers**2
    end do

    ! check mean and std dev
      mean = SUM(array)/size
      sd = SQRT(SUM((array - mean)**2)/size)
      
  end function arraygenerator

end module normalgen
