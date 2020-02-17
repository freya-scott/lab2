module histogram
use write_to_file
implicit none
contains

  subroutine hist(array,nbin,size,x,nk,dx)
    integer :: nbin, size, i, j
    real, dimension(nbin+1) :: x
    integer, dimension(nbin) :: nk
    real :: dx, array(size)


    dx = (maxval(array)-minval(array))/nbin
    do i=1,nbin+1
      x(i) = minval(array)+(i-1)*dx
    end do

    nk=0
    do i=1,size
      do j=1,nbin-1
        if ( array(i).ge.x(j) .and. array(i).lt.x(j+1) ) then
          nk(j) = nk(j) +1

        end if
      end do
      if ( array(i).ge.x(nbin) .and. array(i).le.x(nbin+1) ) then
          nk(nbin) = nk(nbin) +1
      end if
    end do


  end subroutine hist

!___________________________________________________________________
!Histogram test
  subroutine hist_test1()
    implicit none
    integer :: size=100, i, nbin=10, k
    real :: dx
    real, dimension(100) :: array
    integer, dimension(:), allocatable :: nk
    real, dimension(:), allocatable :: x

    allocate(nk(nbin), x(nbin+1))
    do i=1,size
      array(i)=i
    end do

    call hist(array,nbin,size,x,nk,dx)
    call write(nbin,x,nk)
  end subroutine hist_test1

!___________________________________________________________________

!Histogram test
  subroutine hist_test2(array,size,nbin,x,dx,k)
    implicit none
    integer :: size, i, nbin, k
    real :: dx
    real, dimension(:) :: array
    integer, dimension(:), allocatable :: nk
    real, dimension(:), allocatable :: x

    allocate(nk(nbin), x(nbin+1))

    call hist(array,nbin,size,x,nk,dx)
    call write(nbin,x,nk)

  end subroutine hist_test2

end module histogram
