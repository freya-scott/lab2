module histogram
use precisione
use write
implicit none
contains

  function x_values(array,nbin,dx) result(x)
    implicit none
    integer :: nbin, i
    real(kind=rk), dimension(:) :: array
    real(kind=rk), dimension(nbin+1) :: x
    real(kind=rk) :: dx


    do i=1,nbin+1
      x(i) = minval(array) + (i-1)*dx
    end do

  end function x_values

!__________________________________________________________


  subroutine histo(array,nbin,x,nk,size,dx)
    implicit none
    real(kind=rk), dimension(size) :: array
    integer :: nbin, i, j, size
    real(kind=rk) :: dx
    integer, dimension(nbin) :: nk
    real(kind=rk), dimension(nbin+1) :: x

    dx = (maxval(array)-minval(array))/nbin
    x = x_values(array,nbin,dx)

    nk=0
    do i=1,202
      do j=1,nbin-1
        if (array(i).gt.x(j) .and. array(i).le.x(j+1)) then
          nk(j) = nk(j) + 1
        end if
      end do

      if (array(i).ge.x(nbin) .and. array(i).le.x(nbin+1)) then
        nk(nbin) = nk(nbin) + 1
      end if
    end do

    call write_to_file(x,nk,nbin)
  end subroutine histo

end module histogram
