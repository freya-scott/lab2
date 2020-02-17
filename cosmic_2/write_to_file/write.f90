module write
use precisione
use gnuplot
implicit none
contains

  subroutine write_to_file(x,nk,nbin)
    implicit none
    integer :: nbin,i, size
    integer, dimension(nbin) :: nk
    real(kind=rk), dimension(nbin+1) :: x

    open(unit=2, file='hist.txt', action='write')
    do i=1,nbin
      write(2,*) (x(i)+x(i+1))/2., nk(i), sqrt(real(nk(i)))
    end do
    close(unit=2)
    call plot_commands_hist()
  end subroutine write_to_file


end module write
