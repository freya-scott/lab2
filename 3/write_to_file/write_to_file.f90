module write_to_file
use chi_quadrato
implicit none
contains

! write histogram
  subroutine write(nbin,x,nk)
    implicit none
    integer, dimension(nbin) :: nk
    real, dimension(nbin+1) :: x
    integer :: i, nbin

      open(unit=1, file='histogram1.txt',action='write')
      do i=1,nbin
        write(1,*) (x(i)+x(i+1))/2., nk(i)
      end do

  end subroutine write

!___________________________________________________________________

! write chi function
  subroutine write2(nbin,x,k,dx,size,mean,sd)
    implicit none
    integer :: nbin, i, k, size
    real :: chi, xdist, dx, mean, sd
    real, dimension(nbin+1) :: x

    open(unit=12, file='dist100.txt', action='write')
    open(unit=11, file='chi_distribution1.txt', action='write')

      if ( k==100 ) then
        do i=1,nbin+1
          xdist=x(i)
          write(11,*) real(x(i)), chi_(xdist,k)*dx*size, gauss_(xdist,mean,sd)*dx*size
        end do
      else if ( k/=100 ) then
        do i=1,nbin+1
          xdist=x(i)
          write(11,*) real(x(i)), chi_(xdist,k)*dx*size
        end do
      end if

      call gnuplot(k)
  end subroutine write2


  subroutine gnuplot(k)
    implicit none
    integer :: k

    open(unit=13, file='plot.txt', action='write')

    if ( k==1 ) then
      write(13,*) " set yrange[0:4500]"
      write(13,*) " plot 'histogram1.txt' u 1:2 w boxes, 'chi_distribution1.txt' u 1:2 w l"

    else if ( k==100 ) then
      write(13,*) "plot 'histogram1.txt' u 1:2 w boxes lt 2, 'chi_distribution1.txt' u 1:2 w l lt 7, '' u 1:3 w l lt 8"

    else
      write(13,*) " plot 'histogram1.txt' u 1:2 w boxes, 'chi_distribution1.txt' u 1:2 w l"
    end if

  end subroutine

end module write_to_file
