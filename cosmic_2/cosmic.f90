program cosmic

use precisione
use read_data
use histogram
use media_stima
use minimi_quadrati
use maximum_likelihood

implicit none
integer, parameter :: size=202, nbin=15
real(kind=rk), dimension(size) :: array
real(kind=rk), dimension(nbin+1) :: x
real(kind=rk) :: dx
integer, dimension(nbin) :: nk

  call read_(array)
  call histo(array,nbin,x,nk,size,dx)
  call media_array(array,size,nbin)
  call MMQ(nbin,x,dx,size,nk)
  call MML(nk,nbin,x,size,dx)

end program cosmic
