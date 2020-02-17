program histogram
use seed
implicit none
real, dimension(:), allocatable :: x, nk
integer :: n=1000, i, nbin,m
real :: a,b,dx,a_1, b_1, mead
character(len=1) :: input

read*, input

call gen_random_seed()

if ( input=='1' ) then      ! PARTE 1
  a=0.95    ! a,b estremi dell'istogramma
  b=2.05
  dx=0.05
  a_1=1     !numeri casuali generati nell'intervallo a_1,b_1
  b_1=2
  m=1           !no. di vc che si sommano

else if ( input=='2' ) then    ! PARTE 2
  a=-3.   ! a,b estremi dell'istogramma
  b=3.
  dx=0.1
  a_1=-0.5    !numeri casuali generati nell'intervallo a_1,b_1
  b_1=0.5
  m=12.

else     ! PARTE 3
  a=-0.55    ! a,b estremi dell'istogramma
  b=0.55
  dx=0.05
  a_1=-0.5     !numeri casuali generati nell'intervallo a_1,b_1
  b_1=0.5
  m=2.
end if

nbin=int((b-a)/dx)
allocate(nk(nbin))
nk=0

allocate(x(nbin+1))
do i=1,nbin+1
    x(i)=a+dx*(i-1)	! establishing x values
end do

call histo(n,m,a_1,b_1, nbin,x,dx,input,mead)		! generates values and creates histogram
!call gauss(n,nbin,nk,x,dx)		! generates funzione di Gauss
call gnuplot(a,b)		! plot commands for histogram with gauss
call gnuplot_2(a,b,mead)		! plot commands for histogram only
close(unit=3)
close(unit=2)
end program histogram
