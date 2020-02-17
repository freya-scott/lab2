module seed
implicit none

contains

subroutine gen_random_seed()
implicit none
integer :: s, dim, clock
integer, dimension(:), allocatable :: seed
call random_seed(size = dim)
allocate(seed(dim))
call system_clock(count=clock)
seed = clock + 37 * (/ (s - 1, s = 1, dim) /)
call random_seed(put=seed)
deallocate(seed)
end subroutine gen_random_seed

!---------------------------------------------------------------

subroutine histo(n,m,a_1,b_1, nbin,x,dx,input,mead)
implicit none
integer :: n, m, k, i, j, nbin
character(len=1) :: input
real, dimension(n) :: r
real, dimension(m) :: s, d
real, dimension(nbin+1) :: x
real, dimension(nbin) :: nk, devst
real :: a_1, b_1, dx, mead

	do i=1,n
		do k=1,m                    	! do loop per generare numeri casuali
		call random_number(s(k))
			d(k)=a_1+(b_1-a_1)*s(k)

	end do

	if ( input== '3' ) then
		r(i)=sum(d)/2.
	else
		r(i)=sum(d)
	end if
	
		    do j=1,nbin
			if (x(j)<r(i).and.r(i)<x(j+1)) then
			nk(j)=nk(j)+1.0
			else
			cycle
			end if
		    end do
	end do
	devst=sqrt(nk)

	! writing to file histogram data
	open(unit=2, file='hist.dat')
	do i=1,nbin
	    write(2,*) (x(i)+x(i+1))/2.0, nk(i), devst(i)	! mid point of interval, conteggio, error
	end do
mead=sum(nk)
call gauss(n,nbin,nk,x,dx)
end subroutine histo

!-------------------------------------------------------------

subroutine gauss(n,nbin,nk,x,dx)
implicit none
real :: somma_1, mean, somma_2, var, sigma, pi=4.0*atan(1.0), dx
integer :: i, nbin, n
real, dimension(nbin) :: nk, f
real, dimension(nbin+1) :: x

! dati necessari per calcolare il gaussiano
somma_1=0
do i=1,nbin
	somma_1= somma_1 + ((x(i)+x(i+1))/2.0)*nk(i)
end do

mean=somma_1/n
somma_1=0
somma_2=0

do i=1,nbin
somma_1 = somma_1 + (nk(i)*(((x(i)+x(i+1))/2.0)**2))
somma_2 = somma_2 + (nk(i)*mean**2)
end do
var = (somma_1-somma_2)/n

! writing gauss data to file
open(unit=3, file='gauss.dat')
do i=1,nbin
f(i)= 1/sqrt(2*pi*var) * exp(- (((x(i)+x(i+1))/2.0) -mean)**2 / (2*var))   ! Funzione di distribuzione normale
write(3,*) (x(i)+x(i+1))/2.0, f(i)*n*dx
end do
RETURN
end subroutine gauss

!---------------------------------------------------------------

subroutine gnuplot(a,b)
implicit none
real, intent(in) :: a,b

open(unit=3, file='gauss.txt')
write(unit=3, fmt=*) "set title 'Istogramma distribuzione uniforme'"
write(unit=3, fmt=*) "set xlabel 'Numeri casuali'"
write(unit=3, fmt=*) "set ylabel 'Conteggi'"
write(unit=3, fmt=*) "set xrange [",a,":",b,"]"
write(unit=3, fmt=*) "set yrange [0:]"
write(unit=3, fmt=*) "plot 'hist.dat' u 1:2:3 with error, '' u 1:2 with boxes, 'gauss.dat' u 1:2 w l"

end subroutine gnuplot

!---------------------------------------------------------------

subroutine gnuplot_2(a,b,mead)
implicit none
real, intent(in) :: a,b,mead
! solo istogramma
open(unit=3, file='hist.txt')
write(unit=3, fmt=*) "set title 'Istogramma distribuzione uniforme'"
write(unit=3, fmt=*) "set xlabel 'Numeri casuali'"
write(unit=3, fmt=*) "set ylabel 'Conteggi'"
write(unit=3, fmt=*) "set xrange [",a,":",b,"]"
write(unit=3, fmt=*) "set yrange [0:]"
write(unit=3, fmt=*) "plot 'hist.dat' u 1:2 w boxes, '' u 1:2:3 w error,",&
                    &mead/22.

end subroutine gnuplot_2

end module seed
