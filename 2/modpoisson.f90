module poissonmod
implicit none
contains

	! subroutine per il calcolo della funzione di distribuzione di Poisson
	subroutine distribution(fact,n)
	implicit none
	real, dimension(n) :: fact, p
	integer :: i, n
	real :: v
	! per valore atteso v=0.1
	open(unit=1, file='v=0.1')
	v=0.1
	do i=1,n
		p(i)=v**i/fact(i)*exp(-v)
		write(1,*) i, p(i)
	end do
	close(unit=1)
	
	! per valore atteso v=0.5
	open(unit=2, file='v=0.5')
	v=0.5
	do i=1,n
		p(i)=v**i/fact(i)*exp(-v)
		write(2,*) i, p(i)
	end do
	close(unit=2)

	! per valore atteso v=1.0
	open(unit=3, file='v=1.0')
	v=1.0
	do i=1,n
		p(i)=v**i/fact(i)*exp(-v)
		write(3,*) i, p(i)
	end do
	close(unit=3)

	! per valore atteso v=3.0
	open(unit=4, file='v=3.0')
	v=3.0
	do i=1,n
		p(i)=v**i/fact(i)*exp(-v)
		write(4,*) i, p(i)
	end do
	close(unit=4)
	end subroutine distribution

	! subroutine per commandi gnuplot--in gnuplot "load 'poisson.dat'"
	subroutine gnuplot()
	implicit none
	open(unit=7, file='poisson.dat')
	write(7,*) "set title 'Distribuzione di Poisson"
	write(7,*) "set xlabel 'x'"
	write(7,*) "set ylabel 'P(X=x)'"
	write(7,*) "plot 'v=0.1' u 1:2 w l,'v=0.5' u 1:2 w l, 'v=1.0' u 1:2 w l, 'v=3.0' u 1:2 w l"
	
	! commandi gnuplot per gauss e poisson con v=10--in gnuplot "load 'gauss.dat'"
	open(unit=8, file='gauss.dat')
	write(8,*) "set title 'Distribuzione di Poisson e Gauss per v=10"
	write(8,*) "set xlabel 'x'"
	write(8,*) "plot 'gauss' u 1:2 w l, '' u 1:3 w l "
	end subroutine gnuplot


	! subroutine per funzione di Poisson e Gauss con v=10
	subroutine gauss(n,fact)
	implicit none
	real, dimension(n) :: fact, p, g
	integer :: i, n
	real :: v=10, pi=4.0*atan(1.0)

	open(unit=5, file='gauss')
	v=10

	do i=1,n
		g(i)=1/sqrt(2*pi*v) * exp(-(i-v)**2/(2*v))	! funzione distribuzione gauss
		p(i)=v**i/fact(i)*exp(-v)			! funzione distribuzione poisson
		write(5,*) i, p(i), g(i)
	end do
	close(unit=5)
	end subroutine gauss

end module poissonmod









