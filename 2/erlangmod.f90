module erlangmod
implicit none
contains

	! Subroutine per determinare la funzione di distribuzione di Erlang
	subroutine erlangfunc(n,x)
	implicit none
	integer :: i, n, k,  j
	real, dimension(n) :: f, x, g
	real :: t=1.0, mu, var, pi=4.0*atan(1.0)

	open(unit=11, file='k=1')
	k=1
	do i=1,n
		f(i)=1* x(i)**(k-1)/t**k * exp(-x(i)/t)
		write(11,*) x(i), f(i)
	end do
	
	open(unit=12, file='k=2')
	k=2
	do i=1,n
		f(i)=1* x(i)**(k-1)/t**k * exp(-x(i)/t)	
		write(12,*) x(i), f(i)
	end do

	open(unit=13, file='k=3')
	k=3
	do i=1,n
		f(i)=1.0/2.0 * x(i)**(k-1)/t**k * exp(-x(i)/t)	! 1.0/2.0=(k-1)!=2!
		write(13,*) x(i), f(i)
	end do

	! Per il secondo grafico
	open(unit=15, file='gausserlang.dat')
	k=10
	mu=k*t
	var=k*t**2
	do i=1,n	
		g(i)=1/sqrt(2*pi*var) * exp(-(x(i)-mu)**2/(2*var))	! Gauss
		f(i)=1.0/362880* x(i)**(k-1)/t**k * exp(-x(i)/t)	! Erlang (362880=(k-1)!=9!)
		write(15,*) x(i), f(i), g(i)
	end do
	end subroutine erlangfunc


	! subroutine commandi gnuplot-- in gnuplot "load 'erlang.txt'"
	subroutine gnu()
	implicit none

	open(unit=13, file='erlang.txt')
	write(13,*) "set title 'Distribuzione di Erlang'"
	write(13,*) "set xlabel 't'"
	write(13,*) "plot 'k=1' u 1:2 w l, 'k=2' u 1:2 w l, 'k=3' u 1:2 w l"
	
	open(unit=14, file='erlanggauss.txt')
	write(14,*) "set title 'Distribuzione di Erlang e di Gauss"
	write(14,*) "plot 'gausserlang.dat' u 1:2 w l, '' u 1:3 w l"
	end subroutine

end module erlangmod
