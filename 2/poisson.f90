program poisson
use poissonmod
implicit none
real :: v,  fac
real, dimension(:), allocatable :: p, x, fact
integer :: i, n, j

n=20			! il range--]0,n]

allocate(p(n+1))	! arrray di valori della funzione
allocate(fact(n))	! array del fattoriale

fac=1
do i=1,n		! loop per calcolare il fattorial al denominatore della funzione di distribuzione
	fac=fac*i
	fact(i)=fac	
end do

call distribution(fact,n)
call gauss(n,fact)
call gnuplot()
end program poisson



