program erlang
use erlangmod
implicit none
real :: a=0, b=20, dx
integer :: i, n=100
real, dimension(:), allocatable :: x

allocate(x(n))
dx=(b-a)/n

do i=1,n
	x(i)=a+dx*(i-1)	! establishing x values
end do

call erlangfunc(n,x)
call gnu()
end program erlang
