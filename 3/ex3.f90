program ex3
  use normalgen
  use histogram
  use write_to_file

  implicit none
  integer, PARAMETER ::  size=10000, nbin=60
  real :: array(size), dx, mean, sd
  integer :: k=100

  real, dimension(:), allocatable :: x

    array = arraygenerator(k,size)

    mean = SUM(array)/size
    sd = SQRT(SUM((array - mean)**2)/size)

    call hist_test2(array,size,nbin,x,dx,k)
    call write2(nbin,x,k,dx,size,mean,sd)
end program ex3
