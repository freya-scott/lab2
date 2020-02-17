module read_data
use precisione
implicit none
contains

  subroutine read_(array)
    implicit none
    real(kind=rk), dimension(202) :: array
    integer :: i

    open(unit=1, file='Data3.txt', action='read')
    do i=1,202
      read(1,*) array(i)
      array(i) = array(i)*0.010036668/10**3
    end do
  end subroutine read_

end module read_data
