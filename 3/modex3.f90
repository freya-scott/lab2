module modex3
  implicit none
contains

  subroutine gen_svalues(n,k,s)
    implicit none
    integer :: n,i,j,k
    real, dimension(n) :: s
    real :: r, c

    do i=1,n
      c=0
      do j=1,k
        call random_number(r)
        c = c+r**2
      end do
      s(i) = c
    end do

  end subroutine gen_svalues

!____________________________________________________________________
! test validity of array s
  subroutine checks(s,n)
    implicit none
    real, dimension(:) :: s
    integer :: i,n

    do i=1,n
      if (s(i).gt.1) then
        print*, 'error'
      else
      end if
    end do
  end subroutine checks

!___________________________________________________________________
end module modex3
