module media_stima
use precisione
implicit none
contains

  subroutine media_array(array,size)
    implicit none
    real(kind=rk), dimension(:) :: array
    real(kind=rk) :: media, sigma
    integer :: size,i

    media = sum(array)/size
    do i=1,size
      sigma = sigma + (array(i)-media)**2
    end do
    sigma = sqrt(sigma/size)

    PRINT*, 'MEDIA', media, 'Sigma', sigma

  end subroutine media_array

end module media_stima
