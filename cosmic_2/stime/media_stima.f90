module media_stima
use precisione
implicit none
contains

  subroutine media_array(array,size,nbin)
    implicit none
    real(kind=rk), dimension(:) :: array
    real(kind=rk) :: media, sigma
    integer :: size,nbin

    media = sum(array)/size
    sigma = sqrt(media**2/size)

    PRINT*, 'Media dei tempi', media

  end subroutine media_array

end module media_stima
