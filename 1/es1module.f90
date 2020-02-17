module precisione
	implicit none
	integer, parameter :: rk = selected_real_kind(8)
end module precisione


module es1mod
use precisione
implicit none
contains

  subroutine read_data()
    implicit none
    integer, parameter :: size=5
    integer :: j
    real, dimension(size) :: V, I, error

    open(unit=1, file='data.txt', action='read')
    do j=1,size
      read(1,*) V(j), I(j), error(j)
    end do
    call formule(v,I,error)
  end subroutine read_data



  subroutine formule()
    implicit none
    real(kind=rk) :: s00,s10,s01,s20,s11,d
    integer :: size
    real, dimension(size) :: V, I, error

    s00 = sum(1/error**2)
    s10 = sum(V/error**2)
    s01 = sum(I/error**2)
    s20 = sum(V**2/error**2)
    s11 = sum(V*I/error**2)
    d = s00*s20 - s10**2

    call stime(s00,s10,s01,s20,s11,d)
  end subroutine formule



  subroutine stime(s00,s10,s01,s20,s11,d,I,V)
    implicit none
    integer :: size
    real(kind=rk) :: s00,s10,s01,s20,s11,d
    real(kind=rk) :: m,qsigma_m,sigma_q, cov
    real(kind=rk), dimension(size) :: I,V

    m = (s00*s11-s10*s01)/d
    q = (s20*s01-s10*s11)/d
    sigma_m = sqrt(s00/d)
    sigma_q = sqrt(s20/d)
    cov = s10/d

    call write_to_file(size,I,V,m,q)
  end subroutine stime


  subroutine write_to_file()
    implicit none
    integer :: size, j
    real(kind=rk), dimension(size) :: I, V
    real(kind=rk) :: m, q

    open(unit=2, file='m_q_stimati.txt', action='write')
    do j=1,size
      write(2,*) I(j), m*V(j)+q
    end do

end module es1mod
