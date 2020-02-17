module gnuplot
use precisione
implicit none
contains


  subroutine plot_commands_hist()
    implicit none

    open(unit=7,file='histogram.txt', action='write')
    write(7,*) "plot 'hist.txt' u 1:2 w boxes, '' u 1:2:3 w err"

  end subroutine plot_commands_hist



  subroutine plot_commands_q(qmin,sigma_dx,sigma_sx,tau_min)
    implicit none
    real(kind=rk) :: qmin, sigma_dx, sigma_sx, tau_min
    open(unit=4, file='MMQ.txt', action='write')

    write(4,*) " set title 'Metodo grafico minimi quadrati' "
    write(4,*) "set xrange[0.03:0.12]"
    write(4,*)  " set arrow from " , sigma_sx , " , graph 0 to " , sigma_sx, " , graph 1 nohead "
    write(4,*)  " set arrow from " , sigma_dx , " , graph 0 to " , sigma_dx, " , graph 1 nohead "
    write(4,*) " set arrow from " , tau_min , " , graph 0 to " , tau_min , " , graph 1 nohead "
    write(4,*) "plot 'q.txt' u 1:2 w l"
    write(4,*) "replot", qmin
    write(4,*) "replot", qmin+1

  end subroutine plot_commands_q


end module gnuplot
