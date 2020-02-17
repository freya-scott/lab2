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


  subroutine plot_commands_MML(lnL_max,sigma_dx,sigma_sx,tau_max)
    implicit none
    real(kind=rk) :: lnL_max, sigma_dx, sigma_sx,tau_max
    open(unit=9, file='MML.txt', action='write')

    write(9,*) " set title 'Metodo grafico maximum likelihood' "
  !  write(9,*) "set xrange[0.03:0.12]"
  !  write(9,*) "set yrange[-408:-335]"
    write(9,*)  " set arrow from " , sigma_sx , " , graph 0 to " , sigma_sx, " , graph 1 nohead "
    write(9,*)  " set arrow from " , sigma_dx , " , graph 0 to " , sigma_dx, " , graph 1 nohead "
    write(9,*) " set arrow from " ,tau_max , " , graph 0 to " ,tau_max , " , graph 1 nohead "
    write(9,*) "plot 'maxlike.txt' u 1:2 w l"
    write(9,*) "replot", lnL_max
    write(9,*) "replot", lnL_max-0.5

  end subroutine plot_commands_MML

end module gnuplot
