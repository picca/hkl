set title "Psi scan"
set xrange [0:360]
set xtics 0,30,360
set yrange [-180:180]
set ytics -180,30,180
plot \
 '../../build/doc/example/data' u 1:2 title "omega",\
 '../../build/doc/example/data' u 1:3 title "chi",\
 '../../build/doc/example/data' u 1:4 title "phi",\
 '../../build/doc/example/data' u 1:5 title "2theta"
pause -1

set term postscript eps color enhanced
set output "psi.eps"
replot