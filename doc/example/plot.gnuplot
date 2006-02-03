set term postscript eps color enhanced
set xrange [0:360]
set xtics 0,30,360
set yrange [-180:180]
set ytics -180,30,180

set title "Psi scan omega0=34 chi=45 phi=77 2theta=-5 1st solution"
set output "psi_34_45_77_-5_1st.eps"
plot \
 '../../build/doc/example/data_1st' u 1:2 title "omega",\
 '../../build/doc/example/data_1st' u 1:3 title "chi",\
 '../../build/doc/example/data_1st' u 1:4 title "phi",\
 '../../build/doc/example/data_1st' u 1:5 title "2theta"

set title "Psi scan omega0=34 chi=45 phi=77 2theta=-5 2nd solution"
set output "psi_34_45_77_-5_2nd.eps"
plot \
 '../../build/doc/example/data_2nd' u 1:2 title "omega",\
 '../../build/doc/example/data_2nd' u 1:3 title "chi",\
 '../../build/doc/example/data_2nd' u 1:4 title "phi",\
 '../../build/doc/example/data_2nd' u 1:5 title "2theta"
 
set title "Psi scan omega0=30 chi=0 phi=0 2theta=60 1st solution"
set output "psi_30_0_0_60_1st.eps"
plot \
 '../../build/doc/example/data_30_0_0_60_1st' u 1:2 title "omega",\
 '../../build/doc/example/data_30_0_0_60_1st' u 1:3 title "chi",\
 '../../build/doc/example/data_30_0_0_60_1st' u 1:4 title "phi",\
 '../../build/doc/example/data_30_0_0_60_1st' u 1:5 title "2theta"

set title "Psi scan omega0=30 chi=0 phi=0 2theta=60 2nd solution"
set output "psi_30_0_0_60_2nd.eps"
plot \
 '../../build/doc/example/data_30_0_0_60_2nd' u 1:2 title "omega",\
 '../../build/doc/example/data_30_0_0_60_2nd' u 1:3 title "chi",\
 '../../build/doc/example/data_30_0_0_60_2nd' u 1:4 title "phi",\
 '../../build/doc/example/data_30_0_0_60_2nd' u 1:5 title "2theta"
