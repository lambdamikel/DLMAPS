set terminal postscript eps monochrome dashed
set output "varyN-ps.eps"
set data style linespoints
#set ylabel "probability of satisfiability"
set xlabel "L/N"
set yrange[0:1]
plot "ps11-2d.data" using 1:2 title 'N=10', \
"ps10-2d.data" using 1:2 title 'N=8', \
"ps0-2d.data" using 1:2 title 'N=5', \
"ps5-2d.data" using 1:2 title 'N=4', \
"ps4-2d.data" using 1:2 title 'N=3'
