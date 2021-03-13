set terminal postscript eps monochrome dashed
set output "varyM-ps.eps"
set data style linespoints
#set ylabel "probability of satisfiability"
set xlabel "L/N"
set yrange[0:1]
plot "ps5-2d.data" using 1:2 title 'M=1', \
"ps6-2d.data" using 1:2 title 'M=2', \
"ps7-2d.data" using 1:2 title 'M=5', \
"ps8-2d.data" using 1:2 title 'M=10', \
"ps9-2d.data" using 1:2 title 'M=20'
