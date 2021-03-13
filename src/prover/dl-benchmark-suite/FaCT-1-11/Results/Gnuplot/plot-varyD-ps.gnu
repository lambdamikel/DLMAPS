set terminal postscript eps monochrome dashed
set output "varyD-ps.eps"
set data style linespoints
#set ylabel "probability of satisfiability"
set xlabel "L/N"
set yrange[0:1]
plot "ps1-2d.data" using 1:2 title 'D=5', \
"ps2-2d.data" using 1:2 title 'D=4', \
"ps3-2d.data" using 1:2 title 'D=3', \
"ps4-2d.data" using 1:2 title 'D=2'
