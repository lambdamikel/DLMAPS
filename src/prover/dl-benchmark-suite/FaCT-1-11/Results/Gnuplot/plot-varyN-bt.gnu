set terminal postscript eps monochrome dashed
set output "varyN-bt.eps"
set data style linespoints
#set ylabel "median backtracking search space"
set xlabel "L/N"
p(x) = 60 * x
set logscale y
plot "ps11-2d.data" using 1:9 title 'N=10', \
"ps10-2d.data" using 1:9 title 'N=8', \
"ps0-2d.data" using 1:9 title 'N=5', \
"ps5-2d.data" using 1:9 title 'N=4', \
"ps4-2d.data" using 1:9 title 'N=3'
