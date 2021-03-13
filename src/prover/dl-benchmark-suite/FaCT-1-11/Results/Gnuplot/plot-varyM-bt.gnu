set terminal postscript eps monochrome dashed
set output "varyM-bt.eps"
set data style linespoints
#set ylabel "median backtracking search space"
set xlabel "L/N"
p(x) = 60 * x
set logscale y
plot "ps5-2d.data" using 1:9 title 'M=1', \
"ps6-2d.data" using 1:9 title 'M=2', \
"ps7-2d.data" using 1:9 title 'M=5', \
"ps8-2d.data" using 1:9 title 'M=10', \
"ps9-2d.data" using 1:9 title 'M=20'
