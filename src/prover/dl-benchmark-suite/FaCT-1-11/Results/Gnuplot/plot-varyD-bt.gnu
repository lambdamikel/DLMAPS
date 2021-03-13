set terminal postscript eps monochrome dashed
set output "varyD-bt.eps"
set data style linespoints
#set ylabel "median backtracking search space"
set xlabel "L/N"
p(x) = 60 * x
set logscale y
plot "ps1-2d.data" using 1:9 title 'D=5', \
"ps2-2d.data" using 1:9 title 'D=4', \
"ps3-2d.data" using 1:9 title 'D=3', \
"ps4-2d.data" using 1:9 title 'D=2'
