set terminal postscript eps monochrome dashed
set output "ps8-2d.eps"
set data style linespoints
set ylabel "percentile times (s)"
set xlabel "L/N"
p(x) = 60 * x
set logscale y
plot "ps8-2d.data" using 1:3 title '50%', \
"ps8-2d.data" using 1:4 title '60%', \
"ps8-2d.data" using 1:5 title '70%', \
"ps8-2d.data" using 1:6 title '80%', \
"ps8-2d.data" using 1:7 title '90%', \
"ps8-2d.data" using 1:8 title '100%'
