set terminal png size 800, 600
set output "temp.png"
set style data lines
plot "tests/timing.txt"
