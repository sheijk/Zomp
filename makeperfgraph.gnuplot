set terminal png size 800, 600
set output "perf_results.png"
set style data lines
plot "tests/timing.txt"
