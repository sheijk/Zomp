set terminal png size 800, 600
set output "perf_results2.png"
set style data lines
plot "tests/timing_sexpr.txt", "tests/timing_iexpr.txt"
