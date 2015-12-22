set datafile separator comma
set terminal png
set logscale x
set logscale y
set xlabel "N"
set ylabel "ns"
set style line 1 lt rgb "green"
set style line 2 lt rgb "red"
set style line 3 lt rgb "blue"
set key left top
set format "%'.0f"

file = "jmhBenchmarks/sonicreducer.csv"
set terminal png size 800, 600
set output "sonicreducer.png"
set title "sonicreducer"
bench = "com.rklaehn.abc.SonicReducerBench.createBulk"
plot \
  file using 8:((stringcolumn(1) eq "com.rklaehn.abc.SonicReducerBench.foldLeft")? $5:1/0) with linespoints title "foldLeft" ls 1, \
  file using 8:((stringcolumn(1) eq "com.rklaehn.abc.SonicReducerBench.reduceIterable")? $5:1/0) with linespoints title "reduceIterable" ls 2, \
  file using 8:((stringcolumn(1) eq "com.rklaehn.abc.SonicReducerBench.reduceArray")? $5:1/0) with linespoints title "reduceArray" ls 3, \
  file using 8:((stringcolumn(1) eq "com.rklaehn.abc.SonicReducerBench.stringBuilder")? $5:1/0) with linespoints title "stringBuilder" ls 4

file = "jmhBenchmarks/setcreate.csv"
set terminal png size 800, 1200
set output "setcreate.png"
set multiplot layout 2,1
set title "create bulk"
bench = "com.rklaehn.abc.SetCreateBench.createBulk"
plot \
  file using 9:((stringcolumn(8) eq "arrayset")  && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "arrayset" ls 1, \
  file using 9:((stringcolumn(8) eq "hashset")   && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "hashset" ls 2, \
  file using 9:((stringcolumn(8) eq "sortedset") && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "sortedset" ls 3

set title "create elementwise"
bench = "com.rklaehn.abc.SetCreateBench.createElements"
plot \
  file using 9:((stringcolumn(8) eq "arrayset")  && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "arrayset" ls 1, \
  file using 9:((stringcolumn(8) eq "hashset")   && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "hashset" ls 2, \
  file using 9:((stringcolumn(8) eq "sortedset") && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "sortedset" ls 3
unset multiplot

file = "jmhBenchmarks/setelement.csv"
set terminal png size 800, 1200
set output "setelement.png"
set multiplot layout 2,1

set title "contains true"
bench = "com.rklaehn.abc.SetElementBench.containsTrue"
plot \
  file using 9:((stringcolumn(8) eq "arrayset")  && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "arrayset" ls 1, \
  file using 9:((stringcolumn(8) eq "hashset")   && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "hashset" ls 2, \
  file using 9:((stringcolumn(8) eq "sortedset") && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "sortedset" ls 3

set title "contains false"
bench = "com.rklaehn.abc.SetElementBench.containsFalse"
plot \
  file using 9:((stringcolumn(8) eq "arrayset")  && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "arrayset" ls 1, \
  file using 9:((stringcolumn(8) eq "hashset")   && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "hashset" ls 2, \
  file using 9:((stringcolumn(8) eq "sortedset") && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "sortedset" ls 3
unset multiplot

file = "jmhBenchmarks/setset.csv"
set terminal png size 800, 2400
set output "setset.png"
set multiplot layout 4,1
set title "union"
bench = "com.rklaehn.abc.SetSetBench.union"
plot \
  file using 10:((stringcolumn(8) eq "arrayset")  && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "arrayset" ls 1, \
  file using 10:((stringcolumn(8) eq "hashset")   && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "hashset" ls 2, \
  file using 10:((stringcolumn(8) eq "sortedset") && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "sortedset" ls 3

set title "intersect"
bench = "com.rklaehn.abc.SetSetBench.intersect"
plot \
  file using 10:((stringcolumn(8) eq "arrayset")  && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "arrayset" ls 1, \
  file using 10:((stringcolumn(8) eq "hashset")   && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "hashset" ls 2, \
  file using 10:((stringcolumn(8) eq "sortedset") && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "sortedset" ls 3

set title "diff"
bench = "com.rklaehn.abc.SetSetBench.diff"
plot \
  file using 10:((stringcolumn(8) eq "arrayset")  && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "arrayset" ls 1, \
  file using 10:((stringcolumn(8) eq "hashset")   && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "hashset" ls 2, \
  file using 10:((stringcolumn(8) eq "sortedset") && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "sortedset" ls 3

set title "filter"
bench = "com.rklaehn.abc.SetSetBench.filter"
plot \
  file using 10:((stringcolumn(8) eq "arrayset")  && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "arrayset" ls 1, \
  file using 10:((stringcolumn(8) eq "hashset")   && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "hashset" ls 2, \
  file using 10:((stringcolumn(8) eq "sortedset") && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "sortedset" ls 3

set title "subsetOf"
bench = "com.rklaehn.abc.SetSetBench.subsetOf"
plot \
  file using 10:((stringcolumn(8) eq "arrayset")  && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "arrayset" ls 1, \
  file using 10:((stringcolumn(8) eq "hashset")   && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "hashset" ls 2, \
  file using 10:((stringcolumn(8) eq "sortedset") && (stringcolumn(1) eq bench)? $5:1/0) with linespoints title "sortedset" ls 3
unset multiplot
