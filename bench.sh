#!/bin/bash
sbt "jmhBenchmarks/jmh:run -i 5 -wi 5 -f1 -t1 -rf csv -rff setelement.csv SetElementBench" 
sbt "jmhBenchmarks/jmh:run -i 5 -wi 5 -f1 -t1 -rf csv -rff setset.csv SetSetBench" 
gnuplot plot.gnu
