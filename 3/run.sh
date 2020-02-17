#!/bin/bash

gfortran -c -o A.o precisione/precisione.f90
gfortran -c -o B.o random/randomseed.f90
gfortran -c -o C.o random/normalgen.f90
gfortran -c -o D.o chi/chi_quadrato.f90
gfortran -c -o E.o write_to_file/write_to_file.f90
gfortran -c -o F.o histogram/histogram.f90

gfortran ex3.f90 A.o B.o C.o D.o E.o F.o

./a.out

rm a.out
rm *.mod
rm *.o
gnuplot
#gnuplot -e "load 'plot.txt'"
