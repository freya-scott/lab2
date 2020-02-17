#!/bin/bash

gfortran -c -o A.o precisione/precisione.f90
gfortran -c -o B.o read_data/read_data.f90
gfortran -c -o C.o write_to_file/write.f90
gfortran -c -o D.o histogram/histogram.f90
gfortran -c -o E.o stime/media_stima.f90
gfortran -c -o F.o stime/minimi_quadrati.f90

gfortran cosmic.f90 A.o B.o C.o D.o E.o F.o

./a.out

rm a.out
rm *.mod
rm *.o
