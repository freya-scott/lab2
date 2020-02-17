#!/bin/bash

gfortran es1module.f90 es1.f90
./a.out

rm a.out
rm *.mod
