#!/bin/bash

for i in {1..10..1} 
do 
    R CMD BATCH "--args index=$i" Scripts/PNV_Mammiferes_Seq.R PNV_Mammiferes$i.out &
done



