#!/bin/bash

for i in {1..10..1} 
do 
    R CMD BATCH "--args index=$i" Scripts/PNM_Mammiferes_Seq.R PNM_Mammiferes$i.out &
done



