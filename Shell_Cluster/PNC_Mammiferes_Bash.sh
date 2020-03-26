#!/bin/bash

for i in {1..10..1} 
do 
    R CMD BATCH "--args index=$i" Scripts/PNC_Mammiferes_Seq.R PNC_Mammiferes$i.out &
done



