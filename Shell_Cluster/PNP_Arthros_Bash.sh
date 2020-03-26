#!/bin/bash

for i in {1..10..1} 
do 
    R CMD BATCH "--args index=$i" Scripts/PNP_Arthros_Seq.R PNP_Arthros$i.out &
done



