#!/bin/bash

for i in {1..10..1} 
do 
    R CMD BATCH "--args index=$i" Scripts/PNE_Arthros_Seq.R PNE_Arthros$i.out &
done



