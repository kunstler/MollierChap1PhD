#!/bin/bash

for i in {1..10..1} 
do 
    R CMD BATCH "--args index=$i" Scripts/PNC_Arthros_Seq.R PNC_Arthros$i.out &
done



