#!/bin/bash

for i in {1..10..1} 
do 
    R CMD BATCH "--args index=$i" Scripts/PNC_Oiseaux_Seq.R PNC_Oiseaux$i.out &
done



