#!/bin/bash

for i in {1..10..1} 
do 
    R CMD BATCH "--args index=$i" Scripts/PNE_Oiseaux_Seq.R PNE_Oiseaux$i.out &
done



