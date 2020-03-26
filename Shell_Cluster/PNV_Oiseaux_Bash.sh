#!/bin/bash

for i in {1..10..1} 
do 
    R CMD BATCH "--args index=$i" Scripts/PNV_Oiseaux_Seq.R PNV_Oiseaux$i.out &
done



