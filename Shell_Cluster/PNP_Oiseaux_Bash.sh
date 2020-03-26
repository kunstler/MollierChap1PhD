#!/bin/bash

for i in {1..10..1} 
do 
    R CMD BATCH "--args index=$i" Scripts/PNP_Oiseaux_Seq.R PNP_Oiseaux$i.out &
done



