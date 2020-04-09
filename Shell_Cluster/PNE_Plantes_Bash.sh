#!/bin/bash

for i in {1..10..1} 
do 
    R CMD BATCH "--args index=$i" Scripts/PNE_Plantes_Seq.R PNE_Plantes$i.out &
done



