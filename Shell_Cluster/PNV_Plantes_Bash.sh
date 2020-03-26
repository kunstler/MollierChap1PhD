#!/bin/#!/bin/bash

for i in {1..10..1} 
do 
    R CMD BATCH "--args index=$i" Scripts/PNV_Plantes_Seq.R PNV_Plantes$i.out &
done



