#!/bin/bash

for i in {1..10..1} 
do 
    R CMD BATCH "--args index=$i" Scripts/PNE_Reptiles_Seq.R PNE_Reptiles$i.out &
done



