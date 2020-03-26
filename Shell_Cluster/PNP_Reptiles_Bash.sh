#!/bin/bash

for i in {1..10..1} 
do 
    R CMD BATCH "--args index=$i" Scripts/PNP_Reptiles_Seq.R PNP_Reptiles$i.out &
done



