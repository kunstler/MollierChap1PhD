#!/bin/bash

for i in {1..10..1} 
do 
    R CMD BATCH "--args index=$i" Scripts/PNC_Reptiles_Seq.R PNC_Reptiles$i.out &
done



