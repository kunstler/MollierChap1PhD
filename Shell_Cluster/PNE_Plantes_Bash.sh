#!/bin/#!/bin/bash

for {1..10} 
do 
   R CMD BATCH "--args index=$i" Scripts/PNE_Plantes_Seq.R
   echo $i
done


