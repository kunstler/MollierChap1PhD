#!/bin/sh
#SBATCH --job-name=PNV_Reptiles
#SBATCH -p q-128Go
#SBATCH --time=190:00:00                                    
#SBATCH -A u_emgr                                          
#SBATCH -o PNV_Reptiles%j.out
#SBATCH -e PNV_Reptiles%j.err
#SBATCH --cpus-per-task=1
#SBATCH --mail-type=ALL                                   
#SBATCH --mail-user=georges.kunstler@irstea.fr                                                                                                    
#SBATCH --mem=15000
  
module unload gcc
module load R/gcc
module load lapack/gcc/64/3.8.0-with-blas
module load nlopt/2.4.2                                                         

echo ${SLURM_JOB_NODELIST}
echo ${SLURM_ARRAY_TASK_ID}


# on lance le script R en precisant bien son chemin 
R CMD BATCH "--args index=${SLURM_ARRAY_TASK_ID} PN='PNV' GR='Reptiles'" Scripts/PN_GR_Seq.R PNV_Reptiles${SLURM_ARRAY_TASK_ID}.out
