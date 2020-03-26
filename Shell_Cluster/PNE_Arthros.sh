#!/bin/sh
#SBATCH --job-name=PNE_Arthros
#SBATCH -p q-128Go
#SBATCH --time=190:00:00                                    
#SBATCH -A u_emgr                                          
#SBATCH -o PNE_Arthros%j.out
#SBATCH -e PNE_Arthros%j.err
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
R CMD BATCH "--args index=${SLURM_ARRAY_TASK_ID} PN='PNE' GR='Arthros'" Scripts/PN_GR_Seq.R PNE_Arthros${SLURM_ARRAY_TASK_ID}.out
