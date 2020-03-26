#!/bin/sh
#SBATCH --job-name=PNP_Plantes
#SBATCH -p q-128Go
#SBATCH --time=190:00:00                                    
#SBATCH -A u_emgr                                          
#SBATCH -o PNP_Plantes%j.out
#SBATCH -e PNP_Plantes%j.err
#SBATCH --cpus-per-task=1
#SBATCH --mail-type=ALL                                   
#SBATCH --mail-user=georges.kunstler@irstea.fr  
#SBATCH --mem=15000
  
module unload gcc
module load R/gcc_5.2.0/64/3.6.1

echo ${SLURM_JOB_NODELIST}
echo ${SLURM_ARRAY_TASK_ID}


# on lance le script R en precisant bien son chemin 
R CMD BATCH "--args index=${SLURM_ARRAY_TASK_ID}" Scripts/PNP_Plantes_Seq.R PNP_Plantes${SLURM_ARRAY_TASK_ID}.out
