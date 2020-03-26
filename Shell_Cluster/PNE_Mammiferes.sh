#!/bin/sh
#SBATCH --job-name=PNE_Mammiferes
#SBATCH -p q-128Go
#SBATCH --time=190:00:00                                    
#SBATCH -A u_emgr                                          
#SBATCH -o PNE_Mammiferes%j.out
#SBATCH -e PNE_Mammiferes%j.err
#SBATCH --cpus-per-task=1
#SBATCH --mail-type=ALL                                   
#SBATCH --mail-user=georges.kunstler@irstea.fr  
#SBATCH --mem=15000
  
module unload gcc
module load R/gcc_5.2.0/64/3.6.1

echo ${SLURM_JOB_NODELIST}
echo ${SLURM_ARRAY_TASK_ID}


# on lance le script R en precisant bien son chemin 
R CMD BATCH "--args index=${SLURM_ARRAY_TASK_ID} PN='PNE' GR='Mammiferes'" Scripts/PN_GR_Seq.R PNE_Mammiferes${SLURM_ARRAY_TASK_ID}.out
