#!/bin/bash +x 
#SBATCH -J HermesBatchRun
#SBATCH --time=12:10:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=80
#SBATCH --partition=compute
#SBATCH -o hermes-%j
#SBATCH --array=0-49

BATCHFILE=$1  #BBB/BBG_all.bat
EXECUTABLE=hermestogo
PROJECTDATA=./${BATCHFILE}
CMDLINE="-module batch -concurrent 70 -batch $PROJECTDATA -lines"

ARGS=($(./calcHermesBatch -list 50 -batch ${PROJECTDATA}))

echo $ARGS

srun ./${EXECUTABLE} ${CMDLINE} ${ARGS[$SLURM_ARRAY_TASK_ID]}
