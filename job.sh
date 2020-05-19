#!/bin/sh
#$ -V
#$ -t 1-500:1 
#$ -cwd
#$ -S /bin/bash
#$ -N RJOB
#$ -o Output/$JOB_NAME.o$TASK_ID
#$ -e Output/$JOB_NAME.e$TASK_ID
#$ -q omni
#$ -P quanah


#Load the latest version of the R language - compiled using the Intel compilers.
module load intel R

#Run the example R script using the Rscript application.

echo Starting task $SGE_TASK_ID

Rscript R_testscript.R Output/$R-${JOB_ID}.Rout

