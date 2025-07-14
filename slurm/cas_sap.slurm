#!/bin/bash
#SBATCH --job-name=cas_sap
#SBATCH --output=../result/cas_sap.out
#SBATCH --error=../result/cas_sap.err
#SBATCH --time=05:00:00
#SBATCH --partition=cm4_inter
#SBATCH --nodes=1
#SBATCH --cpus-per-task=112
#SBATCH --mem=256G
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=Ju.Haoran@campus.lmu.de

export R_FUTURE_FORK_ENABLE=TRUE

module load r/4.3.3-gcc13-mkl

cd ~/seminar
Rscript program/cascading_submission.R
