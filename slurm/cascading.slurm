#!/bin/bash
#SBATCH --job-name=cascading
#SBATCH --output=../result/cascading_%j.out
#SBATCH --error=../result/cascading_%j.err
#SBATCH --time=24:00:00
#SBATCH --clusters=cm4
#SBATCH --partition=cm4_tiny
#SBATCH --qos=cm4_tiny
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=112
#SBATCH --mem=256G
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=Ju.Haoran@campus.lmu.de


module load r/4.3.3-gcc13-mkl
export R_FUTURE_FORK_ENABLE=TRUE

cd ~/seminar
Rscript program/cascading_tuning.R
Rscript program/cascading_submission.R
