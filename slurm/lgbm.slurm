#!/bin/bash
#SBATCH --job-name=lgbm_base
#SBATCH --output=../result/lgbm_base.out
#SBATCH --error=../result/lgbm_base.err
#SBATCH --time=08:00:00
#SBATCH --partition=cm4_inter
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=112
#SBATCH --mem=256G
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=Ju.Haoran@campus.lmu.de

export R_FUTURE_FORK_ENABLE=FALSE

module load r/4.3.3-gcc13-mkl

cd ~/seminar
Rscript program/lgbm.R
