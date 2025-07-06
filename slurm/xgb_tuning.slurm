#!/bin/bash
#SBATCH --job-name=xgb_hp
#SBATCH --output=../result/xgb_tuning_%j.out
#SBATCH --error=../result/xgb_tuning_%j.err
#SBATCH --time=04:00:00
#SBATCH --cpus-per-task=64
#SBATCH --mem=128G
#SBATCH --partition=cm4_inter
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=Ju.Haoran@campus.lmu.de

module load r/4.3.3-gcc13-mkl
export R_FUTURE_FORK_ENABLE=TRUE

cd ~/seminar
Rscript program/xgboost_hp.R
