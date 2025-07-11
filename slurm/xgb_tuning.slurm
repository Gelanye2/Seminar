#!/bin/bash
#SBATCH --job-name=xgb_hp
#SBATCH --output=../result/xgb_tuning_%j.out
#SBATCH --error=../result/xgb_tuning_%j.err
#SBATCH --time=12:00:00
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
Rscript program/xgboost_hp.R
