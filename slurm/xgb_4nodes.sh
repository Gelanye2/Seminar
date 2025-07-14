#!/bin/bash
#SBATCH --job-name=xgb_hp
#SBATCH --output=../result/xgb_tuning_%j.out
#SBATCH --error=../result/xgb_tuning_%j.err
#SBATCH --time=24:00:00
#SBATCH --clusters=cm4
#SBATCH --partition=cm4_std
#SBATCH --qos=cm4_std
#SBATCH --nodes=4
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=112
#SBATCH --mem=448G
#SBATCH --exclusive
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=Ju.Haoran@campus.lmu.de

module load r/4.3.3-gcc13-mkl
export R_FUTURE_FORK_ENABLE=FALSE
export R_PARALLELLY_LAUNCHER=srun

cd ~/seminar
Rscript program/xg_hp_4nodes.R
