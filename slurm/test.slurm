#!/bin/bash
#SBATCH --job-name=test_inter
#SBATCH --output=../result/test_inter.out
#SBATCH --error=../result/test_inter.err
#SBATCH --time=00:01:00
#SBATCH --partition=cm4_inter
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=112
#SBATCH --mem=256G
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=Ju.Haoran@campus.lmu.de

export R_FUTURE_FORK_ENABLE=FALSE
export R_PARALLELLY_LAUNCHER=srun
export R_PARALLELLY_BACKEND=12000:12020

module load r/4.3.3-gcc13-mkl

cd ~/seminar
Rscript program/test.R
