#!/bin/bash
#
#SBATCH --job-name=test_omp
#SBATCH --output=out_slurm.txt
#SBATCH --ntasks=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=12
#SBATCH --time=1:00:00
#SBATCH --mem-per-cpu=1G
#SBATCH --gres=gpu:1

export OMP_NUM_THREADS=12
./blas
