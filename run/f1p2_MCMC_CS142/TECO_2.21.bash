#!/bin/bash
#PBS -l walltime=48:00:00
#PBS -N 142_p2
#PBS -o /scratch_lg/cardamom/shuangma/TECO/TECO_2.21/run/f1p2_MCMC_CS142/output/out.o
#PBS -e /scratch_lg/cardamom/shuangma/TECO/TECO_2.21/run/f1p2_MCMC_CS142/output/error.err
#PBS -k oed
#PBS -l mem=400mb
#PBS -m abe
#PBS -M shuang.ma@jpl.nasa.gov
#PBS -q array

### set working directory
cd /scratch_lg/cardamom/shuangma/TECO/TECO_2.21/run/f1p2_MCMC_CS142
./TECO_2.21
