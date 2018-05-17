#!/usr/bin/csh

#SBATCH --job-name=Serial_Test_Job
#SBATCH --ntasks=1 --constraint=hasw
#SBATCH --time=1:00:00
#SBATCH -o output.%j
#SBATCH -e error.%j
#SBATCH -qos=debug
#SBATCH --account=pmalexan
#SBATCH --workdir=/discover/nobackup/pmalexan/SISVAT_postProc/

/discover/nobackup/pmalexan/SISVAT.ETH-Camp-GL/ECsvat.sh
