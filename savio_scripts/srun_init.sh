#!/bin/bash
srun --job-name hpc -A fc_lmklab -p savio3_bigmem --nodes=1 -t 04:00:00 --ntasks-per-node=32 \
--pty /bin/bash
