#!/bin/bash
srun --job-name batchdcdc -A fc_lmklab -p savio3_bigmem --nodes=3 -t 08:00:00 --ntasks-per-node=32 --pty /bin/bash
