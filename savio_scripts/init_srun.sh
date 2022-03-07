#!/bin/bash
srun --job-name wfatplots -A fc_lmklab -p savio2 --nodes=1 -t 08:00:00 --ntasks-per-node=24 --pty /bin/bash
