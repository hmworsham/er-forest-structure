#!/bin/bash
srun --job-name processwf -A fc_lmklab -p savio3 --nodes=5 -t 10:00:00 --ntasks-per-node=32 \
--pty /bin/bash
