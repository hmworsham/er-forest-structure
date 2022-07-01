#!/bin/bash
srun --job-name checksums -A fc_lmklab -p savio --nodes=1 -t 03:00:00 \
--ntasks-per-node=20 \
--pty /bin/bash
