#!/bin/bash
srun --job-name chunkwf -A fc_lmklab -p savio3 --nodes=1 -t 03:00:00 --ntasks-per-node=20 --pty bash
