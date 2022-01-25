#!/bin/bash
srun --job-name chunkwf -A fc_lmklab -p savio3 --nodes=1 -t 02:00:00 --ntasks-per-node=32 --pty bash
