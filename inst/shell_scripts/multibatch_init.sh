#!/bin/bash
sbatch ./batchcluster_init_li.sh
sbatch ./batchcluster_init_pt.sh
sbatch ./batchcluster_init_ls.sh
sbatch ./batchcluster_init_mc.sh
sbatch ./batchcluster_init_lmffw.sh
sbatch ./batchcluster_init_lmfvw.sh
#sbatch ./batchcluster_init_lmfauto.sh
sbatch ./batchcluster_init_ws.sh

#sbatch ./batchcluster_init_06.sh
#sbatch ./batchcluster_init_07.sh
#sbatch ./batchcluster_init_08.sh
#sbatch ./batchcluster_init_09.sh
#sbatch ./batchcluster_init_10.sh
