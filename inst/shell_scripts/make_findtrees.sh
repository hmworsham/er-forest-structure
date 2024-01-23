#!/bin/bash

## Define iterators
i=1
end=65
seriesi=1
seriesend=5

## Split up R files into chunks
basename=../notebooks/LiDAR/05.11_detect_trees_full_watershed.R
outname=../notebooks/LiDAR/05.11_detect_trees_full_watershed_$i.R

while [ $i -le $end ]; do
    echo $i
    echo $seriesi
    echo $seriesend
    outname=../notebooks/LiDAR/05.11_detect_trees_full_watershed_$i.R
    cp $basename $outname
    echo -e "print('SET: $seriesi:$seriesend')
	  lapply(set5[$seriesi:$seriesend], function(x) {
                 plan(multisession, workers=30L)
		 print(lascat[x,]\$filename)
                 ls.trees <- find_trees(lascat[x,], 
                                        algo,
                                        uniqueness='bitmerge')
                 })" >> $outname

    touch batchcluster_init_findtrees_$i.sh
    echo "#!/bin/bash

#SBATCH --job-name=findtrees_$i
#SBATCH --account=fc_lmklab
#SBATCH --partition=savio3
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32
#SBATCH --time=23:00:00

## sbatch commands to run
module load r r-packages r-spatial
while true
do
R CMD BATCH --no-save $outname
done" >> batchcluster_init_findtrees_$i.sh

    chmod +rwx batchcluster_init_findtrees_$i.sh

    ## append slurm init file to multibatch script
    echo "sbatch ./batchcluster_init_findtrees_$i.sh" >> multibatch_findtrees_init.sh

    ## Update iterators
    i=$(($i+1))
    seriesi=$(($seriesi+5))
    seriesend=$(($seriesend+5))
done
