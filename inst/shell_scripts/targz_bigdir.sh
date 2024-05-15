#!/bin/bash

# Tar and compress a large directory with subdirectories

scratch=/global/scratch/users/worsham/waveform_lidar_chunks_1e5/

cd $scratch

for i in *
	do
		tar cf - $i | pigz > $i.tar.gz;
	done
