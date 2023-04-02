#!/bin/bash

# Update aptitude
sudo apt update -y

if [ ! -r ~/R ]
then

	# Install build dependencies
	sudo apt-get install libssl1.0.0_1.0.2g-1ubuntu4.19_amd64.deb \ 
	r-base libapparmor1 \
	gdebi-core 

	# Clone repo
	wget http://download2.rstudio.org/rstudio-server-0.97.336-amd64.deb -O ~/.rstudio.deb

	# Configure environment
	sudo gdebi rstudio.deb

	# Create RStudio user
	sudo adduser rstudio
fi

exec "$SHELL"
