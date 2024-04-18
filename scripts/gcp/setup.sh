# https://posit.co/download/rstudio-server/
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/'
sudo apt update
sudo apt -y install r-base
sudo apt -y install libssl-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
	libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libxml2-dev libcurl4-openssl-dev

sudo apt -y install gdebi-core
wget https://download2.rstudio.org/server/focal/amd64/rstudio-server-2023.06.1-524-amd64.deb
sudo gdebi -n rstudio-server-2023.06.1-524-amd64.deb

sudo useradd rstudio -m
sudo passwd rstudio
