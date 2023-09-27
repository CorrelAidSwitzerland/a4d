# as sudo
sudo -u rstudio gsutil -m cp -r gs://a4dphase2_upload /home/rstudio/data
# as rstudio user (from Terminal in RStudio)
gsutil -m cp -r gs://a4dphase2_upload /home/rstudio/data
