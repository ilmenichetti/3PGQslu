# this is needed to run python inside R
library(reticulate)

library(data.table)
library(plyr)
library(tidyverse)
library(sf)
library(raster)

source("Rsrc/extractWeatherFun.r")

# specify here the path of python in your computer
# the code works with python 2 but not with python 3
use_python("C:/Python27")

source_python("Rsrc/clipick.py")
##if error:
# devtools::install_github("rstudio/reticulate")
# reticulate::install_miniconda()

coord <- data.table(lat=56,long=13)
ciao <- getWDpoints(coord)
ciao
#test
