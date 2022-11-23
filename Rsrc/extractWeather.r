# this is needed to run python inside R
library(reticulate)
library(data.table)
library(plyr)
library(tidyverse)
library(sf)
library(raster)
##########
# coordPlots = coordinates of plots; must have columns long and lat
# sY = start year, sM = start month, sD = start day, eY = end year, eM = end month, eD = end day
getWDpoints <- function(coordPlots, sY=1951, sM=1, sD=1, eY=2100, eM=12, eD=31) {
  # here we store the weather data
  wDs <- list()
  
  # this is the number of coordinate points
  nDFs <- nrow(coordPlots)
  
  for (i in 1:nDFs) {
    lon <- coordPlots$long[i]
    lat <- coordPlots$lat[i]
    
    # getting the weather data from clipick
    result <- getWeatherData(lon, lat, sY, sM, sD, eY, eM, eD)
    
    # weather datas as data.table
    wD <- data.table(t(sapply(result,c)))
    
    # fix the column names
    names(wD) <- as.character(wD[1,])
    wD <- wD[-1,]
    
    # this is the number of the day
    wD$rday <- 1:nrow(wD)
    
    wDs[[i]] <- wD
  }
  
  return(wDs)
}

#####################

setwd("C:/Users/39348/OneDrive/Documents/Github/3PGQslu")
source("Rsrc/extractWeatherFun.r")

# specify here the path of python in your computer
# the code works with python 2 but not with python 3
use_python("C:/Python27")

source_python("Rsrc/clipick.py")
##if error:
# devtools::install_github("rstudio/reticulate")
# reticulate::install_miniconda()


coord <- data.table(lat=60.2833333,long=14.969444444444445)
clim <- getWDpoints(coord)
x = as.data.frame(clim)

write.csv(clim,file = "C:/Users/39348/OneDrive/Desktop/LAVORO_CHECCO/CALIBRAZIONE/Allometry/DATI_CLIMATICI/Dati_giornalieri_estratti/climate_data_Nyhammar.csv",)
