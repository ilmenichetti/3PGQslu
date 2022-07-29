# this is needed to run python inside R
library(reticulate)

library(readxl)
library(data.table)
library(plyr)
library(tidyverse)
library(sf)
library(raster)

source("Rsrc/extractWeatherFun.r")
coords <- read_excel('myData/Sites_Lon_Lat.xlsx')

# specify here the path of python in your computer
# the code works with python 2 but not with python 3
use_python("C:/Python27")

source_python("Rsrc/clipick.py")
##if error:
# devtools::install_github("rstudio/reticulate")
# reticulate::install_miniconda()

##extract weather data
WD <- getWDpoints(coords)

###convert weather data
for(i in length(WD)){
  WD[[i]]$PAR<-as.numeric(WD[[i]]$rss)*0.44*4.56
  # MJ / sq meter TO mol/m2 # 4.56 (?mol/J* or ?mol s???1W*???1)
  # 0.44 Percents of the energy of solar radiation is actually in the 400 - 700 nm range  
  WD[[i]]$TAir<-(as.numeric(WD[[i]]$tasmax)+
                   as.numeric(WD[[i]]$tasmin))/2
  SVP <-  610.7*10^(7.5*as.numeric(WD[[i]]$TAir)/
                      (237.3+as.numeric(WD[[i]]$TAir)))
  WD[[i]]$VPD <- SVP*(1-(as.numeric(WD[[i]]$hursmax)+
                as.numeric(WD[[i]]$hursmin))/2/100)/1000
  WD[[i]]$Precip<-as.numeric(WD[[i]]$pr)
}
