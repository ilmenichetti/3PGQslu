library(r3PG)
library(dplyr)
library(ggplot2)
library(readxl)
library(data.table)

###select path for data and set to working directory
## pathX <- "C:/Users/39348/OneDrive/Desktop/LAVORO_CHECCO/CALIBRAZIONE/"
# pathX <- "C:/Users/minunno/Documents/yucatrote/SLU/codes/"

# setwd(pathX)

data_site <- read_excel('myData/INPUT_R_1225sk2020_LM.xlsx', sheet = 'd_site')
data_species <- read_excel('myData/INPUT_R_1225sk2020_LM.xlsx', sheet = 'd_species')
data_thinning <- read_excel('myData/INPUT_R_1225sk2020_LM.xlsx', sheet = 'd_thinnings')
data_parameters <- read_excel('myData/INPUT_R_1225sk2020_LM.xlsx', sheet = 'd_parameters')
data_sizeDist <- read_excel('myData/INPUT_R_1225sk2020_LM.xlsx', sheet = 'd_sizeDist')


siteIDs <- data_site$siteID
data_species$planted = as.character(data_species$planted)
data_species$fertility = as.double(data_species$fertility)


##### set data #####

all_parameters = data_parameters
all_sizeDist = data_sizeDist
# #### Correzione errore anno ###
my_climate = rbind(d_climate,d_climate)
my_climate = rbind(my_climate,d_climate)
nYears = nrow(my_climate)/12
my_climate$year = as.double(rep(1987:(1987+nYears-1), each = 12))
my_climate = as.matrix(my_climate)

####loop sites
my_out <- list()
for(i in siteIDs){
  my_site <- data_site[data_site$siteID==i,3:10]
  my_species <- data_species[data_species$siteID == i,3:9]
  #my_climate = all_climate [i,]
  my_thinning = data_thinning[data_thinning$siteID==i,3:8]
  #my_thinning = all_thinning [i,]
  my_parameters = all_parameters
  my_sizeDist = all_sizeDist
  my_out[[i]] = run_3PG(
    site        = my_site,
    species     = my_species,
    climate     = my_climate,
    thinning    = my_thinning,
    parameters  = my_parameters,
    size_dist   = my_sizeDist,
    settings    = list(light_model = 2, transp_model = 2, phys_model = 2,
                       height_model = 1, correct_bias = 0, calculate_d13c = 0),
    check_input = TRUE, df_out = F)

  print(paste("siteID",i))
}
  
  
  
  
