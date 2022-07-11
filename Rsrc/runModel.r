library(r3PG)
library(dplyr)
library(ggplot2)
library(readxl)
library(data.table)
library(ggpubr)

###select path for data and set to working directory
## pathX <- "C:/Users/39348/OneDrive/Desktop/LAVORO_CHECCO/CALIBRAZIONE/"
pathX <- "C:/Users/minunno/Documents/github/3PGQslu/"
setwd(pathX)

data_site <- read_excel('myData/INPUT_R_1225sk2020_LM.xlsx', sheet = 'd_site')
data_species <- read_excel('myData/INPUT_R_1225sk2020_LM.xlsx', sheet = 'd_species')
#data_climate <- read_excel('myData/INPUT_R_1225sk2020_LM.xlsx', sheet = 'd_climate')
data_thinning <- read_excel('myData/INPUT_R_1225sk2020_LM.xlsx', sheet = 'd_thinnings')
data_parameters <- read_excel('myData/INPUT_R_1225sk2020_LM.xlsx', sheet = 'd_parameters')
data_sizeDist <- read_excel('myData/INPUT_R_1225sk2020_LM.xlsx', sheet = 'd_sizeDist')

###obsData 
obsData <- data.table(read_excel("myData/TABELLA_OBS.xlsx"))



siteIDs <- data_site$siteID
data_species$planted = as.character(data_species$planted)
data_species$fertility = as.double(data_species$fertility)


##### set data #####

# all_site = data_site[,3:10]
# all_species = data_species [,3:9]
# #all_climate = data_climate [,3:12]
# all_thinning = data_thinning [,3:8]
all_parameters = data_parameters
all_sizeDist = data_sizeDist
# #### Correzione errore anno ###
my_climate = rbind(d_climate,d_climate)
my_climate = rbind(my_climate,d_climate)
nYears = nrow(my_climate)/12
my_climate$year = as.double(rep(1987:(1987+nYears-1), each = 12))
my_climate = as.matrix(my_climate)

# ### correzione errore stringa ###
# my_species <- all_species[1,]
# my_species$planted = as.character(my_species$planted)
# my_species$fertility = as.double(my_species$fertility)
# #
# #
# #
# ####################################################
# #
# #
# #
# ####select a site and run model
# input_1225 = read_excel('INPUT_R_1225sk2020_LM.xlsx', sheet = 'input_marklund')
# #####
# #### SELECT SITE ###
# first_site = 1 ####solo per il primo sito
# last_site = 4 ####solo per il primo sito
# #site_sliding = function(input_1225,first_site,last_site){    ####dal secondo sito in poi
# #  if(input_1225$rev[last_site+6]==6){
# #   last_site = (first_site + 5)
# #}else{
# # last_site = (first_site + 3)
# #  }
# #}
# #first_site = (last_site + 1)    ####dal secondo sito in poi
# #last_site = site_sliding(input_1225,first_site,last_site)  ####dal secondo sito in poi
# ######
# # my_out = list()
# # all_thinning = list() ####chiedi spiegazioni
# # all_thinning[[1]] = ## e' = a nthinnigSites?
# #   nthinnigSites =  c(2,5,5,3,5,2,2,5,5,5,2,2,2,5,5,2,3,5,6,5,2,2,5,5)
# # nSites <- length(nthinnigSites)
# # first_thin <- c(1,cumsum(nthinnigSites)[1:(nSites-1)]+1) ####chiedi spiegazioni
# # last_thin <- cumsum(nthinnigSites)
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
  


#' extractData3PG
#'
#' @param out array with 3PG output
#' @param siteX site ID for which to extract the data
#' @param varX character string indicating the variable to extract
#'
#' @return vector of model output
#' @export
#'
#' @examples
extractData3PG <- function(out,siteX,varX){
  indX <- which(i_output==varX,arr.ind=T)
  groupID <- i_output[indX[1],]$group_id
  varID <- i_output[indX[1],]$variable_id
  outX <- out[[siteX]][,,groupID,varID]
  if(all(is.na(outX))){
    stop("check output variable. ",
         "choose variable betwee these: ",
         i_output[,3])
  }
  return(outX)
}

# obsData[var_name=="stem_n"]$var_name <- "stems_n"
varXs <- unique(obsData$var_name)
sites <- unique(obsData$site_id)
varX="height"
dataSim <- data.table()
pList <- list()
for(varX in varXs){
  outX <- extractData3PG(my_out,siteX,varX)
  simX <- data.table(site_id=siteX,n_month=1:length(outX),
                     layers_id=1,#####!!!!to make general and include in the extractData3PG function
                     value=outX,var_name=varX,
              data_type="modelled")
  dataSim <- rbind(dataSim,simX)

  pList[[varX]] <- ggplot() + ggtitle(varX) +
    geom_line(dataSim[var_name==varX],mapping=aes(x=n_month,y=value)) +
    geom_point(obsData[var_name==varX],mapping=aes(x=n_month,y=obs,col=data_type))
  print(varX)
  
}


p1 <- ggarrange(plotlist=pList[1:4], ncol=2,nrow=2,common.legend = T)
p2 <- ggarrange(plotlist=pList[5:8], ncol=2,nrow=2,common.legend = T)

print(p1)
print(p2)
