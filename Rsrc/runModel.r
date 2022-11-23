library(r3PG)
library(dplyr)
library(ggplot2)
library(readxl)
library(data.table)
library(ggpubr)

###select path for data and set to working directory
pathX <- "C:/Users/39348/OneDrive/Documents/Github/3PGQslu"
setwd(pathX)

data_site <- read_excel('myData/INPUT_R_ALL_SITES.xlsx', sheet = 'd_site')
data_species <- read_excel('myData/INPUT_R_ALL_SITES.xlsx', sheet = 'd_species')
data_climate <- read_excel('myData/INPUT_R_ALL_SITES.xlsx', sheet= 'd_climate')
data_thinning <- read_excel('myData/INPUT_R_ALL_SITES.xlsx', sheet = 'd_thinnings')
data_parameters <- read_excel('myData/INPUT_R_ALL_SITES.xlsx', sheet = 'd_parameters')
data_sizeDist <- read_excel('myData/INPUT_R_ALL_SITES.xlsx', sheet = 'd_sizeDist')

###obsData 
obsData <- data.table(read_excel("myData/TABELLA_OBS.xlsx"))

Plot_ID <- data_site$Plot_ID
data_species$planted = as.character(data_species$planted)
data_species$fertility = as.double(data_species$fertility)


##### set data #####

all_site = data_site[,5:12]
all_species = data_species [,4:9]
all_climate = data_climate [,2:9]
all_thinning = data_thinning [,3:8]
all_parameters = data_parameters
all_sizeDist = data_sizeDist

##
my_out <- list()
for(i in Plot_ID){
  my_site <- data_site[data_site$Plot_ID==i,5:12]
  my_species <- data_species[data_species$Plot_ID == i,4:10]
  my_climate = all_climate
  my_thinning = data_thinning[data_thinning$Plot_ID==i,3:8]
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
extractData3PG <- function(out,plotX,varX){
  indX <- which(i_output==varX,arr.ind=T)
  groupID <- i_output[indX[1],]$group_id
  varID <- i_output[indX[1],]$variable_id
  outX <- out[[plotX]][,,groupID,varID]
  if(all(is.na(outX))){
    stop("check output variable. ",
         "choose variable betwee these: ",
         i_output[,3])
  }
  return(outX)
}

######### DATA'S SIMULATION #############
plotX = c(1:length(my_out))
varXs <- unique(obsData$var_name)
n_months =read_excel('myData/INPUT_R_ALL_SITES.xlsx', sheet = 'n_month')

datax_tab = data.table()
for (i in varXs) {   
  g = which(i_output[,3]==i) #variable row
  j = i_output[g,c(1,2)] #####group and variable ID####
  for (n in plotX) {
    p = which(n_months$Plot_ID==n)
    
    datax_tab = rbind(datax_tab, data.table(Plot_ID = n,
                                            n_month = n_months$n_mese[which(n_months$Plot_ID==n)],
                                            group = j[1],
                                            variable = j[2],
                                            value = my_out[[n]][n_months$n_mese[which(n_months$Plot_ID==n)],,as.numeric(j[1]),as.numeric(j[2])],
                                            var_name = i_output[g,3]))
    
    print(paste("Plot_ID",i))
  }
}

setnames(datax_tab,c('Plot_ID', 'n_month','group','variable','sim','var_name'))
datax_tab$data_type = "simulated"
obsXtab = obsData[,c(2:3,5:9)]
setnames(obsXtab,c('Plot_ID', 'n_month','group','variable','obs','var_name','data_type'))
setkey(datax_tab, Plot_ID, n_month,group,variable,var_name)
setkey(obsXtab, Plot_ID, n_month,group,variable,var_name)
all_data_tab = merge(datax_tab,obsXtab[data_type=='total'])
plot(all_data_tab$obs, all_data_tab$sim)
all_data_tab[var_name=='stems_n',plot(obs,sim)]
i = 1
all_data_tab[var_name ==varXs[i],plot(obs,sim,main=varXs[i])]


#per ogni dato metti in tab mese, layer, gruppo ,variabile
#calcola il residual= estrai tutti i dati simulati che corrispondono agli osservati (usa il n_mese)