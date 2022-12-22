library(r3PG)
library(dplyr)
library(ggplot2)
library(readxl)
library(data.table)
library(ggpubr)
library(remotes)

library(devtools)
# vRef <- "master"   #### choose package version to run the model  "master" "v0.2.x"
# devtools::install_github("ForModLabUHel/r3PG", vRef)
devtools::install_github("ilmenichetti/r3PG", subdir = "pkg", build_vignettes = T)
#devtools::install_github(repo = "ForModLabUHel/r3PG", subdir = "pkg", build_vignettes = T)


soilInit <- matrix(c(10,5,6,3,10,5,20,10,0,0),2,5)
testRun <- run_3PG(site=d_site,species=d_species,
                   climate = d_climate,
                   df_out = FALSE,soil = soilInit)
plot(testRun[,1,9,6])
points(testRun[,2,9,6],col=2,pch=20)

###select path for data and set to working directory
## pathX <- "C:/Users/39348/OneDrive/Desktop/LAVORO_CHECCO/CALIBRAZIONE/"
# pathX <- "C:/Users/minunno/Documents/github/3PGQslu/"
pathX <- "C:/Users/39348/OneDrive/Documents/Github/3PGQslu"
setwd(pathX)

data_site <- read_excel('myData/INPUT_R_ALL_SITES.xlsx', sheet = 'd_site')
data_species <- read_excel('myData/INPUT_R_ALL_SITES.xlsx', sheet = 'd_species')
data_climate <- read_excel('myData/INPUT_R_ALL_SITES.xlsx', sheet= 'd_climate')
data_thinning <- read_excel('myData/INPUT_R_ALL_SITES.xlsx', sheet = 'd_thinnings')
data_parameters <- read_excel('myData/INPUT_R_ALL_SITES.xlsx', sheet = 'd_parameters')
data_sizeDist <- read_excel('myData/INPUT_R_ALL_SITES.xlsx', sheet = 'd_sizeDist')

# ###obsData 
# unique(obsData$site_id)
# data_site <- read_excel('../myData/INPUT_R_1225sk2020_LM.xlsx', sheet = 'd_site')
# data_species <- read_excel('../myData/INPUT_R_1225sk2020_LM.xlsx', sheet = 'd_species')
# data_climate <- read_excel('../myData/INPUT_R_1225sk2020_LM.xlsx', sheet = 'd_climate')
# data_thinning <- read_excel('../myData/INPUT_R_1225sk2020_LM.xlsx', sheet = 'd_thinnings')
# data_parameters <- read_excel('../myData/INPUT_R_1225sk2020_LM.xlsx', sheet = 'd_parameters')
# data_sizeDist <- read_excel('../myData/INPUT_R_1225sk2020_LM.xlsx', sheet = 'd_sizeDist')
# 
# ###obsData 
# obsData <- data.table(read_excel("../myData/TABELLA_OBS.xlsx"))


#siteIDs <- data_site$Site_ID
Plot_ID <- data_site$Plot_ID
data_species$planted = as.character(data_species$planted)
data_species$fertility = as.double(data_species$fertility)


Skogaby <- data.table(read_excel("Vignette_example/Skogaby_measurements.xlsx"))


##### set data #####
###JUST SKOGABY

all_site = data_site[data_site$siteName=="Skogaby",5:12]
all_species = data_species [data_species$siteName=="Skogaby",4:9]
all_climate = data_climate [,2:9]
all_thinning = data_thinning [data_thinning$siteName=="Skogaby",3:8]
all_parameters = data_parameters
all_sizeDist = data_sizeDist

write.xlsx(data_site[data_site$siteName=="Skogaby",], file = "Skogaby_example.xlsx", sheetName = "data_site")
write.xlsx(data_species[data_species$siteName=="Skogaby",], file = "Skogaby_example.xlsx", sheetName = "data_species", append=T)
write.xlsx(data_climate, file = "Skogaby_example.xlsx", sheetName = "data_climate", append=T)
write.xlsx(data_thinning[data_thinning$siteName=="Skogaby",], file = "Skogaby_example.xlsx", sheetName = "data_thinning", append=T)
write.xlsx(data_parameters, file = "Skogaby_example.xlsx", sheetName = "data_parameters", append=T)
write.xlsx(data_sizeDist, file = "Skogaby_example.xlsx", sheetName = "data_sizeDist", append=T)

Plot_ID <- data_site[data_site$siteName=="Skogaby",]$Plot_ID 


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

range((my_out[[i]][,1,2,1]))

#plotting DBH
stand_variable_names<-i_output[i_output$group_id==2,]

for(i in Plot_ID){
stand_out_ID1<-as.data.frame(my_out[[i]][,1,2,])
names(stand_out_ID1)<-stand_variable_names$variable_name

plot(stand_out_ID1$age, stand_out_ID1$basal_area, type="l", col="darkgreen", ylim=c(0, max(stand_out_ID1$basal_area)), ylab="Basal area", xlab="age")
Skogaby_data_time<-Skogaby[Skogaby$SiteID==i,]$Year-Skogaby[Skogaby$SiteID==i,]$Year[1]+stand_out_ID1$age[1]
points(Skogaby_data_time, Skogaby[Skogaby$SiteID==i,]$BasalArea, pch=16)

}


run
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


varXs <- unique(obsData$var_name)
outX <- extractData3PG(my_out,siteX[1],varXs[1])

str(outX)





# obsData[var_name=="stem_n"]$var_name <- "stems_n"
all_plot <- c(1:length(my_out))
#siteX <- 1
varXs <- unique(obsData$var_name)
sites <- unique(obsData$site_id)
varX="dbh"
dataSim <- data.table()
pList <- list()

for (i in all_plot) {    #################controlla con Checco se va bene ##########
  siteX = all_plot[i]
  obsData[siteX]
  for(varX in varXs){
    outX <- extractData3PG(my_out,siteX,varX)
    simX <- data.table(site_id=siteX,n_month=1:length(outX),
                       layers_id=1,#####!!!!to make general and include in the extractData3PG function
                       value=outX,var_name=varX,
                       data_type="modelled")
    dataSim <- rbind(dataSim,simX)
    
    obsData$n_month <- as.integer(obsData$n_month)
    obsData$obs <- as.numeric(obsData$obs)
    #make plot
    pList[[varX]] <- ggplot() + ggtitle(varX) +
      geom_line(dataSim[var_name==varX],mapping=aes(x=n_month,y=value)) +
      geom_point(obsData[site_id==i & var_name==varX],mapping=aes(x=n_month,y=obs,col=as.factor(data_type)))
    print(varX)
  }
}


str(simX)

p1 <- ggarrange(plotlist=pList[1:4], ncol=2,nrow=2,common.legend = T)
p2 <- ggarrange(plotlist=pList[5:8], ncol=2,nrow=2,common.legend = T)

print(p1)
print(p2)

str(pList)

######
which(i_output[,3]=="dbh")
i_output[20,c(1,2)]
my_out[[1]][72,,2,5]####[[]] indica il Plot_ID? devi creare una lista di tabelle datax_list 

#per ogni dato metti in tab mese, layer, gruppo ,variabile
#calcola il residual= estrai tutti i dati simulati che corrispondono agli osservati (usa il n_mese)

