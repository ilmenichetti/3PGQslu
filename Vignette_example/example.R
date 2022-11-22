
#TODO
# - parsQlitter
# -edit the data for the scenario we want
#Possibile modifica da fare nel pacchetto: ho notato che se inserisco come input dei parametri della roba a caso (sono stringhe, per errore, perché ci ho copiato dei valori insieme al C.I. quindi con parentesi) il comando li digerisce senza problemi (solo in output c'é NA)

#TODO r3PG documentation
# soil initialization documentation (parameter "soil" in the command)
# soil is an array con [mesi (n), coorti forestali (? species name?), pools (massa)]
# add steady state calculation in case init=NULL?

#TODO r3PGQ
#initialization of q0

library(tidyverse)
library(readxl)
library(r3PG)
library(xlsx)

# new_parameters<-my_parameters
# new_parameters_corrected<-my_parameters
#  
#  for(j in 4:dim(new_parameters)[2]){
#    for( i in 1:dim(new_parameters)[1]){
#    if(!is.na(new_parameters[i,j])){
#      new_parameters_corrected[i,j]<- strsplit(as.character(new_parameters[i,j]), split = " ")[[1]][1]
#    } else {new_parameters_corrected[i,j]<- NA}
#    }
#  }
#  write.csv(new_parameters_corrected, file="new_parameters_corr")



my_site<-read_excel("birch_scenario.xlsx", sheet="d_site")
my_species<-read_excel("birch_scenario.xlsx", sheet="d_species")
my_climate<-read_excel("birch_scenario.xlsx", sheet="d_climate")
my_thinning<-read_excel("birch_scenario.xlsx", sheet="d_thinning")
my_parameters<-read_excel("birch_scenario.xlsx", sheet="d_parameters")
my_sizeDist<-read_excel("birch_scenario.xlsx", sheet="d_sizeDist")
my_parsQlitter<-read_excel("birch_scenario.xlsx", sheet="d_parsQlitter")

def_parameters<-read_csv("./csv/d_parameters.csv")
# 
# #reorder the parameter list
# my_parameters<-my_parameters[order(match(my_parameters$parameter, def_parameters$parameter)),]
# 
# colnames(my_parameters)
# 
# my_parameters
# def_parameters
# 
# all_equal(my_parameters, def_parameters)
# my_parameters$`Fagus sylvatica`== def_parameters$`Fagus sylvatica`
# 
# my_parameters$`Fagus sylvatica`[2]
# def_parameters$`Fagus sylvatica`[2]

# my_site<-read_csv("./csv/d_site.csv")
# my_species<-read_csv("./csv/d_species.csv")
# my_climate<-read_csv("./csv/d_climate.csv")
# my_thinning<-read_csv("./csv/d_thinning.csv")
# my_parameters<-read_csv("./csv/d_parameters.csv")
# my_sizeDist<-read_csv("./csv/d_sizeDist.csv")
# my_parsQlitter<-read_csv("./csv/d_parsQlitter.csv")
# 
# 
#my_species<-my_species[2,]

my_out = run_3PG(
  site        = my_site,
  species     = my_species,
  climate     = my_climate,
  thinning    = my_thinning,
  parameters  = my_parameters,
  parsQlitter  = my_parsQlitter,
  size_dist   = my_sizeDist,
  settings    = list(light_model = 2, transp_model = 2, phys_model = 2,
                     height_model = 1, correct_bias = 0, calculate_d13c = 0),
  check_input = TRUE, df_out = F)


str(my_out)

#all months, species, stand variables, all
stand_variable_names<-i_output[i_output$group_id==2,]
stand_out_betula<-as.data.frame(my_out[,1,2,])
names(stand_out_betula)<-stand_variable_names$variable_name
plot(stand_out_betula$age, stand_out_betula$basal_area)

plot(stand_out_betula$age, stand_out_betula$stems_n)

stand_out_pine<-as.data.frame(my_out[,2,2,])
names(stand_out_pine)<-stand_variable_names$variable_name
plot(stand_out_pine$age, stand_out_pine$stems_n)
