library(r3PG)
library(readxl)

# library(devtools)
# devtools::install_github(repo = "ForModLabUHel/r3PG", subdir = "pkg", build_vignettes = T)


data_site <- read_excel('Skogaby_example_Riitta.xlsx', sheet = 'data_site')
data_species <- read_excel('Skogaby_example_Riitta.xlsx', sheet = 'data_species')
data_climate <- read_excel('Skogaby_example_Riitta.xlsx', sheet= 'data_climate')
data_thinning <- read_excel('Skogaby_example_Riitta.xlsx', sheet = 'data_thinning')
data_parameters <- read_excel('Skogaby_example_Riitta.xlsx', sheet = 'data_parameters')
data_sizeDist <- read_excel('Skogaby_example_Riitta.xlsx', sheet = 'data_sizeDist')


  my_out = run_3PG(
    site        = data_site,
    species     = data_species,
    climate     = data_climate,
    thinning    = data_thinning,
    parameters  = data_parameters,
    size_dist   = data_sizeDist,
    settings    = list(light_model = 2, transp_model = 2, phys_model = 2,
                       height_model = 1, correct_bias = 0, calculate_d13c = 0),
    check_input = TRUE, df_out = F)
  




#plotting results against data
Skogaby <- (read_excel("Skogaby_measurements.xlsx"))
Skogaby[Skogaby==0]=NA

#plotting DBH
data("i_output")
stand_variable_names<-i_output[i_output$group_id==2,]


stand_out_ID1<-as.data.frame(my_out[,1,2,])
names(stand_out_ID1)<-stand_variable_names$variable_name
plot(stand_out_ID1$age, stand_out_ID1$basal_area, type="l", col="darkgreen", ylab="Basal area", xlab="age")

plot(stand_out_ID1$age, stand_out_ID1$stems_n, type="l", col="darkgreen", ylab="Basal area", xlab="age")

write.csv(stand_out_ID1,"Simulation:SI_34.csv")
