
#TODO
# - parsQlitter
# -edit the data for the scenario we want


library(readxl) #to use tibbles, a bit more manageable data structure
library(r3PG) #to use tibbles, a bit more manageable data structure


my_site<-read_xlsx("./csv/poplar_3PG_data_and_parameters_test.xlsx", sheet = "d_site")
my_species<-read_xlsx("./csv/poplar_3PG_data_and_parameters_test.xlsx", sheet = "d_species")
my_climate<-read_xlsx("./csv/poplar_3PG_data_and_parameters_test.xlsx", sheet = "d_climate")
my_thinning<-read_xlsx("./csv/poplar_3PG_data_and_parameters_test.xlsx", sheet = "d_thinning")
my_parameters<-read_xlsx("./csv/poplar_3PG_data_and_parameters_test.xlsx", sheet = "d_parameters")
my_sizeDist<-read_xlsx("./csv/poplar_3PG_data_and_parameters_test.xlsx", sheet = "d_sizeDist")
my_parsQlitter<-read_xlsx("./csv/poplar_3PG_data_and_parameters_test.xlsx", sheet = "d_parsQlitter")

my_parameters<-read_xlsx("./csv/poplar_3PG_data_and_parameters_test.xlsx", sheet = "d_parameters")
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

stand_variable_names<-i_output[i_output$group_id==2,]
stand_out<-as.data.frame(my_out[,1,2,])
names(stand_out)<-stand_variable_names$variable_name
plot(stand_out$age, stand_out$basal_area, type="l", col="darkgreen",  ylab="Basal area", xlab="age")

plot(stand_out$age, stand_out$stems_n, type="l", col="darkgreen",  ylab="Basal area", xlab="age")
