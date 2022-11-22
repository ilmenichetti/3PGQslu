
#TODO
# - parsQlitter
# -edit the data for the scenario we want


library(readr) #to use tibbles, a bit more manageable data structure


my_site<-read_csv("./csv/d_site.csv")
my_species<-read_csv("./csv/d_species.csv")
my_climate<-read_csv("./csv/d_climate.csv")
my_thinning<-read_csv("./csv/d_thinning.csv")
my_parameters<-read_csv("./csv/d_parameters.csv")
my_sizeDist<-read_csv("./csv/d_sizeDist.csv")
my_parsQlitter<-read_csv("./csv/d_parsQlitter.csv")

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
