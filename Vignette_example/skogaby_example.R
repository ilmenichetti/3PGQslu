library(r3PG)
library(readxl)

# library(devtools)
# devtools::install_github(repo = "ForModLabUHel/r3PG", subdir = "pkg", build_vignettes = T)


data_site <- read_excel('Skogaby_example.xlsx', sheet = 'data_site')
data_species <- read_excel('Skogaby_example.xlsx', sheet = 'data_species')
data_climate <- read_excel('Skogaby_example.xlsx', sheet= 'data_climate')
data_thinning <- read_excel('Skogaby_example.xlsx', sheet = 'data_thinning')
data_parameters <- read_excel('Skogaby_example.xlsx', sheet = 'data_parameters')
data_sizeDist <- read_excel('Skogaby_example.xlsx', sheet = 'data_sizeDist')

#siteIDs <- data_site$Site_ID
Plot_ID <- data_site$Plot_ID


## run the simulation for each plot
my_out <- list()
for(i in Plot_ID){
  my_site <- data_site[data_site$Plot_ID==i,5:12]
  my_species <- data_species[data_species$Plot_ID == i,4:10]
  my_climate = data_climate
  my_thinning = data_thinning[data_thinning$Plot_ID==i,3:8]
  my_parameters = data_parameters
  my_sizeDist = data_sizeDist
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



#plotting results against data
Skogaby <- (read_excel("Skogaby_measurements.xlsx"))
Skogaby[Skogaby==0]=NA

#plotting DBH
data("i_output")
stand_variable_names<-i_output[i_output$group_id==2,]

png("basal_area_sim_skogaby.png", height=3000, width=5000, res=300)
par(mfrow=c(4,6))

treatments<-unique(Skogaby$Treatment)

for(i in Plot_ID){
stand_out_ID1<-as.data.frame(my_out[[i]][,1,2,])
names(stand_out_ID1)<-stand_variable_names$variable_name
plot(stand_out_ID1$age, stand_out_ID1$basal_area, type="l", col="darkgreen", ylim=c(0, max(Skogaby$BasalArea, na.rm=T)), ylab="Basal area", xlab="age", main = Skogaby[Skogaby$SiteID==i,]$Treatment[1], xlim=c(28,50))
Skogaby_data_time<-Skogaby[Skogaby$SiteID==i,]$Year-Skogaby[Skogaby$SiteID==i,]$Year[1]+stand_out_ID1$age[1]
points(Skogaby_data_time, Skogaby[Skogaby$SiteID==i,]$BasalArea, pch=16)

}
dev.off()


dim(stand_out_ID1)

ABG_variable_names<-i_output[i_output$group_id==4,]
ABG_out_ID1<-as.data.frame(my_out[[i]][,1,4,])
names(ABG_out_ID1)<-ABG_variable_names$variable_name



stocks_variable_names<-i_output[i_output$group_id==9,]
stocks_out_ID1<-as.data.frame(my_out[[i]][,1,9,])
names(stocks_out_ID1)<-stocks_variable_names$variable_name
plot(stocks_out_ID1$age, stocks_out_ID1$soilCfol, type="l", col="darkgreen",  ylab="Basal area", xlab="age")
Skogaby_data_time<-Skogaby[Skogaby$SiteID==i,]$Year-Skogaby[Skogaby$SiteID==i,]$Year[1]+stocks_out_ID1$age[1]

dim(stocks_out_ID1)
