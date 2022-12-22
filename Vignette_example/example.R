
#TODO
#Inizializzazione con i management standard in Svezia, con SI come input

# tmax branches 20% stem
# tmax roots 0.5
# tmax leaves 0.5
# tmax stem 50% DBH


#variables output array:
#Months, layer, group, variables

devtools::install_github("ilmenichetti/r3PG", subdir = "pkg", build_vignettes = T)

library(lubridate)

library(tidyverse)
library(readxl)
library(r3PG)
library(xlsx)




my_site<-read_excel("birch_scenario.xlsx", sheet="d_site")
my_species<-read_excel("birch_scenario.xlsx", sheet="d_species")
my_climate<-read_excel("birch_scenario.xlsx", sheet="d_climate")
my_thinning<-read_excel("birch_scenario.xlsx", sheet="d_thinning")
my_parameters<-read_excel("birch_scenario.xlsx", sheet="d_parameters")
my_sizeDist<-read_excel("birch_scenario.xlsx", sheet="d_sizeDist")
my_parsQlitter<-read_excel("birch_scenario.xlsx", sheet="d_parsQlitter")

my_soil<-read_excel("birch_scenario.xlsx", sheet="d_soil")[,-1]


example_species<-tibble(species=("Pinus sylvestris"), 
                     planted=("1955-01"),
                     fertility=(0.5),
                     stems_n=(2000),
                     biom_stem=(90),
                     biom_root=(10),
                     biom_foliage=(5))


example_thinning<-tibble(species=c("Pinus sylvestris","Pinus sylvestris"), 
                          age=c(50, 75),
                          stems_n=c(1500, 1000),
                          stem=c(1,1),
                          root=c(1,1),
                          foliage=c(1,1))

example_site<-tibble(latitude   =(47.6), 
                     altitude   =(100),
                     soil_class =(3),
                     asw_i      =(999),
                     asw_min    =(0),
                     asw_max    =(100),
                     from       =NA,
                     to         =NA)

C_init(lat=45,
       SI=23,
       site        = example_site,
       species     = example_species,
       climate     = my_climate,
       thinning    = example_thinning,
       parameters  = my_parameters,
       parsQlitter = my_parsQlitter,
       size_dist   = my_sizeDist,
       settings    = list(light_model = 2, transp_model = 2, phys_model = 2,
                          height_model = 1, correct_bias = 0, calculate_d13c = 0))



C_init(lat=45,
       SI=23,
       site        = my_site,
       climate     = my_climate,
       size_dist   = my_sizeDist,
       settings    = list(light_model = 2, transp_model = 2, phys_model = 2,
                          height_model = 1, correct_bias = 0, calculate_d13c = 0))

C_init(lat=45,
       SI=23,
       site        = my_site,
       species     = my_species,
       climate     = my_climate,
       thinning    = my_thinning,
       parameters  = my_parameters,
       size_dist   = my_sizeDist,
       settings    = list(light_model = 2, transp_model = 2, phys_model = 2,
                          height_model = 1, correct_bias = 0, calculate_d13c = 0))


C_init(lat=45,
       SI=23,
       species     = my_species,
       site        = my_site,
       climate     = my_climate,
       size_dist   = my_sizeDist,
       settings    = list(light_model = 2, transp_model = 2, phys_model = 2,
                          height_model = 1, correct_bias = 0, calculate_d13c = 0))


my_out = run_3PG(site        = my_site,
                  species     = my_species,
                  climate     = my_climate,
                  thinning    = my_thinning,
                  parameters  = my_parameters,
                  parsQlitter = my_parsQlitter,
                  soil        = my_soilInit,
                  size_dist   = my_sizeDist,
                  settings    = settings,
                  check_input = TRUE, df_out = F)



str(my_out)

plot(my_out[,1,9,6], type="l")
plot(my_out[,1,9,7], type="l")




#all months, species, stand variables, all
stand_variable_names<-i_output[i_output$group_id==2,]
stand_out_betula<-as.data.frame(my_out[,1,2,])
names(stand_out_betula)<-stand_variable_names$variable_name
plot(stand_out_betula$age, stand_out_betula$basal_area)

plot(stand_out_betula$age, stand_out_betula$stems_n)

stand_out_pine<-as.data.frame(my_out[,2,2,])
names(stand_out_pine)<-stand_variable_names$variable_name
plot(stand_out_pine$age, stand_out_pine$stems_n)





management_plan<-function(SI, Lat){
  
  ### Self-contained declaration of matrices for the management plans
  # this is in order to skip the reading of the specific csv and make the function more general
  
  N0<-c(1250,1250,1250,1250,1400,1600,1800,2000,2200,2300,2500,2700,2900,3000)
  
  gm<-c(141, 141, 141, 141, 126, 121, 111, 106, 101, 95, 85, 80, 75, 70)
  
  s1_vec<-c(0.75, 0.75, 0.75, 0.75, 0.72, 0.69, 0.71, 0.73, 0.76, 0.75, 0.72, 0.73, 0.76, 0.76)
  s2_vec<-c(0.71, 0.71, 0.71, 0.71, 0.74, 0.72, 0.73, 0.73, 0.73, 0.74, 0.73, 0.67, 0.71, 0.69)
  s3_vec<-c(1, 1, 1, 1, 1, 0.86, 0.84, 0.71, 0.74, 0.74, 0.76, 0.72, 0.74, 0.74)
  s4_vec<-c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0.75, 0.72, 0.76, 0.77, 0.74)
  s<-as.data.frame(cbind(s1_vec,s2_vec,s3_vec,s4_vec))
  
  g1_vec<-c(75, 75, 75, 75, 62, 55, 47, 37, 33, 30, 27, 25, 24, 21)
  g2_vec<-c(100, 100, 100, 100, 82, 76, 65, 53, 48, 43, 36, 35, 35, 30)
  g3_vec<-c(140, 140, 140, 140, 125, 91, 85, 71, 65, 55, 45, 46, 45, 39)
  g4_vec<-c(140, 140, 140, 140, 125, 120, 110, 105, 100, 70, 57, 59, 57, 47)
  g<-as.data.frame(cbind(g1_vec,g2_vec,g3_vec,g4_vec))
  
  H0<-c(12, 12, 12, 12, 12, 12, 12, 11, 11, 11, 11, 11, 11, 11)
  gm<-c(141, 141, 141, 141, 126, 121, 111, 106, 101, 95, 85, 80, 75, 70)
  
  
  #selection id
  ID<-((SI-10)/2)+1
  
  ### Select the values from the specific site
  
  #Year when start diameter at breast hight is measured
  T.=H0[ID]#was T
  
  
  #Year of final felling
  gm=gm[ID]
  #Thinnings
  g1=as.numeric(g[ID,1])
  g2=as.numeric(g[ID,2])
  g3=as.numeric(g[ID,3])
  g4=as.numeric(g[ID,4])
  s1=as.numeric(s[ID,1])
  s2=as.numeric(s[ID,2])
  s3=as.numeric(s[ID,3])
  s4=as.numeric(s[ID,4])
  
  N=N0[ID]
  
  thinnings<-data.frame(g1, g2, g3, g4, gm, s1, s2, s3, s4)
  results<-data.frame(H0=T., N, thinnings)
  
  return(results)
}

#management<-management_plan(12,45)




extend_time<-function(climate, from, length){
  
  require(lubridate)
  length<-length+1
  
  from_date<-as.Date(paste(from, "-01", sep=""), format = "%Y-%m-%d")
  seq_date<-seq(from_date, by="month", length.out=length*12)
  
  repetitions<-ceiling(length/(diff(range(climate$year))+1))
  climate_long<-do.call(rbind, replicate(repetitions, climate, simplify=FALSE)) # where m is your matrix
  climate_long_cut<-climate_long[1:(length*12),]
  climate_long_cut$year<-year(seq_date)
  climate_long_cut$month<-month(seq_date)
  return(climate_long_cut)
  
}


# lat=45
# SI=23
# site        = my_site
# species     = my_species
# climate     = my_climate
# thinning    = my_thinning
# parameters  = my_parameters
# parsQlitter = my_parsQlitter
# size_dist   = my_sizeDist
# settings    = list(light_model = 2, transp_model = 2, phys_model = 2,
#                    height_model = 1, correct_bias = 0, calculate_d13c = 0)


sim_thinning<-function(SI, lat, planted){


management<-management_plan(SI,lat)
thinning_boolean<-management[8:11]!=1
management[9]<-management[8]*management[9]
management[10]<-management[9]*management[10]
management[11]<-management[10]*management[11]


main_species<-tibble(species=("a"), 
       planted=("a"),
       fertility=(0.5),
       stems_n=(1),
       biom_stem=(90),
       biom_root=(10),
       biom_foliage=(5))
main_species$species<- "Picea abies"
main_species$stems_n<-management$N
main_species$planted<-planted
main_species$species<- "Picea abies"

thinning_template<-tibble(species=("a"), 
                     age=(0),
                     stems_n=(0),
                     stem=(1),
                     root=(1),
                     foliage=(1))

main_thinning<-thinning_template[0,]
for(i in 1:length(which(thinning_boolean))){
  if(thinning_boolean[i]==T){
    main_thinning_line<-thinning_template
    main_thinning_line$species<- "Picea abies"
    main_thinning_line$age=management[2+i]
    main_thinning_line$stems_n=round(management[7+i]*management$N)
    main_thinning_line$stem=1
    main_thinning_line$root=1
    main_thinning_line$foliage=1
    main_thinning<-rbind(main_thinning, main_thinning_line)
  }
}


#final felling
main_thinning_last_line<-thinning_template
main_thinning_last_line$species<- "Picea abies"
main_thinning_last_line$age<-management$gm
main_thinning_last_line$stems_n<-0 #no stems are left
main_thinning_last_line$stem=1
main_thinning_last_line$root=1
main_thinning_last_line$foliage=1
main_thinning<-rbind(main_thinning, main_thinning_last_line)


return(list(main_species=main_species, main_thinning=main_thinning))

}


sim_thinning(SI=23, lat=45)


C_init<-function(lat,
                 SI,
                 site,
                 species=NULL,
                 climate,
                 thinning=NULL,
                 parameters=NULL,
                 parsQlitter=NULL,
                 size_dist,
                 settings = list(light_model = 2, transp_model = 2, phys_model = 2,
                                 height_model = 1, correct_bias = 0, calculate_d13c = 0)){

  planted="1950-01"
    
  #some data check
  if(is.null(species)){warning("you did not supply a species matrix, assuming plantation date as 1950. This will not influence the final result")}
  if(is.null(thinning) & !is.null(species)){stop("you supplied a species matrix, you need to supply also the associated thinning matrix")}
  if(is.null(species) & !is.null(thinning)){stop("you supplied a thinning matrix, you need to supply also the associated species matrix")}
  if(i!s.null(thinning) & !is.null(species)){warning("you supplied a species matrix and the associated thinning matrix, these will be used in the initialization simulation. Please make sure that your thinning plan includes a whole cultural cycle")}
  
  
  if(is.null(species)){
    sim_management<-sim_thinning(SI, lat, planted)
    main_species<-sim_management$main_species
  } else {  
    main_species<-species[which.max(species$stems_n),]
    
    }
  
  if(is.null(thinning)){
    sim_management<-sim_thinning(SI=23, lat=45, planted)
    main_thinning<-sim_management$main_thinning
  } else {  
    main_thinning<-thinning
  }
  
  
  #selecting one dummy initialization value for simulating the first time series (to calculate the average inputs)
  soilInit<-c(3,3,3,3,0)
  
  #extending the cycle to one full length (management$gm+1 length, final felling)
  main_site<-site
  main_site$from<-main_species$planted
  end_cycle<-as.Date(paste(main_site$from, "-01", sep=""), format = "%Y-%m-%d") %m+% years(main_thinning[nrow(main_thinning),]$age+1)
  main_site$to<-format(end_cycle, "%Y-%m")

  #extending the climate time series
  climate_long<-extend_time(climate=climate,from=main_site$from, length=as.numeric(main_thinning[nrow(main_thinning),]$age+1))
  
  #if the Q parameters are null, then the initialization assumes spruce parameterization
  if(is.null(parsQlitter)){
  Qpars<- list(7.00, 0.36, 0.25, 0.50, 0.00, 1.10, 0.50, 7.00, 0.36, 
               0.25, 0.50, 0.00, 1.10, 0.50, 7.00, 0.36, 0.25, 0.50,
               0.00, 1.10, 0.50, 7.00, 0.36, 0.25, 0.50, 0.00, 1.10, 0.30)
  names(Qpars)<-c("beta_fol",    "eta_11_fol",  "e0_fol",      "fc_fol",      "delay_fol",   "q0_fol",     
                  "z_leaves",    "beta_root",   "eta_11_root", "e0_root",     "fc_root",     "delay_root", 
                  "q0_root",     "z_root",      "beta_bran",   "eta_11_bran", "e0_bran",     "fc_bran",    
                  "delay_bran",  "q0_bran",     "z_branch",    "beta_stem",   "eta_11_stem", "e0_stem",    
                  "fc_stem",     "delay_stem",  "q0_stem",     "z_stem"    )
  
  main_parsQlitter<-data.frame(parameter=names(Qpars), "Picea abies"=as.vector(unlist(Qpars)))
  colnames(main_parsQlitter)[2]="Picea abies"
  } else {
    main_parsQlitter<-data.frame( parsQlitter[,1], parsQlitter[,colnames(parsQlitter)==main_species$species])
  colnames(main_parsQlitter)[2]=main_species$species
  
  Qpars<-as.list(unlist(parsQlitter[,colnames(parsQlitter)==main_species$species]))
  names(Qpars)<-c("beta_fol",    "eta_11_fol",  "e0_fol",      "fc_fol",      "delay_fol",   "q0_fol",     
                  "z_leaves",    "beta_root",   "eta_11_root", "e0_root",     "fc_root",     "delay_root", 
                  "q0_root",     "z_root",      "beta_bran",   "eta_11_bran", "e0_bran",     "fc_bran",    
                  "delay_bran",  "q0_bran",     "z_branch",    "beta_stem",   "eta_11_stem", "e0_stem",    
                  "fc_stem",     "delay_stem",  "q0_stem",     "z_stem"    )
  
  }
  
  
  
  #if the 3PG parameters are null, then the initialization assumes spruce parameterization
  if(is.null(parameters)){
    pars<- list(0.829700,   0.148000,   0.133000,   2.305000,   0.715100,   0.082400,   0.000800,   0.001000,
                60.000000,  0.000000,   0.000000,   0.000000,   2.472700,  24.949400,  30.137800,   1.000000,
                0.700000,   9.000000,   1.004000,   0.958400,   0.000000,   0.600000,   1.000000, 400.000000,
                4.000000,   0.950000,   0.000000,   0.000000,   0.000000,   1.000000, 376.084300,   1.874000,
                0.488000,   0.436000,   0.437000,   8.710000,   3.850000,  25.100000,   0.637800,   3.000000,
                0.223700,   3.000000,   5.000000,   0.027000,   0.470000,   0.000000,   0.024600,   3.330000,
                0.089600,   0.200000,   0.660000,   2.000000,   4.400000,  27.000000,   0.000000,   0.000000,
                0.000000,   0.400000,   0.400000,   1.000000,   3.000000,  37.730000,  17.850000,   0.006360,
                0.000115,   2.310000,   0.330000,   0.000000,   0.630000,   0.640000,   0.000000,  -0.069000,
                0.067000,  35.180000,  27.180000,   0.000000,  -0.005000,   0.000000, -90.000000,   0.800000,
                24.000000,   2.300000)
    names(pars)<-c("pFS2" ,         "pFS20",         "aWS",           "nWS",           "pRx",          
                   "pRn",           "gammaF1",       "gammaF0",       "tgammaF",       "gammaR",       
                   "leafgrow",      "leaffall",      "Tmin",          "Topt",          "Tmax",         
                   "kF",            "SWconst",       "SWpower",       "fCalpha700",    "fCg700",       
                   "m0",            "fN0",           "fNn",           "MaxAge",        "nAge",         
                   "rAge",          "gammaN1",       "gammaN0",       "tgammaN",       "ngammaN",      
                   "wSx1000",       "thinPower",     "mF",            "mR",            "mS",           
                   "SLA0",          "SLA1",          "tSLA",          "k",             "fullCanAge",   
                   "MaxIntcptn",    "LAImaxIntcptn", "cVPD",          "alphaCx",       "Y",            
                   "MinCond",       "MaxCond",       "LAIgcx",        "CoeffCond",     "BLcond",       
                   "RGcGw",         "D13CTissueDif", "aFracDiffu",    "bFracRubi",     "fracBB0",      
                   "fracBB1",       "tBB",           "rhoMin",        "rhoMax",        "tRho",         
                   "crownshape",    "aH",            "nHB",           "nHC",           "aV",           
                   "nVB",           "nVH",           "nVBH",          "aK",            "nKB",          
                   "nKH",           "nKC",           "nKrh",          "aHL",           "nHLB",         
                   "nHLL",          "nHLC",          "nHLrh",         "Qa",            "Qb",           
                   "gDM_mol",       "molPAR_MJ"    )
    
    my_parameters<-data.frame(parameter=names(pars), "Picea abies"=as.vector(unlist(pars)))
    colnames(my_parameters)[2]="Picea abies"
    parameters=my_parameters
  }
  

  
  init_out = run_3PG(
    site        = main_site,
    species     = main_species,
    climate     = climate_long,
    thinning    = main_thinning,
    parameters  = parameters,
    parsQlitter = main_parsQlitter,
    soil        = soilInit,
    size_dist   = size_dist,
    settings    = settings,
    check_input = TRUE, 
    df_out = F)
  

  
  soilInit_out<-as.data.frame(mat.or.vec(1, 5))
  soilInit_out[1]<-Q_ss(b=as.numeric(Qpars$beta_fol),
                            e0=as.numeric(Qpars$e0_fol),
                            q0=as.numeric(Qpars$q0_fol),
                            u0=u0_calc(lat),
                            fc=as.numeric(Qpars$fc_fol),
                            eta11=as.numeric(Qpars$eta_11_fol),
                            Tmax=3,
                            inputs=mean(init_out[,1,4,7], na.rm = T))
    
    soilInit_out[2]<-Q_ss(b=as.numeric(Qpars$beta_root),
                            e0=as.numeric(Qpars$e0_root),
                            q0=as.numeric(Qpars$q0_root),
                            u0=u0_calc(lat),
                            fc=as.numeric(Qpars$fc_root),
                            eta11=as.numeric(Qpars$eta_11_root),
                            Tmax=3,
                            inputs=mean(init_out[,1,4,8], na.rm = T))
    
    soilInit_out[3]<-Q_ss(b=as.numeric(Qpars$beta_stem),
                            e0=as.numeric(Qpars$e0_stem),
                            q0=as.numeric(Qpars$q0_stem),
                            u0=u0_calc(lat),
                            fc=as.numeric(Qpars$fc_stem),
                            eta11=as.numeric(Qpars$eta_11_stem),
                            Tmax=mean(init_out[,1,2,5], na.rm=T),
                            inputs=mean(init_out[,1,4,12], na.rm = T))
    
    soilInit_out[4]<-Q_ss(b=as.numeric(Qpars$beta_bran),
                            e0=as.numeric(Qpars$e0_bran),
                            q0=as.numeric(Qpars$q0_bran),
                            u0=u0_calc(lat),
                            fc=as.numeric(Qpars$fc_bran),
                            eta11=as.numeric(Qpars$eta_11_bran),
                            Tmax=3,
                            inputs=mean(init_out[,1,4,13], na.rm = T))
    
    soilInit_out[5]<-0
    
  
  if(is.null(species)){
    return(soilInit_out)
  } else {
    
  #calculating the proportions to distribute the initialization
  species_proportions<-species$stems_n/sum(species$stems_n)
  species_soilInit_out<-mat.or.vec(dim(species)[1], 5)
  
  for(i in 1:dim(species_soilInit_out)[1]){
    species_soilInit_out[i,]<-as.numeric(species_proportions[i]*soilInit_out)
  }
  return(species_soilInit_out)
  }
    
}





Q_ss= function(fc, inputs, e0, eta11, b, u0, q0, Tmax){
  
  I=inputs
  a = fc*b*eta11*u0*q0^b
  z = (1-e0)/(b*eta11*e0) #zeta is not variable over time, it is edaphic
  
  C_ss=(1/a*1/(z-1)+Tmax/3)*I
  return(C_ss)
}



u0_calc<-function(lat){
  u0_calc=(0.0855+0.0157*(50.6-0.768*lat))
  return(u0_calc)
}

