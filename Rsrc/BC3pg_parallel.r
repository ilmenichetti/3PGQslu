library(r3PG)
library(dplyr)
library(ggplot2)
library(multidplyr)
library(tidyr)
library(purrr)
library(BayesianTools)
library(sensitivity)
library(data.table)
library(parallel)

load('myData/input3pgLists.rdata')
sites <- c(1:26,28:57)#length(allInputs$site)
pErr <- c(0.1,0.001)
nCores = 1
climate <- climate[1:1000,]
# # parameters for calibration and their ranges
# param_morris.df <- bind_rows(param_solling, error_solling) %>% filter(!is.na(min))
# par_cal_names <- param_morris.df$param_name
# par_cal_min <- param_morris.df$min
# par_cal_max <- param_morris.df$max
# par_cal_best <- param_morris.df$default
# 
# par_def.df <- select(param_solling, parameter = param_name, piab = default)
# 
# param.draw <- getSample(mcmc_out, numSamples = 500, coda = F, whichParameters = 1:20) %>%
#   as.data.frame() %>%
#   mutate(mcmc_id = 1:n()) %>%
#   nest_legacy(-mcmc_id, .key = 'pars')   %>%
#   mutate( 
#     pars = map(pars, unlist),
#     pars = map(pars, ~tibble::enframe(.x, 'parameter', 'piab')),
#     pars = map(pars, ~bind_rows(.x, filter(par_def.df, !parameter %in% par_cal_names))))

###functions

# Heavy tailed noraml distribution
Sivia_log<-function(diff,sd){
  # sd[which(sd<=0)]<-11e-6e-6
  diff[which(abs(diff)<=1e-6)]<-1e-6
  R2<-(diff/sd)^2
  prob<-1/(sd*(pi*2)^0.5)*(1-exp(-R2/2))/R2
  log(prob)
}
calc_ll <- function(dataX,varNameX,pErr){
  llX <- 0
  if(nrow(dataX[var_name==varNameX])>0){
    subData <- dataX[var_name==varNameX]
    resX <- as.numeric(subData$sims) - as.numeric(subData$obs)
    sdX <- pErr[1] + pErr[2] * as.numeric(subData$sims)
    llX <- sum(Sivia_log(resX,sdX))
  }
  return(llX)
}

##test
# i=4
# site <- allInputs$site[[i]]
# species <- allInputs$species[[i]]
# thinning <- allInputs$thinning[[i]]
# obsData <- allInputs$obsData[[i]]
# pErr <- c(0.1,0.001)
# out <- run_3PG(site = site, species = species, climate = climate,
#                thinning = thinning, parameters = parameters,
#                settings=settings_3pg, check_input = TRUE,
#                df_out = F)

# dataX <- obsData
# #remove NAs
# naObs <- which(is.na(as.numeric(dataX$obs)))
# if(length(naObs>1)) dataX <- dataX[-naObs]
# dataX$sims <- out[as.matrix(dataX[,3:6])]
# 
# ll_stem <- calc_ll(dataX,"stems_n",pErr)
# ll_ba <- calc_ll(dataX,"basal_area",pErr)
# ll_v <- calc_ll(dataX,"volume",pErr)
# ll_d <- calc_ll(dataX,"dbh",pErr)
# ll_h <- calc_ll(dataX,"height",pErr)
# ll_Wstem <- calc_ll(dataX,"biom_stem",pErr)
# ll_Wroot <- calc_ll(dataX,"biom_root",pErr)
# ll_Wfol <- calc_ll(dataX,"biom_foliage",pErr)
# 
# ll <- ll_stem + ll_ba + ll_v + ll_d + ll_h + ll_Wstem + ll_Wroot + ll_Wfol




#' Title
#'
#' @param site 
#' @param species 
#' @param thinning 
#' @param climate #weather inputs
#' @param obsData 
#' @param parameters 
#' 
#' @param outType 
#' modOut returns model output;
#' ll returns the log likelihood
#' datX returns the database of observed and simulated 
#'
#' @return
#' @export
#'
#' @examples
multi_r3pg <- function(site, species, thinning,climate, obsData,parameters,outType="ll"){
  #' @description simulate the n runs for a given site with the drawn parameter combination
  
  
  #' r3pg_int <- function( par_df){
  #'   #' @description function to run one site and return required output on standing biomass
  #'   #' @param par_df a data.frame of parameters
    
  out <- run_3PG(site = site, species = species, climate = climate,
                 thinning = thinning, parameters = parameters,
                 settings=settings_3pg, check_input = TRUE,
                 df_out = F)
  
  dataX <- obsData
  #remove NAs
  naObs <- which(is.na(as.numeric(dataX$obs)))
  if(length(naObs>1)) dataX <- dataX[-naObs]
  dataX$sims <- out[as.matrix(dataX[,3:6])]
  
  ll_stem <- calc_ll(dataX,"stems_n",pErr)
  ll_ba <- calc_ll(dataX,"basal_area",pErr)
  ll_v <- calc_ll(dataX,"volume",pErr)
  ll_d <- calc_ll(dataX,"dbh",pErr)
  ll_h <- calc_ll(dataX,"height",pErr)
  ll_Wstem <- calc_ll(dataX,"biom_stem",pErr)
  ll_Wroot <- calc_ll(dataX,"biom_root",pErr)
  ll_Wfol <- calc_ll(dataX,"biom_foliage",pErr)
  
  ll <- ll_stem + ll_ba + ll_v + ll_d + ll_h + ll_Wstem + ll_Wroot + ll_Wfol
  if(outType=="ll") return(ll)
  if(outType=="modOut") return(out)
  if(outType=="datX") return(dataX)
}

####runModel for multiple sites and report likelihood
ll <- numeric(nSites)
ll <-mclapply(sites, function(i,ll){
  print(i)
  ll[i] <- multi_r3pg(allInputs$site[[i]],allInputs$species[[i]],
                     allInputs$thinning[[i]],allInputs$climate[[i]],
                     allInputs$obsData[[i]],parameters)
},ll=ll,mc.cores = nCores)


####runModel for multiple sites and return a list of databases for each site with the simulated ond observed data
datX <- list()
datX <-mclapply(sites, function(i,datX){
  print(i)
  datX[[i]] <- multi_r3pg(allInputs$site[[i]],allInputs$species[[i]],
                      allInputs$thinning[[i]],allInputs$climate[[i]],
                      allInputs$obsData[[i]],parameters,outType="datX")
},datX=datX,mc.cores = nCores)


###likelihood 
logLike <- function(pX){
  
  # print("here")
  parameters$`Pinus sylvestris`[pIds] <- pX
  ll <- numeric(nSites)
  ll <-mclapply(sites, function(i,ll){
    # print(i)
    ll[i] <- multi_r3pg(allInputs$site[[i]],allInputs$species[[i]],
                        allInputs$thinning[[i]],allInputs$climate[[i]],
                        allInputs$obsData[[i]],parameters)
  },ll=ll,mc.cores = nCores)
  loglike <- sum(unlist(ll))
  return(loglike)
}

# pIds <- 1:20
# ptest1 <- parameters$`Pinus sylvestris`[pIds]
# ptest2 <- parameters$`Pinus sylvestris`[pIds] * 1.2
# ptest3 <- parameters$`Pinus sylvestris`[pIds] * 0.8

# logLike(ptest1)
# logLike(ptest2)
# logLike(ptest3)

# calibration
### Define the prior
pIds <- 1:20
par <- list()
par$best <- parameters$`Pinus sylvestris`[pIds]
par$min <- parameters$`Pinus sylvestris`[pIds] * 0.8
par$max <- parameters$`Pinus sylvestris`[pIds] * 1.2
par$names <- parameters$parameter[pIds]

pY <- which(par$best<0)
par$min[pY] <- par$best[pY] * 1.2
par$max[pY] <- par$best[pY] * 0.8
pY <- which(par$best==0)
par$min[pY] <- par$best[pY] -0.1
par$max[pY] <- par$best[pY] + 0.1 

prior <- createUniformPrior(lower = par$min, upper = par$max)
### Create Bayesian Setup
BCmod <- createBayesianSetup(logLike, prior, best = par$best, 
                                names = par$names)
## Running MCMC
iterations=3e3
thin = 1 #100
nChains <- 3
settings <- list(iterations = iterations, nrChains = nChains,thin=thin)

calibration <- runMCMC(BCmod, sampler="DREAMzs", settings = settings)

save(calibration, file="calOut/calibration.rdata")
