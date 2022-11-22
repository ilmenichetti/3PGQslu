library(readxl)
library(stringr)
library(lubridate)


# data sanity check:
# 1) sometimes the single spreadsheets start with the data at a different row nr, breaking the loop. The loop skips the first 4 rowns, at row 5 there must be the header
# 2) the last date reports always the hour in the timestamp, this breaks the loop. Correct manually.


target<-as.data.frame(read_xlsx("Target.xlsx", sheet=1))
colnames(target)

latlong<-read_xlsx("EKSHÄRAD.xlsx", sheet=1) #extracting treatment name from the first cell

site<-"EKSHÄRAD"



treat<-list()


# parsing


#loop for treatments (sheetsS)

shets_nr<-length(excel_sheets("EKSHÄRAD.xlsx"))


for(j in 2:(shets_nr)){ # loop j per ogni parcella (foglio excel)
  
treatment<-str_sub(names(read_xlsx("EKSHÄRAD.xlsx", sheet=j)[1,1]), -1) #extracting treatment name from the first cell
treat_table<-as.data.frame(read_xlsx("EKSHÄRAD.xlsx", sheet=j, skip=4)) #the data we are going to work in this jth step

  
## subsetting the revisioins
#treat_table[,1][treat_table[,1]=="."]<-NA

revisions_pos<-which(!is.na(treat_table[,1]) & !treat_table[,1]==".")
revisions_pos_end<-which(treat_table[,1]==".")
revisions_N<-length(revisions_pos) #how many revisions?

#loop for revisions
for(i in 1:revisions_N){
  
  #subset_total<-treat_table[revisions_pos[i]:(revisions_pos[i+1]-1),]
  subset_total<-treat_table[revisions_pos[i]:(revisions_pos_end[i]-1),]
  
  if(i == 1){
  species<-subset_total[2,]$Trädslag}
  
  #subsetting only the species planted in the beginning
  #subset_total[2:dim(subset_total)[1],]$Trädslag
  select_species<-subset_total[2:dim(subset_total)[1],]$Trädslag==species & !is.na(subset_total[2:dim(subset_total)[1],]$Trädslag)
  subset_species<-subset_total[2:dim(subset_total)[1],][select_species,]
  
  
  #general value for the revision
  revision_date<-as.Date(str_sub(subset_total[1,6],-10, -1))
  age<-as.numeric(gsub("Ålder ", "", subset_total[1,3]))

  
  target_22_27<-mat.or.vec(1,6)
  target_28_33<-mat.or.vec(1,6)
  target_34_39<-mat.or.vec(1,6)
  
  #loop for the different stocks data
  for (k in 1:dim(subset_species)[1]){
    


    if(is.na(subset_species[k,2])){ #remaining stocks
      target_22_27<-subset_species[k,5:10]
     }
 
    if(is.na(subset_species[k,2])){ #thinned stocks
      target_28_33<-subset_species[k,12:17]
    }
    
    
    if(!is.na(subset_species[k,2]) && subset_species[k,2]=="TORR"){ #dead stocks
      target_34_39<-subset_species[k,12:17]
    }
      
  } #closing the k loop, each row of the revision i, only one tree species
  
  datarow<-c(site, treatment, i, NA, year(revision_date), month(revision_date), day(revision_date), age,
             latlong[1,2], latlong[2,2], latlong[3,2], #lat
             latlong[1,4], latlong[2,5], latlong[3,6],#long,
             latlong[3,7], #altitude,
             NA, #origin,
             species,#tree species
             target_22_27,
             target_28_33,
             target_34_39)
  names(datarow)<-names(target)
  target<-rbind(target,datarow)
  
  
} # closing the i loop, revision



} #closing the j loop, sheet number






## writing the output file 

#read the template

#write







