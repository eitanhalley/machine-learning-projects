library(rlang)
library(MASS)
library(fitdistrplus)
library(magrittr)
library(dplyr)
library(lazyeval)
library(parallel)
library(e1071)
library(plotly)
library(ggplot2)
library(triangle)
library(sqldf)
#library(readxl)
# library(knitr)
# library(rmarkdown)
library(simmer)
library(simmer.plot)
library(epiDisplay)
### you must install every package under this
library(mice)
library(ggmice)
library(tidyverse)
library(Ecdat)
library(stringr)
library(readr)
library(dplyr)

############################################################################

filePath=choose.files() 
DataTable<-read.csv(filePath,header=TRUE) 

clean_DataTable <- DataTable

clean_DataTable$X <- NULL# replica of Unnamed..0 
clean_DataTable[clean_DataTable == "yes"] <- as.numeric(1) 
clean_DataTable[clean_DataTable == "no"] <- as.numeric(0)
clean_DataTable$isNewBuilt <-  as.numeric(as.character(clean_DataTable$isNewBuilt))
clean_DataTable$hasStormProtector <-  as.numeric(as.character(clean_DataTable$hasStormProtector))
clean_DataTable$hasGuestRoom <-  as.numeric(as.character(clean_DataTable$hasGuestRoom))



#---------------------switch null values, part of the values need to be full numbers -----------------
clean_DataTable$numberOfRooms[is.na(clean_DataTable$numberOfRooms)]<-as.integer(mean(clean_DataTable$numberOfRooms,na.rm = TRUE))

prob_hasYard <- runif(1,0,1)

clean_DataTable$hasYard[is.na(clean_DataTable$hasYard)]<-
  if(mean(clean_DataTable$hasYard,na.rm = TRUE) <= prob_hasYard){
    1
  }else{
    0
  }

prob_Pool <- runif(1,0,1)


clean_DataTable$hasPool[is.na(clean_DataTable$hasPool)]<-
  if(mean(clean_DataTable$hasPool,na.rm = TRUE) <= prob_Pool){
    1
  }else{
    0
    }

clean_DataTable$floors[is.na(clean_DataTable$floors)]<-as.integer(mean(clean_DataTable$floors,na.rm = TRUE))

clean_DataTable$cityCode[is.na(clean_DataTable$cityCode)]<-as.integer(mean(clean_DataTable$cityCode,na.rm = TRUE))

clean_DataTable$cityPartRange[is.na(clean_DataTable$cityPartRange)]<-as.integer(mean(clean_DataTable$cityPartRange,na.rm = TRUE))

clean_DataTable$numPrevOwners[is.na(clean_DataTable$numPrevOwners)]<-as.integer(mean(clean_DataTable$numPrevOwners,na.rm = TRUE))

clean_DataTable$made[is.na(clean_DataTable$made)]<-as.integer(mean(clean_DataTable$made,na.rm = TRUE))


prob_isNewBuilt <- runif(1,0,1)

clean_DataTable$isNewBuilt[is.na(clean_DataTable$isNewBuilt)]<-
  if(mean(clean_DataTable$isNewBuilt,na.rm = TRUE) <= prob_isNewBuilt){
    1
  }else{
    0
    }

prob_hasStormProtector <- runif(1,0,1)

clean_DataTable$hasStormProtector[is.na(clean_DataTable$hasStormProtector)]<-
  if(mean(clean_DataTable$hasStormProtector,na.rm = TRUE) <= prob_hasStormProtector){
    1
  }else{
    0
    }


clean_DataTable$basement[is.na(clean_DataTable$basement)]<-mean(clean_DataTable$basement,na.rm = TRUE)

clean_DataTable$attic[is.na(clean_DataTable$attic)]<-mean(clean_DataTable$attic,na.rm = TRUE)

clean_DataTable$garage[is.na(clean_DataTable$garage)]<-mean(clean_DataTable$garage,na.rm = TRUE)

prob_hasStorageRoom <- runif(1,0,1)

clean_DataTable$hasStorageRoom[is.na(clean_DataTable$hasStorageRoom)]<-
  if(mean(clean_DataTable$hasStorageRoom,na.rm = TRUE) <= prob_hasStorageRoom){
    1
  }else{
    0
    }

clean_DataTable$price[is.na(clean_DataTable$price)]<-mean(clean_DataTable$price,na.rm = TRUE)

clean_DataTable$squareMeters[is.na(clean_DataTable$squareMeters)]<-mean(clean_DataTable$squareMeters,na.rm = TRUE)


#get new model in excel
#install.packages("openxlsx")
#library(openxlsx)
#write.xlsx(clean_DataTable,'X_test_new.xlsx',colNames = TRUE)
#Enter path before 'x_test_new.csv'
write.csv(clean_DataTable, 'X_test_new.csv', col.names=TRUE)


