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
library(stringer)
##install.packages("stringr")

############################################################################

filePath=choose.files() 
DataTable<-read.csv(filePath,header=TRUE) 


#--------hist---------------------
hist(DataTable$squareMeters,main ="House size",axes = TRUE,xlab = "House squareMeters" ,col = "red", labels = TRUE )
hist(DataTable$basement,main ="Basement size",axes = TRUE,xlab = "Basement squareMeters" ,col = "red", labels = TRUE )
hist(DataTable$attic,main ="Attic size",axes = TRUE,xlab = "Attic squareMeters" ,col = "red", labels = TRUE )
hist(DataTable$garage,main ="Garage size",axes = TRUE,xlab = "Garage squareMeters" ,col = "red", labels = TRUE )
hist(DataTable$price,main ="House price",axes = TRUE,xlab = "price of house" ,col = "red", labels = TRUE )
hist(DataTable$cityCode,main ="City code",axes = TRUE,xlab = "zip code" ,col = "red", labels = TRUE )

#--------probability---------------------
#num of rooms
DataSetNumRoom0_20<-subset(DataTable,DataTable$numberOfRooms>=0 & DataTable$numberOfRooms<=20)
precentNumRoom0_20<-count(DataSetNumRoom0_20)/count(DataTable)
DataSetNumRoom21_40<-subset(DataTable,DataTable$numberOfRooms>20 & DataTable$numberOfRooms<=40)
precentNumRoom21_40<-count(DataSetNumRoom21_40)/count(DataTable)
DataSetNumRoom41_60<-subset(DataTable,DataTable$numberOfRooms>40 & DataTable$numberOfRooms<=60)
precentNumRoom41_60<-count(DataSetNumRoom41_60)/count(DataTable)
DataSetNumRoom61_80<-subset(DataTable,DataTable$numberOfRooms>60 & DataTable$numberOfRooms<=80)
precentNumRoom61_80<-count(DataSetNumRoom61_80)/count(DataTable)
DataSetNumRoom81_100<-subset(DataTable,DataTable$numberOfRooms>80 & DataTable$numberOfRooms<=100)
precentNumRoom81_100<-count(DataSetNumRoom81_100)/count(DataTable)

#has yard
DataSetYard0<-subset(DataTable,DataTable$hasYard==0)
precentYard0<-count(DataSetYard0)/count(DataTable)
DataSetYard1<-subset(DataTable,DataTable$hasYard==1)
precentYard1<-count(DataSetYard1)/count(DataTable)

#has pool
DataSetPool0<-subset(DataTable,DataTable$hasPool==0)
precentPool0<-count(DataSetPool0)/count(DataTable)
DataSetPool1<-subset(DataTable,DataTable$hasPool==1)
precentPool1<-count(DataSetPool1)/count(DataTable)

#num of floors
DataSetNumFllors0_20<-subset(DataTable,DataTable$floors>=0 & DataTable$floors<=20)
precentNumFllors0_20<-count(DataSetNumFllors0_20)/count(DataTable)
DataSetNumFllors21_40<-subset(DataTable,DataTable$floors>20 & DataTable$floors<=40)
precentNumFllors21_40<-count(DataSetNumFllors21_40)/count(DataTable)
DataSetNumFllors41_60<-subset(DataTable,DataTable$floors>40 & DataTable$floors<=60)
precentNumFllors41_60<-count(DataSetNumFllors41_60)/count(DataTable)
DataSetNumFllors61_80<-subset(DataTable,DataTable$floors>60 & DataTable$floors<=80)
precentNumFllors61_80<-count(DataSetNumFllors61_80)/count(DataTable)
DataSetNumFllors81_100<-subset(DataTable,DataTable$floors>80 & DataTable$floors<=100)
precentNumFllors81_100<-count(DataSetNumFllors81_100)/count(DataTable)

#city part range
DataSetPartRange1_2<-subset(DataTable,DataTable$cityPartRange==1 | DataTable$cityPartRange==2)
precentPartRange1_2<-count(DataSetPartRange1_2)/count(DataTable)
DataSetPartRange3_4<-subset(DataTable,DataTable$cityPartRange==3 | DataTable$cityPartRange==4)
precentPartRange3_4<-count(DataSetPartRange3_4)/count(DataTable)
DataSetPartRange5_6<-subset(DataTable,DataTable$cityPartRange==5 | DataTable$cityPartRange==6)
precentPartRange5_6<-count(DataSetPartRange5_6)/count(DataTable)
DataSetPartRange7_8<-subset(DataTable,DataTable$cityPartRange==7 | DataTable$cityPartRange==8)
precentPartRange7_8<-count(DataSetPartRange7_8)/count(DataTable)
DataSetPartRange9_10<-subset(DataTable,DataTable$cityPartRange==9 | DataTable$cityPartRange==10)
precentPartRange9_10<-count(DataSetPartRange9_10)/count(DataTable)

#num of prevOwenrs
DataSetOwenrs1_2<-subset(DataTable,DataTable$numPrevOwners==1 | DataTable$numPrevOwners==2)
precentOwenrs1_2<-count(DataSetOwenrs1_2)/count(DataTable)
DataSetOwenrs3_4<-subset(DataTable,DataTable$numPrevOwners==3 | DataTable$numPrevOwners==4)
precentOwenrs3_4<-count(DataSetOwenrs3_4)/count(DataTable)
DataSetOwenrs5_6<-subset(DataTable,DataTable$numPrevOwners==5 | DataTable$numPrevOwners==6)
precentOwenrs5_6<-count(DataSetOwenrs5_6)/count(DataTable)
DataSetOwenrs7_8<-subset(DataTable,DataTable$numPrevOwners==7 | DataTable$numPrevOwners==8)
precentOwenrs7_8<-count(DataSetOwenrs7_8)/count(DataTable)
DataSetOwenrs9_10<-subset(DataTable,DataTable$numPrevOwners==9 | DataTable$numPrevOwners==10)
precentOwenrs9_10<-count(DataSetOwenrs9_10)/count(DataTable)

#made year
DataSetYear1900_1925<-subset(DataTable,DataTable$made>=1900 & DataTable$made<=1925)
precentYear1900_1925<-count(DataSetYear1900_1925)/count(DataTable)
DataSetYear1926_1950<-subset(DataTable,DataTable$made>1925 & DataTable$made<=1950)
precentYear1926_1950<-count(DataSetYear1926_1950)/count(DataTable)
DataSetYear1951_1975<-subset(DataTable,DataTable$made>1950 & DataTable$made<=1975)
precentYear1951_1975<-count(DataSetYear1951_1975)/count(DataTable)
DataSetYear1976_2000<-subset(DataTable,DataTable$made>1975 & DataTable$made<=2000)
precentYear1976_2000<-count(DataSetYear1976_2000)/count(DataTable)
DataSetYear2000_2022<-subset(DataTable,DataTable$made>2000 & DataTable$made<2022 )
precentYear2000_2022<-count(DataSetYear2000_2022)/count(DataTable)

#new built
DataSetNewBuilt0<-subset(DataTable,DataTable$isNewBuilt==0)
precentNewBuilt0<-count(DataSetNewBuilt0)/count(DataTable)
DataSetNewBuilt1<-subset(DataTable,DataTable$isNewBuilt==1)
precentNewBuilt1<-count(DataSetNewBuilt1)/count(DataTable)

#Storm protection
DataSetStorm0<-subset(DataTable,DataTable$hasStormProtector==0)
precentStorm0<-count(DataSetStorm0)/count(DataTable)
DataSetStorm1<-subset(DataTable,DataTable$hasStormProtector==1)
precentStorm1<-count(DataSetStorm1)/count(DataTable)

#Storage room
DataSetStorage0<-subset(DataTable,DataTable$hasStorageRoom==0)
precentStorage0<-count(DataSetStorage0)/count(DataTable)
DataSetStorage1<-subset(DataTable,DataTable$hasStorageRoom==1)
precentStorage1<-count(DataSetStorage1)/count(DataTable)

#num Guest room
DataSetGuest1_2<-subset(DataTable, DataTable$hasGuestRoom==2 |  DataTable$hasGuestRoom==1 )
precentGuest1_2<-count(DataSetGuest1_2)/count(DataTable)
DataSetGuest3_4<-subset(DataTable,DataTable$hasGuestRoom==3 | DataTable$hasGuestRoom==4)
precentGuest3_4<-count(DataSetGuest3_4)/count(DataTable)
DataSetGuest5_6<-subset(DataTable,DataTable$hasGuestRoom==5 | DataTable$hasGuestRoom==6)
precentGuest5_6<-count(DataSetGuest5_6)/count(DataTable)
DataSetGuest7_8<-subset(DataTable,DataTable$hasGuestRoom==7 | DataTable$hasGuestRoom==8)
precentGuest7_8<-count(DataSetGuest7_8)/count(DataTable)
DataSetGuest9_10<-subset(DataTable,DataTable$hasGuestRoom==9 | DataTable$hasGuestRoom==10)
precentGuest9_10<-count(DataSetGuest9_10)/count(DataTable)

#category
DataSetBasic<-subset(DataTable,DataTable$category=='Basic')
precentBasic<-count(DataSetBasic)/count(DataTable)
DataSetLuxury<-subset(DataTable,DataTable$category=='Luxury')
precentLuxury<-count(DataSetLuxury)/count(DataTable)

#---------------------clean unwanted data -----------------

clean_DataTable <- DataTable
clean_DataTable$X <- NULL# replica of Unnamed..0 
clean_DataTable[clean_DataTable == "yes"] <- as.numeric(1) 
clean_DataTable[clean_DataTable == "no"] <- as.numeric(0)
clean_DataTable[clean_DataTable == -3] <- NA 
clean_DataTable[clean_DataTable == -2256352.0] <- NA
clean_DataTable[clean_DataTable == ""] <- NA
clean_DataTable[clean_DataTable == 2122] <- NA
clean_DataTable[clean_DataTable == "Luxury"] <-as.numeric(1) 
clean_DataTable[clean_DataTable == "Basic"] <- as.numeric(0) 
clean_DataTable$isNewBuilt <-  as.numeric(as.character(clean_DataTable$isNewBuilt))
clean_DataTable$hasStormProtector <-  as.numeric(as.character(clean_DataTable$hasStormProtector))
clean_DataTable$hasGuestRoom <-  as.numeric(as.character(clean_DataTable$hasGuestRoom))
clean_DataTable$category <-  as.numeric(as.character(clean_DataTable$category))

str(clean_DataTable)#check perimeters tip 
#---------------------switch null values, part of the values need to be full numbers -----------------
clean_DataTable$numberOfRooms[is.na(clean_DataTable$numberOfRooms)]<-as.integer(mean(clean_DataTable$numberOfRooms,na.rm = TRUE))

clean_DataTable$hasYard[is.na(clean_DataTable$hasYard)]<-
  if(mean(clean_DataTable$hasYard,na.rm = TRUE) > 0.5){
    ceiling(mean(clean_DataTable$hasYard,na.rm = TRUE))
  }else{
  floor(mean(clean_DataTable$hasYard,na.rm = TRUE))
  }


clean_DataTable$hasPool[is.na(clean_DataTable$hasPool)]<-
  if(mean(clean_DataTable$hasPool,na.rm = TRUE) > 0.5){
  ceiling(mean(clean_DataTable$hasPool,na.rm = TRUE))
}else{
  floor(mean(clean_DataTable$hasPool,na.rm = TRUE))}

clean_DataTable$floors[is.na(clean_DataTable$floors)]<-as.integer(mean(clean_DataTable$floors,na.rm = TRUE))

clean_DataTable$cityCode[is.na(clean_DataTable$cityCode)]<-as.integer(mean(clean_DataTable$cityCode,na.rm = TRUE))

clean_DataTable$cityPartRange[is.na(clean_DataTable$cityPartRange)]<-as.integer(mean(clean_DataTable$cityPartRange,na.rm = TRUE))

clean_DataTable$numPrevOwners[is.na(clean_DataTable$numPrevOwners)]<-as.integer(mean(clean_DataTable$numPrevOwners,na.rm = TRUE))

clean_DataTable$made[is.na(clean_DataTable$made)]<-as.integer(mean(clean_DataTable$made,na.rm = TRUE))


clean_DataTable$isNewBuilt[is.na(clean_DataTable$isNewBuilt)]<-
  if(mean(clean_DataTable$isNewBuilt,na.rm = TRUE) > 0.5){
    ceiling(mean(clean_DataTable$isNewBuilt,na.rm = TRUE))
  }else{
    floor(mean(clean_DataTable$isNewBuilt,na.rm = TRUE))}


clean_DataTable$hasStormProtector[is.na(clean_DataTable$hasStormProtector)]<-
  if(mean(clean_DataTable$hasStormProtector,na.rm = TRUE) > 0.5){
    ceiling(mean(clean_DataTable$hasStormProtector,na.rm = TRUE))
  }else{
    floor(mean(clean_DataTable$hasStormProtector,na.rm = TRUE))}


clean_DataTable$basement[is.na(clean_DataTable$basement)]<-mean(clean_DataTable$basement,na.rm = TRUE)

clean_DataTable$attic[is.na(clean_DataTable$attic)]<-mean(clean_DataTable$attic,na.rm = TRUE)

clean_DataTable$garage[is.na(clean_DataTable$garage)]<-mean(clean_DataTable$garage,na.rm = TRUE)

clean_DataTable$hasStorageRoom[is.na(clean_DataTable$hasStorageRoom)]<-
  if(mean(clean_DataTable$hasStorageRoom,na.rm = TRUE) > 0.5){
    ceiling(mean(clean_DataTable$hasStorageRoom,na.rm = TRUE))
  }else{
    floor(mean(clean_DataTable$hasStorageRoom,na.rm = TRUE))}

clean_DataTable$price[is.na(clean_DataTable$price)]<-mean(clean_DataTable$price,na.rm = TRUE)

clean_DataTable$squareMeters[is.na(clean_DataTable$squareMeters)]<-mean(clean_DataTable$squareMeters,na.rm = TRUE)

#--------------relations ---------------

plot( clean_DataTable$squareMeters,clean_DataTable$price,xlab = "House size [m^2]",ylab ="House price" , col='red', pch = 1)
plot( clean_DataTable$numberOfRooms,clean_DataTable$floors,xlab ="Number of rooms",ylab ="Number of floors" , col='red', pch = 1)
plot( clean_DataTable$squareMeters,clean_DataTable$numberOfRooms,xlab ="House size [m^2]",ylab ="Number of rooms" , col='red', pch = 1)
plot( clean_DataTable$price,clean_DataTable$category,xlab = "House price",ylab ="Basic = 0, Luxury = 1" , col='red', pch = 1)
plot( clean_DataTable$cityPartRange,clean_DataTable$category,xlab = "Area rating",ylab ="Basic = 0, Luxury = 1" , col='red', pch = 1)
plot( clean_DataTable$garage,clean_DataTable$category,xlab = "Garage size",ylab ="Basic = 0, Luxury = 1" , col='red', pch = 1)


#-----------conditional probability with unclean dataSet -------------------
#has yard = 0
DataSetYard0_Lux<-subset(DataTable,DataTable$hasYard==0 & DataTable$category == "Luxury")
DataSetYard0_Bas<-subset(DataTable,DataTable$hasYard==0 & DataTable$category == "Basic")
precentYard0_Lux<-count(DataSetYard0_Lux)/count(DataTable)
precentYard0_Bas<-count(DataSetYard0_Bas)/count(DataTable)

#P(Luxury|hasYard = 0)
p_Lux_con_NoYard <-precentYard0_Lux/precentYard0 

#P(Basic|hasYard = 0)
p_Bas_con_NoYard <-precentYard0_Bas/precentYard0

#has yard = 1
DataSetYard1_Lux<-subset(DataTable,DataTable$hasYard==1 & DataTable$category == "Luxury")
DataSetYard1_Bas<-subset(DataTable,DataTable$hasYard==1 & DataTable$category == "Basic")
precentYard1_Lux<-count(DataSetYard1_Lux)/count(DataTable)
precentYard1_Bas<-count(DataSetYard1_Bas)/count(DataTable)

#P(Luxury|hasYard = 1)
p_Lux_con_HasYard <-precentYard1_Lux/precentYard1

#P(Basic|hasYard = 1)
p_Bas_con_HasYard <-precentYard1_Bas/precentYard1



#has pool = 0
DataSetPool0_Lux<-subset(DataTable,DataTable$hasPool==0 & DataTable$category == "Luxury")
DataSetPool0_Bas<-subset(DataTable,DataTable$hasPool==0 & DataTable$category == "Basic")
precentPool0_Lux<-count(DataSetPool0_Lux)/count(DataTable)
precentPool0_Bas<-count(DataSetPool0_Bas)/count(DataTable)

#P(Luxury|hasPool= 0)
p_Lux_con_NoPool <-precentPool0_Lux/precentPool0 

#P(Basic|hasPool = 0)
p_Bas_con_NoPool <-precentPool0_Bas/precentPool0

#has pool = 1
DataSetPool1_Lux<-subset(DataTable,DataTable$hasPool==1 & DataTable$category == "Luxury")
DataSetPool1_Bas<-subset(DataTable,DataTable$hasPool==1 & DataTable$category == "Basic")
precentPool1_Lux<-count(DataSetPool1_Lux)/count(DataTable)
precentPool1_Bas<-count(DataSetPool1_Bas)/count(DataTable)


#P(Luxury|hasPool = 1)
p_Lux_con_HasPool <-precentPool1_Lux/precentPool0 

#P(Basic|hasPool = 1)
p_Bas_con_HasPool <-precentPool1_Bas/precentPool0


#city part range 1->5
DataSetPartRange1_5<-subset(DataTable,DataTable$cityPartRange>=1 & DataTable$cityPartRange<=5)
precentPartRange1_5<-count(DataSetPartRange1_5)/count(DataTable)

#city part range 1->5 and Luxury
DataSetPartRange1_5_Lux<-subset(DataTable,DataTable$cityPartRange>=1 & DataTable$cityPartRange<=5 & DataTable$category == "Luxury")
precentPartRange1_5_Lux<-count(DataSetPartRange1_5_Lux)/count(DataTable)

#P(Luxury|1 <= CityRange <= 5 )
p_Lux_con_Range1_5 <- precentPartRange1_5_Lux/precentPartRange1_5


#city part range 6->10
DataSetPartRange6_10<-subset(DataTable,DataTable$cityPartRange>5 & DataTable$cityPartRange<=10)
precentPartRange6_10<-count(DataSetPartRange6_10)/count(DataTable)

#city part range 6->10 and Luxury
DataSetPartRange6_10_Lux<-subset(DataTable,DataTable$cityPartRange>5 & DataTable$cityPartRange<=10 & DataTable$category == "Luxury")
precentPartRange6_10_Lux<-count(DataSetPartRange6_10_Lux)/count(DataTable)

#P(Luxury|6 <= CityRange <= 10 )
p_Lux_con_Range6_10 <- precentPartRange6_10_Lux/precentPartRange6_10


#---------------------reset clean data for logistic regression -----------------
clean_DataTable1 <- clean_DataTable
clean_DataTable1$Unnamed..0 <- NULL
clean_DataTable1 <- subset(clean_DataTable,attic < squareMeters)
clean_DataTable1 <- subset(clean_DataTable1,basement < squareMeters)
clean_DataTable1 <- subset(clean_DataTable1,garage < squareMeters)
str(clean_DataTable1)#check perimeters tip 
#-------------------Logistic Regression-----------------

library(tidyverse)
#------intstall
#install.packages("purrr")
library(caret)
theme_set(theme_bw())

# Fit the model
model <- glm(as.numeric(clean_DataTable1$category) ~., data = clean_DataTable1, family = binomial)
summary(model)


#check fit model
Fit_model <- glm(as.numeric(clean_DataTable1$category) ~hasYard + hasPool + made + isNewBuilt, data = clean_DataTable1, family = binomial)
summary(Fit_model)

#check fit model + hasGuestRoom + floor + cityCode
Fit_model_1 <- glm(as.numeric(clean_DataTable1$category) ~hasYard + hasPool + made + isNewBuilt + hasGuestRoom + floors + cityCode, data = clean_DataTable1, family = binomial)
summary(Fit_model_1)



#--------discretization of variables-----------------



# discretization of hasGuestRoom to binary 
clean_DataTable1$hasGuestRoom<-factor(ifelse(clean_DataTable1$hasGuestRoom > 0,1,0))


#check fit model + hasGuestRoom + floor + cityCode
Fit_model_2 <- glm(as.numeric(clean_DataTable1$category) ~hasYard + hasPool + made + isNewBuilt + hasGuestRoom  + cityCode, data = clean_DataTable1, family = binomial)
summary(Fit_model_2)



#discretization cityCode to 5 catgorys
for(X in 1:nrow(clean_DataTable1)){
  
  if(clean_DataTable1$cityCode[X]>=0 & clean_DataTable1$cityCode[X]<=20000) {
     clean_DataTable1$cityCode[X]<-1
  } else if(clean_DataTable1$cityCode[X]>20000 & clean_DataTable1$cityCode[X]<=40000) {
      clean_DataTable1$cityCode[X]<-2
  } else if(clean_DataTable1$cityCode[X]>40000 & clean_DataTable1$cityCode[X]<=60000) {
     clean_DataTable1$cityCode[X]<-3
  } else if(clean_DataTable1$cityCode[X]>60000 & clean_DataTable1$cityCode[X]<=80000) {
     clean_DataTable1$cityCode[X]<-4
  } else if(clean_DataTable1$cityCode[X]>80000 & clean_DataTable1$cityCode[X]<=100000) {
     clean_DataTable1$cityCode[X]<-5
  } 
}



#check fit model + hasGuestRoom + cityCode ## after discretization
Fit_model_2 <- glm(as.numeric(clean_DataTable1$category) ~hasYard + hasPool + made + isNewBuilt + hasGuestRoom  + cityCode , data = clean_DataTable1, family = binomial)
summary(Fit_model_2)

#------------------------------make fineal dataSet for project part B-----------------------------------------------------------------
#build final dataSet 
final_dataTable <- clean_DataTable

# discretization of hasGuestRoom to binary 
final_dataTable$hasGuestRoom<-factor(ifelse(final_dataTable$hasGuestRoom > 0,1,0))

#discretization cityCode to 5 catgorys
for(X in 1:nrow(final_dataTable)){
  
  if(final_dataTable$cityCode[X]>=0 & final_dataTable$cityCode[X]<=20000) {
    final_dataTable$cityCode[X]<-1
  } else if(final_dataTable$cityCode[X]>20000 & final_dataTable$cityCode[X]<=40000) {
    final_dataTable$cityCode[X]<-2
  } else if(final_dataTable$cityCode[X]>40000 & final_dataTable$cityCode[X]<=60000) {
    final_dataTable$cityCode[X]<-3
  } else if(final_dataTable$cityCode[X]>60000 & final_dataTable$cityCode[X]<=80000) {
    final_dataTable$cityCode[X]<-4
  } else if(final_dataTable$cityCode[X]>80000 & final_dataTable$cityCode[X]<=100000) {
    final_dataTable$cityCode[X]<-5
  } 
}

#make new col for final_dataTable 
Error_size_measure <- 0

#add knew variable to clean dataframe error mesure 
final_dataTable$Error_size_measure <- Error_size_measure

#enter wanted values into Error_size_measure to 4 catgorys
for(i in 1:nrow(final_dataTable)){
  
  if((final_dataTable$squareMeters[i] < final_dataTable$basement[i]) & (final_dataTable$squareMeters[i] < final_dataTable$attic[i]) & (final_dataTable$squareMeters[i] < final_dataTable$garage[i]) ){
    final_dataTable$Error_size_measure[i]<-3
  } else if((final_dataTable$squareMeters[i] < final_dataTable$basement[i] & final_dataTable$squareMeters[i] < final_dataTable$attic[i])
            |(final_dataTable$squareMeters[i] < final_dataTable$basement[i] & final_dataTable$squareMeters[i] < final_dataTable$garage[i])
            |(final_dataTable$squareMeters[i] < final_dataTable$attic[i] & final_dataTable$squareMeters[i] < final_dataTable$garage[i])){
    final_dataTable$Error_size_measure[i]<-2
  } else if((final_dataTable$squareMeters[i] < final_dataTable$basement[i])
            |(final_dataTable$squareMeters[i] < final_dataTable$attic[i])
            |(final_dataTable$squareMeters[i] < final_dataTable$garage[i])){
    final_dataTable$Error_size_measure[i]<-1
  }
  
}

# Fit the model
model <- glm(as.numeric(final_dataTable$category) ~., data = final_dataTable, family = binomial)
summary(model)

#check fit model
Fit_model <- glm(as.numeric(final_dataTable$category) ~hasYard + hasPool + made + isNewBuilt, data = final_dataTable, family = binomial)
summary(Fit_model)

#check fit model + hasGuestRoom + floor + cityCode
Fit_model_2 <- glm(as.numeric(final_dataTable$category) ~hasYard + hasPool + made + isNewBuilt + hasGuestRoom  + cityCode , data = final_dataTable, family = binomial)
summary(Fit_model_2)

#check fit model + hasGuestRoom + floor + cityCode + Error_size_measure 
Fit_model_3 <- glm(as.numeric(final_dataTable$category) ~hasYard + hasPool + made + isNewBuilt + hasGuestRoom + cityCode + Error_size_measure   , data = final_dataTable, family = binomial)
summary(Fit_model_3)

#remove unwanted column 
final_dataTable$squareMeters <- NULL
final_dataTable$numberOfRooms <- NULL
final_dataTable$floors <- NULL
final_dataTable$cityPartRange <- NULL
final_dataTable$numPrevOwners <- NULL
final_dataTable$hasStormProtector <- NULL
final_dataTable$basement <- NULL
final_dataTable$attic <- NULL
final_dataTable$garage <- NULL
final_dataTable$hasStorageRoom <- NULL
final_dataTable$price <- NULL


#change catgory from binary back to Luxury=1 and Basic=0
final_dataTable$category<-factor(ifelse(final_dataTable$category > 0,"Luxury","Basic"))

#remove Error_size_measure
final_dataTable$Error_size_measure<-NULL

#get new model in excel
#install.packages("openxlsx")
library(openxlsx)
write.xlsx(final_dataTable,'Xy_train_new.xlsx',colNames = TRUE)


