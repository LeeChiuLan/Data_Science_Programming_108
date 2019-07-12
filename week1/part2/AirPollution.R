library(dplyr)
restaurantfile <- "/Users/linda/Desktop/CS+X-108/20190711/convertcsv.csv"
#sitefile <- "/Users/linda/Desktop/CS+X-108/20190711/空氣品質監測站Eng.csv"


restaurantdata_raw<-read.csv(restaurantfile, header=T, sep="," , na.strings = c("", "NA"))
#restaurantdata_raw = restaurantdata_raw %>% na.omit()   # remove emty rows
dim(restaurantdata_raw)
head(restaurantdata_raw)
#sitedata<-read.csv(sitefile, header=T, sep=",")
sitedata_raw<-read.csv("/Users/linda/Desktop/CS+X-108/20190711/空氣品質監測站Eng.csv", header=T, sep=",", na.strings = c("", "NA"))
sitedata_raw= sitedata_raw %>% na.omit()   # remove emty rows
dim(sitedata_raw)
head(sitedata_raw)

sitedata <- select(sitedata_raw, Name, Longitude,Latitude)
head(sitedata)
sitedata$Name <- paste(sitedata$Name,"Site", sep="_")   #append "Site" to each $Name
head(sitedata)

names(sitedata)[names(sitedata) == "Longitude"] <- "Px"
names(sitedata)[names(sitedata) == "Latitude"] <- "Py"
Description=""
sitedata = cbind(sitedata, Description)
head(sitedata)
dim(sitedata)
#---------------
restaurantdata <- select(restaurantdata_raw, Name, Px, Py, Description)
head(restaurantdata)
dim(restaurantdata)

data = rbind(restaurantdata, sitedata)
library(kableExtra)

