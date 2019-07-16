library(dplyr)
library(readxl)
library(geosphere)

sitedata_raw<-read.csv("../../Datasets/空氣品質監測站Eng.csv", header=T, sep=",")

AQIfile <- "../../Datasets/ATM00679_2019_0322_0430.xlsx"
AQIdata_raw <- read_xlsx(AQIfile)
head(AQIdata_raw)

#--------------- [DataSet] Site ---------------
sitedata <- select(sitedata_raw, Name, Longitude,Latitude) %>%
  rename(SiteName=Name)
sitedata = sitedata[sitedata$SiteName != "",]    # remove empty row
dim(sitedata)
#sitedata$SiteName <- paste(sitedata$SiteName,"Site", sep="_")   #append "Site" to each $Name
head(sitedata)


getDistm <- function(lon1, lat1,lon2, lat2) {
  d <- distm (c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
  return(d)
}

# functiion: getTheNearestDistm
appTheNearestDistm <- function(dat,col1,col2){
  num <- nrow(dat)
  for(i in 1:num){
    minDistm <- .Machine$double.xmax
    ptr <- NULL
    for(j in 1:num){
      if(i!=j){
        d <- getDistm(dat[i,col1],dat[i,col2], dat[j,col1],dat[j,col2])
        if(d < minDistm){
          minDistm <- d
          ptr <- j
        }
      }
    }
    dat[i,"NearestSiteDistm"] <- minDistm
    dat[i,"NearestSite"] <- dat[ptr,"SiteName"]
  }
  
  return(dat)
}

sitedata=appTheNearestDistm(sitedata,"Longitude", "Latitude")

#--------------- Merge DataSet ---------------
AQIdata=merge(AQIdata_raw, sitedata, by.x="SiteName")
head(AQIdata)

library(ggplot2)
Xindian_data<-AQIdata%>%filter(SiteName=="新店")
df <- Xindian_data
df <- df[!(df$AQI == ""), ]
df <- df[!(df$PM25SubIndex == ""), ]
ggplot(data = df, aes(x=MonitorDate, y= AQI)) + geom_line()
ggplot(data = df, aes(x= Class)) + geom_bar(fill = "lightblue", colour="black")

library(plyr)
ddply(df, .(Weekday), summarize,  Rate_AQI=mean(AQI), Rate_PM25=mean(PM25SubIndex))

# function: isContained
isContained <- function(r,Obj1, Obj2) {
  d <- getDistm(Obj1, Obj2)
  if (d <= r){
    return (TRUE)
  }else
    {
      return (FALSE)
  }
}

