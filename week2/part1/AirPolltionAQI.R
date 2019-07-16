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

# ordering of weekday
AQIdata$Weekday <- ordered(AQIdata$Weekday, levels=c("Mon", "Tue", "Wed", "Thu", 
                                                     "Fri", "Sat", "Sun"))

Xindian_data<-AQIdata%>%filter(SiteName=="新店")

# remove a row if value == ""
df <- Xindian_data
df <- df[!(df$AQI == ""), ]
df <- df[!(df$PM25SubIndex == ""), ]
ggplot(data = df, aes(x=MonitorDate, y= AQI)) + geom_line()
ggplot(data = df, aes(x= Class)) + geom_bar(fill = "lightblue", colour="black")

library(plyr)
df_AQI <- ddply(df, .(Weekday), summarize,  Rate_AQI=mean(AQI), Rate_PM25=mean(PM25SubIndex))

g <- ggplot(df_AQI, aes(x=Weekday, y=Rate_AQI, label = Rate_AQI)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Site:[Xindian] AQI", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
g + geom_text(aes(label = Rate_AQI),size = 4, hjust = 0.5, vjust = 3, position=position_dodge(width=0.9), vjust=-0.25)

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

