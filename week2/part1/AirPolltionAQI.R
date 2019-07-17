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
nearNB.name <- Xindian_data$NearestSite %>% factor %>% levels
nearNB.dist <- Xindian_data$NearestSiteDistm %>% factor %>% levels
nearNB_data<-AQIdata%>%filter(SiteName==nearNB.name)


# remove a row if value == ""
df <- Xindian_data
df <- df[!(df$AQI == ""), ]
df <- df[!(df$PM25SubIndex == ""), ]
ggplot(data = df, aes(x=MonitorDate, y= AQI)) + geom_line()

color_group = c("red", "blue", "green")
ggplot(data = Xindian_data, aes(x= Weekday, fill = Class)) + geom_bar() + 
  scale_fill_manual(values=color_group)

library(plyr)
df_AQI <- ddply(df, .(Weekday), summarize,  Rate_AQI=mean(AQI)%>%round(digits = 2), Rate_PM25=mean(PM25SubIndex)%>%round(digits = 2))

g <- ggplot(df_AQI, aes(x=Weekday, y=Rate_AQI, label = Rate_AQI)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Site:[Xindian] AQI", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
g + geom_text(aes(label = Rate_AQI),size = 4, hjust = 0.5, vjust = 3, position=position_dodge(width=0.9), vjust=-0.25)

new_AQI <- rbind(Xindian_data,nearNB_data) %>% select(SiteName,SiteId,MonitorDate,AQI,PM25SubIndex,Weekday,Class,NearestSiteDistm,NearestSite,NearestSiteDistm)
head(new_AQI,-10)

s <- ggplot(data = new_AQI, aes(x=MonitorDate,y=AQI, group=SiteName)) 
s + geom_line(linetype="dashed", color="lightblue", size=1.2) + geom_point(color="red",size=3)

ggplot(data = new_AQI, aes(x=MonitorDate, y= AQI)) + geom_line(aes(color=SiteName))+
  theme(text=element_text(family="黑體-繁 中黑", size=14))

ggplot(data = new_AQI, aes(x=MonitorDate, y= PM25SubIndex)) + geom_line(aes(color=SiteName))+
  theme(text=element_text(family="黑體-繁 中黑", size=14)) + 
  labs(title="PM2.5", caption="source: mpg")

ggplot(data = new_AQI, aes(x=Weekday, y= AQI), fill=SiteName) + geom_boxplot(aes(color=SiteName))+
  theme(text=element_text(family="黑體-繁 中黑", size=14))

library("gridExtra")
p1 <- ggplot(data = Xindian_data, aes(x= Weekday, fill = Class)) + geom_bar() + 
  scale_fill_manual(values=color_group) + 
  labs(title="Site:[Xindian]", caption="source: mpg")  
p2 <- ggplot(data = nearNB_data, aes(x= Weekday, fill = Class)) + geom_bar() + 
  scale_fill_manual(values=color_group) +  
  labs(title="Site:[Guting]", caption="source: mpg")  
grid.arrange(p1,p2)

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

