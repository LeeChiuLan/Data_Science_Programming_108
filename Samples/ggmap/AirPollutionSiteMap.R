library(dplyr)
library(ggplot2)
library(ggmap)

stopifnot(has_google_key()){
  cat("register_google(key =\"xxxxx....\") Firstly")
}

twmap <- get_googlemap(center = c(lon=120.58,lat=23.58),zoom = 7, language = "zh-TW", maptype="roadmap")
#ggmap(twmap)
sitedata_raw<-read.csv("/Users/linda/Desktop/CS+X-108/20190711/空氣品質監測站Eng.csv", header=T, sep=",")

sitedata <- select(sitedata_raw, Name, Longitude,Latitude)
sitedata = sitedata[sitedata$Name != "",]    # remove empty row
SiteMap <- ggmap(twmap) + geom_point(data = sitedata, aes(x = Longitude, y = Latitude), color="red")

#SiteMap <- ggmap(twmap, darken = c(0.5,"white")) + geom_point(data = sitedata, aes(x = Longitude, y = Latitude), color="red")

SiteMap