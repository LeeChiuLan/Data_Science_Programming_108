library(dplyr)
datafile <- "/Users/linda/Desktop/CS+X-108/20190711/convertcsv.csv"
#sitefile <- "/Users/linda/Desktop/CS+X-108/20190711/空氣品質監測站.csv"


data<-read.csv(datafile, header=T, sep=",")
dim(data)

#sitedata<-read.csv(sitefile, header=T, sep=",")
sitedata<-read.csv("/Users/linda/Desktop/CS+X-108/20190711/空氣品質監測站.csv", header=T, sep=",")
dim(sitedata)
head(sitedata)