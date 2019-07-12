library(dplyr)
restaurantfile <- "/Users/linda/Desktop/CS+X-108/20190711/convertcsv.csv"
#sitefile <- "/Users/linda/Desktop/CS+X-108/20190711/空氣品質監測站Eng.csv"

restaurantdata_raw<-read.csv(restaurantfile, header=T, sep=",")
dim(restaurantdata_raw)
head(restaurantdata_raw)
#sitedata<-read.csv(sitefile, header=T, sep=",")  # failed to read like this since the file name is in Chinese
sitedata_raw<-read.csv("/Users/linda/Desktop/CS+X-108/20190711/空氣品質監測站Eng.csv", header=T, sep=",")
dim(sitedata_raw)
head(sitedata_raw)

#--------------- [DataSet] site ---------------
sitedata <- select(sitedata_raw, Name, Longitude,Latitude)
sitedata = sitedata[sitedata$Name != "",]    # remove empty row
dim(sitedata)
sitedata$Name <- paste(sitedata$Name,"Site", sep="_")   #append "Site" to each $Name
#head(sitedata)

# rename the specified columns' name 
names(sitedata)[names(sitedata) == "Longitude"] <- "Px"
names(sitedata)[names(sitedata) == "Latitude"] <- "Py"
# add a column - "Description"
Description=""
sitedata = cbind(sitedata, Description)
head(sitedata)
dim(sitedata)

#--------------- [DataSet] restaurant ---------------
restaurantdata <- select(restaurantdata_raw, Name, Px, Py, Description)
head(restaurantdata)
dim(restaurantdata)

# merge [sie], [restaurant]
data = rbind(restaurantdata, sitedata)
dim(data)
library(kableExtra)

