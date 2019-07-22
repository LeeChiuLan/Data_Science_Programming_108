library(dplyr)
library(readxl)

index <- 1

sites <- c('士林','大同','中山','古亭','松山','陽明','萬華')
#colorSet <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
colorSet <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

siteName <- sites[index]
plotColor <- colorSet[index]
df <- read_excel(paste0("../Datasets/", siteName ,".xlsx"))
dim(df)

# remove the row which value is "NR"
selector = rowSums(df[4:27] == "NR") == 0
df<-df[selector, ]
dim(df)

# remove the row which value is ""
selector = rowSums(df[4:27] == "") == 0
df<-df[selector, ]
dim(df)

# add weekday
Sys.setlocale("LC_TIME", "en_US")
df <- df %>% rename(Date="日期")
df$WeekDay <- weekdays(as.Date(df$MonitorDate))

df$WeekDay <- ordered(df$WeekDay, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                           "Friday", "Saterday", "Sunday"))

# select the PM2.5 only
pm25data <- df[df[3] == "PM2.5",]
dim(pm25data)
pm25data

# remove NA
pm25data <- pm25data[complete.cases(pm25data), ]
pm25data

pm25data[,4:27]<-lapply(pm25data[,4:27], function(x) as.numeric(as.character(x)))
pm25data$Value <- rowMeans(pm25data[,4:27]) %>% round(digits = 2)
library(ggplot2)
ggplot(data = pm25data, aes(x=Date, y= Value)) + geom_line(color=plotColor) +
  theme(text=element_text(family="黑體-繁 中黑", size=14)) +  
  labs(title=paste0("PM2.5 - [日]   ",siteName,"測站"), caption="source: mpg")
