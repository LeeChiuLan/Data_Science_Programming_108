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
colnames(df)[1] <- "Date"
df$WeekDay <- weekdays(as.Date(df$Date))
df$WeekDay <- ordered(df$WeekDay, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                           "Friday", "Saterday", "Sunday"))

# add Month
df <- df %>% mutate(Month = months(Date))

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

# 畫星期平均
library(plyr)
df_PM25_WK <- ddply(pm25data, .(WeekDay), summarize, Rate_PM25=mean(Value)%>%round(digits = 2))
print(df_PM25_WK)

month_levels <- c("January","February","March","April","May","June","July","August","September","October","November","December")
pm25data_M1 <- pm25data[pm25data$Month == month_levels[1],]
pm25data_M2 <- pm25data[pm25data$Month == month_levels[2],]
pm25data_M3 <- pm25data[pm25data$Month == month_levels[3],]
pm25data_M4 <- pm25data[pm25data$Month == month_levels[4],]
pm25data_M5 <- pm25data[pm25data$Month == month_levels[5],]
pm25data_M6 <- pm25data[pm25data$Month == month_levels[6],]
pm25data_M7 <- pm25data[pm25data$Month == month_levels[7],]
pm25data_M8 <- pm25data[pm25data$Month == month_levels[8],]
pm25data_M9 <- pm25data[pm25data$Month == month_levels[9],]
pm25data_M10 <- pm25data[pm25data$Month == month_levels[10],]
pm25data_M11 <- pm25data[pm25data$Month == month_levels[11],]
pm25data_M12 <- pm25data[pm25data$Month == month_levels[12],]
