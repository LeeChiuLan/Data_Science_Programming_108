library(dplyr)
library(readxl)

index <- 1

sites <- c('士林','大同','中山','古亭','松山','陽明','萬華')
colorSet1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
colorSet <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

doActions <- function(index){
  v <- 1
}

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
                                           "Friday", "Saturday", "Sunday"))

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
pm25data <- pm25data[complete.cases(pm25data), ]  # once again to remove NA
pm25data$meansValue <- rowMeans(pm25data[,4:27]) %>% round(digits = 2)
library(ggplot2)
ggplot(data = pm25data, aes(x=Date, y= meansValue)) + geom_line(color=plotColor) +
  theme(text=element_text(family="黑體-繁 中黑", size=14)) +  
  labs(title=paste0("PM2.5 - [日]   ",siteName,"測站"), caption="source: mpg")

# 星期平均 by Year
library(plyr)
df_PM25_WK <- ddply(pm25data, .(WeekDay), summarize, Rate_PM25=mean(meansValue)%>%round(digits = 2))
print(df_PM25_WK)
g <- ggplot(df_PM25_WK, aes(x=WeekDay, y=Rate_PM25, label = Rate_PM25)) + 
  geom_bar(stat="identity", width=.5, fill=plotColor) + 
  labs(title=paste0("PM2.5 - [2018年]   ",siteName,"測站"), 
       caption="source: mpg") +
  theme(text=element_text(family="黑體-繁 中黑", size=14),axis.text.x = element_text(angle=60, hjust=1))
g + geom_text(aes(label = Rate_PM25),size = 4, hjust = 0.5, vjust = 3, position=position_dodge(width=0.9), vjust=-0.25)

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
# 星期平均 by Month
mdf <- pm25data_M1
df_PM25_WK_M1 <- ddply(mdf, .(WeekDay,Month), summarize, Rate_PM25=mean(meansValue)%>%round(digits = 2))
mdf <- pm25data_M2
df_PM25_WK_M2 <- ddply(mdf, .(WeekDay,Month), summarize, Rate_PM25=mean(meansValue)%>%round(digits = 2))
mdf <- pm25data_M3
df_PM25_WK_M3 <- ddply(mdf, .(WeekDay,Month), summarize, Rate_PM25=mean(meansValue)%>%round(digits = 2))
mdf <- pm25data_M4
df_PM25_WK_M4 <- ddply(mdf, .(WeekDay,Month), summarize, Rate_PM25=mean(meansValue)%>%round(digits = 2))
mdf <- pm25data_M5
df_PM25_WK_M5 <- ddply(mdf, .(WeekDay,Month), summarize, Rate_PM25=mean(meansValue)%>%round(digits = 2))
mdf <- pm25data_M6
df_PM25_WK_M6 <- ddply(mdf, .(WeekDay,Month), summarize, Rate_PM25=mean(meansValue)%>%round(digits = 2))
mdf <- pm25data_M7
df_PM25_WK_M7 <- ddply(mdf, .(WeekDay,Month), summarize, Rate_PM25=mean(meansValue)%>%round(digits = 2))
mdf <- pm25data_M8
df_PM25_WK_M8 <- ddply(mdf, .(WeekDay,Month), summarize, Rate_PM25=mean(meansValue)%>%round(digits = 2))
mdf <- pm25data_M9
df_PM25_WK_M9 <- ddply(mdf, .(WeekDay,Month), summarize, Rate_PM25=mean(meansValue)%>%round(digits = 2))
mdf <- pm25data_M1
df_PM25_WK_M1 <- ddply(mdf, .(WeekDay,Month), summarize, Rate_PM25=mean(meansValue)%>%round(digits = 2))
mdf <- pm25data_M10
df_PM25_WK_M10 <- ddply(mdf, .(WeekDay,Month), summarize, Rate_PM25=mean(meansValue)%>%round(digits = 2))
mdf <- pm25data_M11
df_PM25_WK_M11 <- ddply(mdf, .(WeekDay,Month), summarize, Rate_PM25=mean(meansValue)%>%round(digits = 2))
mdf <- pm25data_M12
df_PM25_WK_M12 <- ddply(mdf, .(WeekDay,Month), summarize, Rate_PM25=mean(meansValue)%>%round(digits = 2))

df_PM25_WK_Months <- rbind(df_PM25_WK_M1,df_PM25_WK_M2,df_PM25_WK_M3,df_PM25_WK_M4,df_PM25_WK_M5,df_PM25_WK_M6,df_PM25_WK_M7,df_PM25_WK_M8,df_PM25_WK_M9,df_PM25_WK_M10,df_PM25_WK_M11,df_PM25_WK_M12)

df_PM25_WK_Months$Month <- ordered(df_PM25_WK_Months$Month, levels=month.name)

g<-ggplot(data = df_PM25_WK_Months, aes(x = as.factor(x = Month), 
                           y = Rate_PM25, 
                           col = WeekDay, 
                           group = WeekDay)) + 
  geom_line() + 
  geom_point()+ 
  labs(title=paste0("PM2.5 - [2018年]   ",siteName,"測站"), 
       caption="source: mpg")+
  scale_color_manual(values=colorSet1)
g+theme(text=element_text(family="黑體-繁 中黑", size=14),axis.text.x = element_text(angle=60, hjust=1))