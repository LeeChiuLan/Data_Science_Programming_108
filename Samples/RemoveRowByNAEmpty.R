library(dplyr)
library(readxl)
library(ggplot2)
library(plyr)

sites <- c('士林','大同','中山','古亭','松山','陽明','萬華')
colorSet1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
colorSet <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Env Settings
Sys.setlocale("LC_TIME", "en_US")
week_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday",
               "Friday", "Saturday", "Sunday")
month_levels <- c("January","February","March","April","May",
                  "June","July","August","September","October","November","December")
month_num <- c("1","2","3","4","5","6","7","8","9","10","11","12")
hour_num <- c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23")
dir_pic <- "../project/pictures"

# Variables Settings
ID1 <- 1   #士林
ID2 <- 2   #大同
ID3 <- 3   #中山
ID4 <- 4   #古亭
ID5 <- 5   #松山
ID6 <- 6   #陽明
ID7 <- 7   #萬華

creatPath <- function(subDir){
  dir.create(file.path(dir_pic, subDir), recursive = TRUE, showWarnings = FALSE)
  ret <- paste0(dir_pic,subDir,"/")
  return (ret)
}

doActions <- function(index){
  siteName <- sites[index]
  plotColor <- colorSet[index]
  df <- read_excel(paste0("../Datasets/", siteName ,".xlsx"))
  #dim(df)

  # remove the row which value is "NR"
  selector = rowSums(df[4:27] == "NR") == 0
  df<-df[selector, ]
  #dim(df)

  # remove the row which value is ""
  selector = rowSums(df[4:27] == "") == 0
  df<-df[selector, ]
  #dim(df)

  # add weekday
  colnames(df)[1] <- "Date"
  df$WeekDay <- weekdays(as.Date(df$Date))
  df$WeekDay <- ordered(df$WeekDay, levels=week_levels)

  # add Month
  df <- df %>% mutate(Month = months(Date))

  # select the PM2.5 only
  pm25data <- df[df[3] == "PM2.5",]
  #dim(pm25data)
  #head(pm25data)

  # remove NA
  pm25data <- pm25data[complete.cases(pm25data), ]
  #pm25data

  pm25data[,4:27]<-lapply(pm25data[,4:27], function(x) as.numeric(as.character(x)))
  pm25data <- pm25data[complete.cases(pm25data), ]  # once again to remove NA
  pm25data$meansValue <- rowMeans(pm25data[,4:27]) %>% round(digits = 2)
  
  filename <- paste0(siteName,".png") # set the output filename for ggplot
  # 1. Draw by day the whole year
  ggplot(data = pm25data, aes(x=Date, y= meansValue)) + geom_line(color=plotColor) +
    theme(text=element_text(family="黑體-繁 中黑", size=14)) +  
    labs(title=paste0("PM2.5 - [日]   ",siteName,"測站"))
  
  ggsave(filename, path = creatPath("/ByDate"))
  
  # 2.Draw by weekday
  # 星期平均 by Year
  df_PM25_WK <- ddply(pm25data, .(WeekDay), summarize, Rate_PM25=mean(meansValue)%>%round(digits = 2))
  print(df_PM25_WK)
  g <- ggplot(df_PM25_WK, aes(x=WeekDay, y=Rate_PM25, label = Rate_PM25)) + 
    geom_bar(stat="identity", width=.5, fill=plotColor) + 
    labs(title=paste0("PM2.5 - [2018年]   ",siteName,"測站")) +
    theme(text=element_text(family="黑體-繁 中黑", size=14),axis.text.x = element_text(angle=60, hjust=1))
  g + geom_text(aes(label = Rate_PM25),size = 4, hjust = 0.5, vjust = 3, position=position_dodge(width=0.9))
  
  ggsave(filename, path = creatPath("/Means/WeekByYear"))

  # 3. Draw weekday per month
  # 選出各月集合
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

  # 合併資料集
  df_PM25_WK_Months <- rbind(df_PM25_WK_M1,df_PM25_WK_M2,df_PM25_WK_M3,df_PM25_WK_M4,df_PM25_WK_M5,df_PM25_WK_M6,df_PM25_WK_M7,df_PM25_WK_M8,df_PM25_WK_M9,df_PM25_WK_M10,df_PM25_WK_M11,df_PM25_WK_M12)
  # 設定月排序
  df_PM25_WK_Months$Month <- ordered(df_PM25_WK_Months$Month, levels=month.name)
  
  g<-ggplot(data = df_PM25_WK_Months, aes(x = as.factor(x = Month), 
                           y = Rate_PM25, 
                           col = WeekDay, 
                           group = WeekDay)) + 
                  geom_line() + 
                  geom_point() + 
                  labs(title=paste0("PM2.5 - [2018年]   ",siteName,"測站")) +
                  scale_color_manual(values=colorSet1)
  g+theme(text=element_text(family="黑體-繁 中黑", size=14),axis.text.x = element_text(angle=60, hjust=1))
  
  ggsave(filename, path = creatPath("/Means/WeekByMonth"))
  
  return(pm25data)
}

data_1 <- doActions(ID1)
data_2 <- doActions(ID2)
data_3 <- doActions(ID3)
data_4 <- doActions(ID4)
data_5 <- doActions(ID5)
data_6 <- doActions(ID6)
data_7 <- doActions(ID7)

# select a range of dates
getRangeInDF <- function(df,MM,dd1=NULL,dd2=NULL){
  theNumberOne <- "-01"
  theNumberSevent <- "-08"
  
  if(is.null(dd1)==FALSE) {
    theNumberOne <- paste0("-",dd1)
  }
  if(is.null(dd2)==FALSE) {
    theNumberSevent <- paste0("-",dd2+1)
  }
  
  startDate <- paste0("2018-",month_num[MM],theNumberOne)
  endDate <- paste0("2018-",month_num[MM],theNumberSevent)
  
  ptimeDate<- subset(df, Date >= startDate & Date < endDate)
  return(ptimeDate)
}

flatByDate <- function(dataIn){
  df["Date"] = dataIn.colindex.names[index]
}

transport4hour <- function(dataIn,index){
  data1 <- as.data.frame(t(dataIn[,1:27]))
  
  # the first row will be the header
  colnames(data1) = as.character(unlist(data1[1,]))
  data1 = data1[-1, ]          # removing the first row.
  data1$Hour <- factor(row.names(data1))
  
  data1 <- data1[-c(1, 2),] # remove the first 2 rows
  return(data1)
}

plotMonthsByHours <- function(dataIn,index,mm){
  siteName <- sites[index]
  data <- dataIn
  ptimeDate <- getRangeInDF(df=data,MM=mm)
  n <- nrow(ptimeDate)
  if(n<7){
    warning(paste0(siteName,' Month-',mm,': obtain the rows ',n, ' < 7'))
  }else
  { # ok, let's do it.
    df_PM25_WK_Hours <- transport4hour(ptimeDate,index = index)
    testData <- df_PM25_WK_Hours
    filename <- paste0(siteName,"-",mm,".png") # set the output filename for ggplot
    # 4. Draw by hour 
    library(reshape2)
    hrData<- melt(testData, id.vars="Hour")   #"value" and "variable" are default output column names of melt()
    #hrData['value'].astype(int)
    hrData$Hour <- ordered(hrData$Hour, levels=hour_num)
    s <- ggplot(data = hrData, aes(x=Hour,y=value, group=variable)) 
    s+ geom_line(aes(color=variable),size=1) + geom_point(color="red",size=0.6)+
      labs(title=paste0("PM2.5 - [時]   ",siteName,"測站"))+
      scale_color_manual(values=colorSet1)+
      theme(text=element_text(family="黑體-繁 中黑", size=14),axis.text.x = element_text(angle=60, hjust=1))
    
    ggsave(filename, path = creatPath("/ByHour"))
      
  }
}

doTask2 <- function(dataIn,index){
  for(mm in 1:12){
    plotMonthsByHours(dataIn,index,mm)
  }
}

# verify
#ptimeDate <- getRangeInDF(df=data_2,MM=11)
#pHourData <- transport4hour(ptimeDate,index = 2)
#testData <- pHourData


doTask2(data_1,ID1)
doTask2(data_2,ID2)
doTask2(data_3,ID3)
doTask2(data_4,ID4)
doTask2(data_5,ID5)
doTask2(data_6,ID6)
doTask2(data_7,ID7)





