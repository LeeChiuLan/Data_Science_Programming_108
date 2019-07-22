library(dplyr)
library(readxl)

#df<-read.csv("../Datasets/二林_raw.csv",encoding = "UTF-8")
df <- read_excel("../Datasets/二林.xlsx")
dim(df)

hourCols <- c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23") %>% as.character()
class(hourCols)
# remove empty row

for(i in hourCols){
  #df[!(is.na(df$i) | df$i==""| df$i=="NR"), ]
  #print(class(i))
  print(df[[i]][1])
  if(df[[i]]=="NR"){
    print(i)
  }
}

dim(df)

selector = rowSums(df[4:27] == "NR") == 0
df[selector, ]
