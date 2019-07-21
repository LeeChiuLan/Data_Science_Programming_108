library(dplyr)
library(xml2)
library(tmcn)
library(NLP)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)
library(rvest)
library(rvest)
library(readxl)
library(kableExtra)
library(tm)  #load text mining library

## 依縣市為Documents <- 有興趣縣市之間的關係

## 讀入資料

### [餐飲 - 觀光資訊資料庫](https://data.gov.tw/dataset/7779)
restaurantfile <- "../../convertcsv.csv"
restaurantdata_raw<-read.csv(restaurantfile, header=T, sep=",")
kable(head(restaurantdata_raw))

### [臺灣地區縣市名稱中英對照](https://alerts.ncdr.nat.gov.tw/Document/%E8%A1%8C%E6%94%BF%E5%8D%80%E4%BB%A3%E7%A2%BC%E8%A1%A8_Taiwan_Geocode_103_v2.xlsx)
mappingTable_raw<-read_xlsx("../../Datasets/行政區代碼表_Taiwan_Geocode_103_v2.xlsx")
head(mappingTable_raw)

### 只要縣市中英對照
mappingTable <- mappingTable_raw %>% select(2, 4) %>%
  rename(City_Eng=縣市英文名_100,City_CH=縣市名_100)
head(mappingTable)

### 英文名去除空白
mappingTable$City_Eng <- gsub('\\s+', '', mappingTable$City_Eng)
head(mappingTable)

### function getEngName()
library(stringr)
getEngName <- function(grepString){
  ans <- mappingTable %>% filter(str_detect(City_CH, grepString))
  return(ans$City_Eng)
}

#### 驗證function getEngName()
TaiEng <- getEngName("臺北")
TaiEng


getdoc <- function(df,des_dir){
  new_df <- df %>% select(Add,Description)
  #head(new_df)

  if(dir.exists(des_dir)){
    unlink(paste0(des_dir, "/*", ""))
  }else
  {
    dir.create(file.path(des_dir))
  }
  
  cutter <- worker("tag")
  num <- nrow(new_df)
  for(i in 1:num){
    strlist <- as.character(new_df[i,"Add"])
    city<-cutter[strlist][][1] %>% getEngName
    if(length(city) > 0){
      doc <- as.character(new_df[i,"Description"])
      if(length(doc) > 0){
        name <- paste0(des_dir, "/",unique(city), ".txt")
        #print(name)
        write(doc[1], unique(name[1]), append = TRUE)
      }
    }
  }
}

### 對於"Description"依縣市("Add")合併儲存
dff <- restaurantdata_raw
dff = dff[df$Add != "",]    # remove empty row
dff = dff[df$Description != "",]    # remove empty row
des_dir <- "./DATA"
getdoc(dff,des_dir)

txt<-system.file("texts","txt",package = 'tm')
docs<-Corpus(DirSource(txt,encoding = "UTF-8"))
writeCorpus(docs, path = "./DATA", filenames = NULL)



### 建立文本資料結構與基本文字清洗

