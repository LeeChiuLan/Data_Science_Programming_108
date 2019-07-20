library(dplyr)
library(tmcn)
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)
library(rvest)
library(xml2)
library(rvest)
library(readxl)
library(kableExtra)

## 依縣市為Documents <- 有興趣縣市之間的關係

## 讀入資料

### [餐飲 - 觀光資訊資料庫](https://data.gov.tw/dataset/7779)
restaurantfile <- "../../convertcsv.csv"
restaurantdata_raw<-read.csv(restaurantfile, header=T, sep=",")
kable(head(restaurantdata_raw))

### [臺灣地區縣市名稱中英對照](https://alerts.ncdr.nat.gov.tw/Document/%E8%A1%8C%E6%94%BF%E5%8D%80%E4%BB%A3%E7%A2%BC%E8%A1%A8_Taiwan_Geocode_103_v2.xlsx)
mappingTable_raw<-read_xlsx("../../Datasets/行政區代碼表_Taiwan_Geocode_103_v2.xlsx")
head(mappingTable_raw)
mappingTable <- mappingTable_raw %>% select(2, 4) %>%
  rename(City_Eng=縣市英文名_100,City_CH=縣市名_100)
head(mappingTable)

mappingTable$City_Eng <- gsub('\\s+', '', mappingTable$City_Eng)
head(mappingTable)

### 對於"Description"依縣市("Add")合併儲存
getdoc <- function()

### 建立文本資料結構與基本文字清洗

