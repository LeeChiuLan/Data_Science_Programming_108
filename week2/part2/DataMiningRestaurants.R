library(dplyr)
library(NLP)
library(xml2)
library(tmcn)
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
restaurantfile <- "../../Datasets/convertcsv.csv"
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
dff = dff[dff$Add != "",]    # remove empty row
dff = dff[dff$Description != "",]    # remove empty row
des_dir <- "./DATA"
getdoc(dff,des_dir)

txt<-system.file("texts","txt",package = 'tm')
docs<-Corpus(DirSource(txt,encoding = "UTF-8"))
#writeCorpus(docs, path = "./DATA", filenames = NULL)


### 建立文本資料結構與基本文字清洗
d.corpus <- Corpus( DirSource("./DATA") )
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})

### 進行斷詞，並依照縣市建立文本矩陣 TermDocumentMatrix
mixseg = worker()
jieba_tokenizer = function(d)
{
  unlist( segment(d[[1]], mixseg) )
}
seg = lapply(d.corpus, jieba_tokenizer)

count_token = function(d)
{
  as.data.frame(table(d))
}
tokens = lapply(seg, count_token)

n = length(seg)
TDM = tokens[[1]]
colNames <- names(seg)
colNames <- gsub(".txt", "", colNames)
for( id in c(2:n) )
{
  TDM = merge(TDM, tokens[[id]], by="d", all = TRUE)
  names(TDM) = c('d', colNames[1:id])
}
TDM[is.na(TDM)] <- 0
library(knitr)
kable(head(TDM))

kable(tail(TDM))

### 將已建好的 TDM 轉成 TF-IDF
tf <- apply(as.matrix(TDM[,2:(n+1)]), 2, sum)

library(Matrix)
idfCal <- function(word_doc)
{ 
  log2( n / nnzero(word_doc) ) 
}
idf <- apply(as.matrix(TDM[,2:(n+1)]), 1, idfCal)

doc.tfidf <- TDM

tempY = matrix(rep(c(as.matrix(tf)), each = length(idf)), nrow = length(idf))
tempX = matrix(rep(c(as.matrix(idf)), each = length(tf)), ncol = length(tf), byrow = TRUE)
doc.tfidf[,2:(n+1)] <- (doc.tfidf[,2:(n+1)] / tempY) * tempX

stopLine = rowSums(doc.tfidf[,2:(n+1)])
delID = which(stopLine == 0)

kable(head(doc.tfidf[delID,1]))
kable(tail(doc.tfidf[delID,1]))

TDM = TDM[-delID,]
doc.tfidf = doc.tfidf[-delID,]

### TF-IDF 文章取得的重要關鍵字
TopWords = data.frame()
for( id in c(1:n) )
{
  dayMax = order(doc.tfidf[,id+1], decreasing = TRUE)
  showResult = t(as.data.frame(doc.tfidf[dayMax[1:10],1]))
  TopWords = rbind(TopWords, showResult)
}
rownames(TopWords) = colnames(doc.tfidf)[2:(n+1)]
TopWords = droplevels(TopWords)
kable(TopWords)

### TF-IDF Hours 文章取得的重要關鍵字 TDM merge 視覺化
TDM$d = as.character(TDM$d)
AllTop = as.data.frame( table(as.matrix(TopWords)) )
AllTop = AllTop[order(AllTop$Freq, decreasing = TRUE),]

kable(head(AllTop))

### ggplot 
TopNo = 6
tempGraph = data.frame()
for( t in c(1:TopNo) )
{
  word = matrix( rep(c(as.matrix(AllTop$Var1[t])), each = n), nrow = n )
  temp = cbind( colnames(doc.tfidf)[2:(n+1)], t(TDM[which(TDM$d == AllTop$Var1[t]), 2:(n+1)]), word )
  colnames(temp) = c("city", "freq", "words")
  tempGraph = rbind(tempGraph, temp)
  names(tempGraph) = c("city", "freq", "words")
}

library(ggplot2)

library(varhandle)
tempGraph$freq = unfactor(tempGraph$freq)
ggplot(tempGraph, aes(city, freq)) + 
  geom_point(aes(color = words, shape = words), size = 5) +
  geom_line(aes(group = words, linetype = words))+
  theme(text=element_text(family="黑體-繁 中黑", size=14),axis.text.x = element_text(angle=90, hjust=1))

kable(tail(AllTop))

### 
