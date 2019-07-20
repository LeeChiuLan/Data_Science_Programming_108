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

# 抓取整個頁面內容
full_page <- read_html("https://sports.ltn.com.tw/basketball/")
full_page

#找出在資料在頁面中的位置後提取資料
tag_nodes <-  html_nodes(full_page, ".boxTitle .listA .list_title")
head(tag_nodes)
title <- html_text(tag_nodes) %>% iconv("UTF-8")
head(title)

str(title)

# 開始斷詞
cutter <- worker("tag")
itemName<-cutter[title]
head(itemName)

### table
data_in_table <- table(itemName)

### 轉成data.frame
raw_data <- data.frame(data_in_table)
head(raw_data)

### 排序 Top-6
head(raw_data[order(raw_data$Freq,decreasing = TRUE),])

### 文字雲
head(sort(data_in_table, decreasing=T))

par(family=("Heiti TC Light"))  # will fix the error code with Chinese
wordcloud(raw_data$itemName, raw_data$Freq,
          min.freq = 1, max.words=200, scale=c(4,.2),
          random.order=FALSE, colors=brewer.pal(5,"Dark2"))

### 新增字典
dic = c("HBL")
new_user_word(cutter,dic)

### jieba_tokenizer()
mixseg = worker()
jieba_tokenizer <- function(d){
  unlist(segment(d[1],mixseg))
}

docs <- title
seg <- lapply(docs, jieba_tokenizer)
freqFrame <- as.data.frame(table(unlist(seg)))
freqFrame <- freqFrame[-c(1:34),]
freqFrame
wordcloud(freqFrame$Var1, freqFrame$Freq,
          scale=c(5,0.5), min.freq = 10,max.words = 50,
          rot.per = 0, colors=brewer.pal(8,"Dark2"),
          ordered.colors = FALSE, use.r.layout = FALSE,
          fixed.asp = TRUE)