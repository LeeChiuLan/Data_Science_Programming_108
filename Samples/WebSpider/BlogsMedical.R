library(dplyr)
library(xml2)
library(rvest)
library(wordcloud)

full_page <- read_html("https://www.pixnet.net/blog/articles/category/33")
full_page

all_nodes<- html_node(full_page,xpath="/html/body")
rank1_title <- all_nodes %>% html_node("h3>a")
rank1_title 
titles <- html_attr(rank1_title,"title")
titles
urls <- html_attr(rank1_title,"href")
urls

df <- data.frame("title"=titles,"URL"=urls)
df

rank <- 10   # get the top-10
for(i in 2:rank){
  rank_tag <- paste0(".rank-",i)
  cat(rank_tag)
  block <- html_node(all_nodes, rank_tag) %>% html_node("h3>a")
  title <- html_text(block)
  theUrl <- html_attr(block,"href")
  df <- data.frame("title"=title,"URL"=theUrl) %>% rbind(df)
}

View(df)

