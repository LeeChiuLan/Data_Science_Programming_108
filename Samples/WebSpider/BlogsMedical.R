library(dplyr)
library(xml2)
library(rvest)
library(wordcloud)

full_page <- read_html("https://www.pixnet.net/blog/articles/category/33")
full_page
test <- html_node(full_page, ".rank-1")
cat(test)

all_tag_nodes <- c(test)
all_tag_nodes
rank <- 10   # get the top-10
for(i in 2:rank){
  rank_tag <- paste0(".rank-",i)
  cat(rank_tag)
  nodes <- html_node(full_page, rank_tag)
  all_tag_nodes <- add_row(all_tag_nodes, nodes)
}

all_tag_nodes