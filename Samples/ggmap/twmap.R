library(ggplot2)
library(ggmap)
if(has_google_key()){
  twmap <- get_googlemap(center = c(lon=120.58,lat=23.58),zoom = 7, language = "zh-TW")
  ggmap(twmap)
  }else
  {
    cat("register_google(key =\"xxxxx....\") Firstly")
  }
