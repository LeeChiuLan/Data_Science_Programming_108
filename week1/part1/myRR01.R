# 第一週練習
library(ggplot2)
data()
iris

ggplot(data=iris, aes(x=Species, y=Sepal.Length))+geom_point()
