library(ggplot2)
diamonds

## 單變數：類別型
ggplot(data = diamonds, aes(x= cut)) + geom_bar(fill = "lightblue", colour="black")

## 雙變數(類別型): 
s <- ggplot(data = diamonds, aes(x= cut, fill = clarity))
s + geom_bar()

## 單變數：連續型
ggplot(data = diamonds, aes(x = price)) + geom_histogram()

## 散佈圖
# - 1.
ggplot(data = diamonds, aes(x=carat,y = price)) + geom_point()
# - 2.
ggplot(data = diamonds, aes(x=table,y = depth)) + geom_point()

## 折線圖
gdata <- head(diamonds)
s <- ggplot(data = gdata, aes(x=carat,y=price, group=cut)) 
s + geom_line(linetype="dashed", color="lightblue", size=1.2) + geom_point(color="red",size=3)

ggplot(data = diamonds, aes(x=x, y= price)) + geom_line()