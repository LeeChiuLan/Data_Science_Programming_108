# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

hello <- function() {
  print("Hello, world!")
}

print(sample(1:3))
print(sample(1:3, size=3, replace=FALSE))  # same as previous line
print(sample(1:2, size=10, prob=c(1,3), replace=TRUE))
barplot(table(sample(1:3, size=1000, replace=TRUE, prob=c(.30,.60,.10))))
