install.packages("magrittr")
install.packages("dplyr")
library(magrittr)
library(dplyr)
library(ggplot2)
data(diamonds,package = 'ggplot2')
head(diamonds)
head(diamonds)%>% dim()
dim(diamonds)
result <- diamonds%>% head(.,n=12)%>% dim(.)
result
diamonds%>% head(.,n=12)
diamonds%>% .$price
mtcars%>% slice(c(1:3,5,6))
mtcars%>%
  filter(carb ==1)


mtcars$name = rownames(mtcars);rownames(mtcars)=NULL
#gather

name gender year2011 year2012 year2013
ChangSik Park M 74.69 84.99 91.73
HaeHee Song F NA NA 75.74
InHo Kim M 88.24 NA 101.85
YeoJin Lee F 88.77 96.45 NA

View(mpg)
mpg
ggplot(data = mpg,aes(x=drv, y= cty))+geom_col
table(Cars93$Type)


install.packages("MASS")
library(MASS)
table(Cars93$Type)
