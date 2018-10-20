install.packages("reshape")
library(dplyr)
library(reshape)
library(ggplot2)
crime_raw<- read.csv("crime.csv")
cctv_raw <- read.csv("cctv.csv")
cr <- crime_raw
cc <- cctv_raw

summary(cr)
str(cr)
head(cr,10)

summary(cc)
str(cc)
head(cc,10)
tail(cc)


cc <- rename(cc,자치구 = 기관명)

df <- left_join(cc,cr, by = "자치구")
df
df <- df%>%
  select(자치구,합계,소계,방범용)
summary(df$소계)
min(df$소계)
max(df$소계)
mean(df$소계)
sd(df$소계)

df <- rename(df,범죄발생 = 합계, 전체cctv = 소계,방범용cctv = 방범용)

df%>%
  arrange(범죄발생)%>%
  head(10)

df%>%
  arrange(desc(방범용cctv))%>%
  head(10)

intersect(df%>%
            arrange(범죄발생)%>%
            head(10),df%>%
            arrange(desc(방범용cctv))%>%
            head(10))


#CCTV가 많은 지역일 수록 범죄발생 빈도가 적은 경향을 보였다.
raw_df <- df
df$전체cctv <- as.factor(df$전체cctv)
df$방범용cctv <- as.factor(df$방범용cctv)
melt(df,measure.vars = c("범죄발생","전체cctv","방범용cctv"))


#보통cctv는 범죄 검거에 큰 도움이 된다 생각하지만 범죄 예방에도 큰 도움이 된다.