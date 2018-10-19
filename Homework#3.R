#1. 관심 자료에 대해 summary를 하고, 집단별로 나눠서 min, max,mean, sd를 구해보세요.
library(readxl)
library(dplyr)

df <- read_excel("baseball_project1.xlsx")
head(df)
summary(df)

df$grade <- ifelse(df$AVG >= 0.300, "Above.300",
                    ifelse(df$AVG >= 0.250, "Above.250", "Below.250"))

A <- df %>% filter(grade == 'Above.300')
mean(A$H)
max(A$H)
min(A$H)
sd(A$H)

B <- df %>% filter(grade == "Above.250")
mean(B$H)
max(B$H)
min(B$H)
sd(B$H)

C <- df %>% filter(grade == "Below.250")
mean(C$H)
max(C$H)
min(C$H)
sd(C$H)

#2집단별로 함수를 적용하는 방법을 활용하여 자료에서 새로운 사실을 발견해 보세요.

summary(A)
summary(B)
summary(C)

boxplot(A$H, A$RBI)
boxplot(B$H, B$RBI)
boxplot(C$H, C$RBI)

#3 두개 이상의 자료를 통합(join)해서 하나의 큰 자료를 만들어 보세요.
df1 <- data.frame(
  id = c(1,2,3,4,5),
  name = c("김현수", "양의지", "이정후", "박병호", "안치홍"),
  homerun = c(20, 23, 6, 43, 24))

df2 <- data.frame(
  name = c("김현수", "양의지", "이정후", "박병호", "안치홍"),
  RBI = c(101, 77, 57, 112, 118),
  TEAM = c("LG", "DOSAN", "NEXEN", "NEXEN", "KIA"))

df3 <- data.frame(
  RBI = c(101, 77, 57, 112, 118),
  H = c(164, 157, 163, 138, 169),
  R = c(95, 84, 81, 88, 88))

inner_join(df1, df2, by="name")
full_join(df1, df2, by="name")
left_join(df1, df2, by="name")
right_join(df1,df2, by="name")

inner_join(df2, df3, by="RBI")
full_join(df2, df3, by="RBI")
left_join(df2, df3, by="RBI")
right_join(df2,df3, by="RBI")

semi_join(df1, df2)

#4 이 자료를 가로형, 세로형으로 형태를 바꿔 보세요.
library(dplyr)
library(tidyr)

df4 <- t(df1)
View(df4)


#5 위의 방법을 활용하여 자료에서 새로운 사실을 발견해 보세요.
#실제로 안타를 더 많이 쳤으면 타점을 더 많이 기록했다라는 것을 확인할 수 있었습니다.