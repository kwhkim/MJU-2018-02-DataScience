#dat = data.frame(id=NA_character_, gender=NA_character_, name=NA_character_, 
#                 year=NA_character_, height=NA_character_, weight=NA_character_,
#                 stringsAsFactors = F)
#dat <- edit(dat)
                 
dat <- structure(list(id = c("144", "132", "224", "231", "332", "335", "423", "432"), 
                      gender = c("M", "F", "M", "F", "M", "F", "M", "F"), 
                      name = c("김민철", "김민정", "한우재", "정채은", "한상기", "이민정", "이두근", "하희주"), 
                      year = c("1", "1", "2", "2", "3", "3", "4", "4"), 
                      height = c("180", "167", "177", "156", "167", "160", "181", "155"), 
                      weight = c("82", "77", "77", "63", "60", "55", "80", "44"), 
                      salary = c("4,000,000", "3,500,000", "6,000,000", "100,000,000", "3,200,000", "1,000,000", "5,000,000", "2,200,000")),
                 row.names = c(NA, 8L), class = "data.frame")                                                                                                             

str(dat)
summary(dat)

# 01. id, year, height, weight는 수치형으로,
#     gender는 범주형(factor)으로,
#     salary도 수치형으로 변환하세요.

# SOLUTION
dat %>% mutate(id = as.numeric(id),
               year = as.numeric(year),
               height = as.numeric(height),
               weight = as.numeric(weight),
               gender = as.factor(gender),
               salary = as.numeric(gsub(",","",salary))) -> dat2
summary(dat2)

# 01+. salary의 경우 ","를 ""로 변환한 후 수치형으로 변환해야 합니다.

# 02. 다음의 datHeight와 datWeight는 동일한 사람의 키와 체중이 들어 있습니다. 
#     어떤 사람의 키와 체중을 하나의 데이터 프레임으로 만드세요
library(dplyr)
datHeight <- dat %>% select(id, gender, name, year, height) %>% arrange(height)
datWeight <- dat %>% select(id, gender, name, year, weight) %>% arrange(weight)

#SOLUTION
dat3 <- left_join(datHeight, datWeight, by='name')
dat4 <- left_join(datHeight, datWeight, by=c('id', 'gender', 'name', 'year'))

# 03. 다음의 plot은 사람마다 다른 키, 성별에 따른 키를 보여줍니다.
#     키와 체중을 하나의 plot에 그려보세요.
library(ggplot2)
ggplot(dat, aes(x=name, y=height)) + geom_point()
ggplot(dat, aes(x=name, y=weight)) + geom_point()

#SOLUTION
ggplot(dat) + geom_point(aes(x=name, y=height), col='blue') + 
  geom_point(aes(x=name, y=height), col='red')

library(tidyr)
dat %>% gather("key", "value", height:weight) -> datLong
datLong %>% ggplot(aes(x=name, y=value, col=key)) + geom_point() 

ggplot(dat, aes(x=height, y=weight, col=name)) + geom_point()
ggplot(dat, aes(x=height, y=weight, col=gender)) + geom_point()
ggplot(dat, aes(x=height, y=weight, col=gender)) + geom_point() + geom_label(aes(label=name), nudge_x=0.4)

# 참고: https://rpubs.com/MarkusLoew/226759

# 04. 다음의 data를 활용하여 사람마다 키의 변천을 그래프로 나타내세요.
datHeight2017to2018 <- 
  structure(list(id = c("144", "132", "224", "231", "332", "335", "423", "432"),
                 gender = c("M", "F", "M", "F", "M", "F", "M", "F"), 
                 name = c("김민철", "김민정", "한우재","정채은", "한상기", "이민정", "이두근", "하희주"), 
                 y2017 = c("170", "164", "177", "155", "167", "160", "179", "155"), 
                 y2018 = c("178", "167", "177", "156", "167", "161", "181", "155")), 
            row.names = c(NA, 8L), class = "data.frame", 
            .Names = c("id", "gender", "name", "y2017", "y2018"))

datHeight2019to2020 <- 
  structure(list(id = c("144", "132", "224", "231", "332", "335", "423", "432"), 
                 gender = c("M", "F", "M", "F", "M", "F", "M", "F"), 
                 name = c("김민철", "김민정", "한우재","정채은", "한상기", "이민정", "이두근", "하희주"), 
                 y2019 = c("182", "170", "178", "157", "170", "166", "183", "156"), 
                 y2020 = c("182", "174", "179", "157", "178","168", "183", "156")), 
            row.names = c(NA, 8L), class = "data.frame", 
            .Names = c("id", "gender", "name", "y2019", "y2020"))

# SOLUTION
dat <- 
  left_join(datHeight2017to2018, datHeight2019to2020, by=c('id', 'name', 'gender', 'name'))

dat2 <- 
  dat %>% mutate(id = as.numeric(id),
                 gender = as.factor(gender),
                 y2017 = as.numeric(y2017),
                 y2018 = as.numeric(y2018),
                 y2019 = as.numeric(y2019),
                 y2020 = as.numeric(y2020))

dat2Long <- dat2 %>% gather("key", "value", y2017:y2020)

dat2Long <- rename(dat2Long, year = key, height = value)

dat2Long <- dat2Long %>% mutate(year = gsub("y", "", year))
dat2Long <- dat2Long %>% mutate(year = as.numeric(year))

ggplot(dat2Long, aes(x=year, y=height, col=name)) + 
  geom_point() + geom_line()  

ggplot(dat2Long, aes(x=year, y=height, col=name)) + 
  geom_point(position=position_dodge(width=0.1)) + geom_line()  

ggplot(dat2Long, aes(x=year, y=height, col=gender)) + 
  geom_point() + geom_line()  

ggplot(dat2Long, aes(x=year, y=height, col=gender, group=name)) + 
  geom_point() + geom_line()  

ggplot(dat2Long, aes(x=year, y=height, col=gender, group=name)) + 
  geom_point() + geom_line() + geom_label(aes(label=name), nudge_x=0.2)

ggplot(dat2Long, aes(x=year, y=height, col=gender, group=name)) + 
  geom_point() + geom_line() + geom_label(aes(label=name), nudge_y=-1)

ggplot(dat2Long, aes(x=year, y=height, col=gender, group=name)) + 
  geom_point() + geom_line() + 
  geom_label(data=dat2Long %>% filter(year==2017), aes(label=name), nudge_y=-1)
