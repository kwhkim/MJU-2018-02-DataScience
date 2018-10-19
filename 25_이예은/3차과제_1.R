library(dplyr)
library(ggplot2)





####데이터 불러오기####
##서울시 노인의 선호 여가활동
leisure_prefer <- read.csv(file='report.txt-5.csv',sep="\t")
head(leisure_prefer)
##서울시 노인 참가활동 통계
leisure_participation <- read.csv(file='report.txt-6.csv',sep="\t")
head(leisure_participation)


##(1). summary /집단별 `min`, `max`, `mean`, `sd`

summary(leisure_prefer)

leisure_prefer_senior<-leisure_prefer %>%
  filter(분류=="만65세 이상") 

leisure_prefer_senior

leisure_prefer_senior <- t(leisure_prefer_senior)
leisure_prefer_senior

colnames(leisure_prefer_senior) <- c("ratio")

category<-c("기간","대분류","분류","운동,건강 프로그램","노래,오락 프로그램","새로운 지식교육 교양 프로그램","수입과 연결된 직업관련 프로그램","여행, 관광, 등산, 낚시 프로그램","사회봉사활동","사교 프로그램","전통문화 프로그램","기타","없음")
leisure_prefer_senior<-cbind(category,leisure_prefer_senior)
leisure_prefer_senior<-as.data.frame(leisure_prefer_senior)

leisure_prefer_senior

mean(leisure_prefer_senior$ratio)

class(leisure_prefer_senior$ratio)

dat1<-leisure_prefer_senior %>%
  filter(category!='기간' & category!='대분류' & category!='분류')
dat1
mean(dat1$ratio)


dat1$ratio<-as.integer(dat1$ratio)

dat1$ratio

mean(dat1$ratio)
max(dat1$ratio)
min(dat1$ratio)

dat2<-as.integer(dat1$ratio)


##

View(leisure_participation)
summary(leisure_participation)

leisure_participation_senior<-leisure_participation %>%
  filter(분류=="만65세 이상") 

leisure_participation_senior

leisure_prefer_senior <- t(leisure_prefer_senior)
leisure_prefer_senior

colnames(leisure_prefer_senior) <- c("ratio")

category<-c("기간","대분류","분류","운동,건강 프로그램","노래,오락 프로그램","새로운 지식교육 교양 프로그램","수입과 연결된 직업관련 프로그램","여행, 관광, 등산, 낚시 프로그램","사회봉사활동","사교 프로그램","전통문화 프로그램","기타","없음")
leisure_prefer_senior<-cbind(category,leisure_prefer_senior)
leisure_prefer_senior<-as.data.frame(leisure_prefer_senior)

leisure_prefer_senior
class(leisure_prefer_senior$ratio)

mode(leisure_prefer_senior$ratio)<-"numeric"

dat1<-leisure_prefer_senior %>%
  filter(category!='기간' & category!='대분류' & category!='분류') %>%
  arrange(desc(ratio))

mean(leisure_prefer_senior$ratio)

dat1

dat2<-as.integer(dat1$ratio)


