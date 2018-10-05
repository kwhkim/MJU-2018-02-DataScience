#국가 통계 포털의 성연령별 취업 데이터
#년도별 취업인원의 변화 파악
#여성, 남성의 취업 인원 비교
#연령대별 취업 인원 비교

#성연령별 취업자 데이터
#-> 국가 통계 포털 (통계청) KOSIS

#데이터 출처:http://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_1DA7024S&vw_cd=MT_TM2_TITLE&list_id=101_B18013&scrId=&seqNo=&lang_mode=ko&obj_var_id=&itm_id=&conn_path=MT_TM2_TITLE&path=%252FeasyViewStatis%252FcustomStatisIndex.do

#http://kosis.kr/easyViewStatis/customStatisIndex.do?vwcd=MT_TM2_TITLE&menuId=M_03_02

#통계 설명 자료: https://meta.narastat.kr/metasvc/index.do?orgId=101&confmNo=101004&kosisYn=Y


# 1. 관심 자료에 대해 summary를 하고, 집단별로 나눠서 min, max, mean, sd를 구해 보세요.
# 2. 집단별로 함수를 적용하는 방법을 활용하여 자료에서 새로운 사실을 발견해 보세요.
# 3. 두 개 이상의 자료를 통합(join)해서 하나의 큰 자료를 만들어 보세요.
# 4. 이 자료를 가로형, 세로형으로 형태를 바꿔 보세요.
# 5. 위의 방법을 활용하여 자료에서 새로운 사실을 발견해 보세요.


library(readxl)

man <- read_excel("man_month_2010-2017.xlsx")
#단위 천명


head(man)
View(man)

summary(man)

#칼럼 이름 바꾸기
library(dplyr)
man1 <- rename(man, year = 연령계층별,
                    age1 =`15 - 19세`,
                    age2 = `20 - 29세`,
                    age3 = `30 - 39세`,
                    age4 = `40 - 49세`,
                    age5 = `50 - 59세`,
                    age6 = `60세이상`)


head(man1)

min(man1$age1)
max(man1$age1)
mean(man1$age1)
sd(man1$age1) #표준편차

summary(man1)

#연도별 월별 데이터 추가
man1$year <- c(rep(2010:2017,times=1,each=12)) 

man1$month <- c(rep(1:12, times=8, each=1))
head(man1)



#각 연도별 나이대의 취업 평균 
meanYear <- man1 %>% 
  group_by(year) %>% 
  summarise(a10 = mean(age1),
            a20 = mean(age2),
            a30 = mean(age3),
            a40 = mean(age4),
            a50 = mean(age5),
            a60 = mean(age6))

meanYear



# 각 연도별 나이대의 취업자 수 총합
sumYear <- man1 %>% 
  group_by(year) %>% 
  summarise(a10 = sum(age1),
            a20 = sum(age2),
            a30 = sum(age3),
            a40 = sum(age4),
            a50 = sum(age5),
            a60 = sum(age6))

sumYear



#새로운 데이터 불러오기 
woman <- read_excel("woman_month_2010-2017.xlsx")
#단위 천명


head(woman)
View(woman)

summary(woman)

#칼럼 이름 바꾸기
library(dplyr)
woman1 <- rename(woman, year = 연령계층별,
               age1 =`15 - 19세`,
               age2 = `20 - 29세`,
               age3 = `30 - 39세`,
               age4 = `40 - 49세`,
               age5 = `50 - 59세`,
               age6 = `60세이상`)


head(woman1)

min(woman1$age1)
max(woman1$age1)
mean(woman1$age1)
sd(woman1$age1) #표준편차

summary(woman1)

#연도별 월별 데이터 추가
woman1$year <- c(rep(2010:2017,times=1,each=12)) 

woman1$month <- c(rep(1:12, times=8, each=1))
head(woman1)




#데이터 합치기
str(man1)
man2 <- man1

man2$gender <- c(rep("man", times=96))
head(man2)

woman2 <- woman1
woman2$gender <- c(rep("woman", times=96))
head(woman2)



library(dplyr)
inner_join(woman2 , man2 , by="year")
full_join(woman2 , man2 , by="year")
a <- left_join(woman2 , man2 , by="year")
b <- right_join(woman2 , man2 , by="year")

View(a)
View(b)

inner_join(woman2 , man2 , by="month")
full_join(woman2 , man2 , by="month")
left_join(woman2 , man2 , by="month")
right_join(woman2 , man2 , by="month")





semi_join(woman2 , man2 , by="year")
anti_join(woman1 , man1 , by="year")


semi_join(woman1 , man1 , by="month")
anti_join(woman1 , man1 , by="month")



#가로형 세로형 바꾸기


job <- rbind(man2,woman2)

View(job)



manMeanYear <- man1 %>% 
  group_by(year) %>% 
  summarise(a10 = mean(age1),
            a20 = mean(age2),
            a30 = mean(age3),
            a40 = mean(age4),
            a50 = mean(age5),
            a60 = mean(age6))

manMeanYear


womanMeanYear <- woman1 %>% 
  group_by(year) %>% 
  summarise(a10 = mean(age1),
            a20 = mean(age2),
            a30 = mean(age3),
            a40 = mean(age4),
            a50 = mean(age5),
            a60 = mean(age6))

womanMeanYear

#새로운 사실

# 10대, 20대의 평균 취업자 수는 모든 연도에서 여성이 남성보다 높음
# 30대, 40대, 50대, 60대 이상의 평균 취업자 수는 모든 연도에서 남성이 여성보다 높음


