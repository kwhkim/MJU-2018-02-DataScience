#60150339 김세영 과제3 

#법무부_사회통합 및 결혼이민자 데이터
#https://www.data.go.kr/dataset/3069956/fileData.do


data <- read.csv("09_김세영/지역별_결혼이민자_현황_자료_2016년_.csv")
#list.files("./09_김세영/")
#list.files()
View(data)
head(data)
dim(data)
str(data)
summary(data)

# 외국인거주비자(F-2), 영주(F-5), 결혼이민(F-6)
#1. F-2비자는 영주자격을 부여받기 위하여 국내에 장기체류 하려는 사람이 발급받는 비자 (기한 : 3년)
# 그 중 F-2-1은 국민과 결혼한 사람에게 발급됨 
#2. F-5기한이 없는 영주비자 - F-6자격으로 2년이상 대한민국에 체류하면 영주신청 가능 
#3. F-6비자는 대한민국 국민과 결혼한 외국인이 가질 수 있는 체류자격(기한 : 2년)
# 순서 : F-2 > F-5 OR F-6 > F5

#### 데이터 전처리

data1 <- rename(data, sido = 시도,
                sigungu = 시군구,
                gender = 성별,
                live = 거주.F.2.,
                l_spouse = 국민배우자.F.2.1.,
                settle = 영주.F.5.,
                s_spouse = 국민배우자.F.5.2.,
                marry_immigrant = 결혼이민.F.6.,
                m_spouse = 국민배우자.F.6.1.,
                rearingChildren = 자녀양육.F.6.2.,
                severMarry = 혼인단절.F.6.3.)
str(data1)
# 외국인거주비자와 외국인거주 국민배우자, 자녀양육 결혼이민 3개의 변수만 int형식으로 데이터가 불러와졌다.
# 이번 과제에서는 이 3개의 변수만 다루기로 한다.

# 계, 소계 제거하기 
data2 <- data1 %>% filter(gender != '계', sigungu != '소계')

# 총계 제거하기 
data3 <- data2 %>% select(-총계)

#결측치 존재X 
table(is.na(data3))

#형식 : 데이터프레임 
class(data3)

View(data3)

#### 데이터 분석 

### 1. 관심 자료에 대해 summary를 하고, 집단별로 나눠서 min, max, mean, sd를 구해 보세요.

summary(data3)

#시도별 외국인거주 현황 최소값 
data3 %>% 
  group_by(sido) %>% 
  summarise(live_min = min(live)) %>% 
  arrange(live_min)

#### 채점사항: 여기서 최소값은 시도별 가장 작은 수의 시군구에 해당하는 값입니다.


#시도별 외국인거주 현황 최대값 
data3 %>% 
  group_by(sido) %>% 
  summarise(live_max = max(live)) %>% 
  arrange(desc(live_max))
#대구광역시가 가장 많고, 그 다음으로 경기도, 대전광역시 순이다.

#성별 외국인거주 현황 평균 
data3 %>% 
  group_by(gender) %>% 
  summarise(mean(live))
#여자가 월등히 많다.

#시도별, 시군구별 외국인거주 현황 표준편차
data3 %>% 
  group_by(sido, sigungu) %>% 
  summarise(sd_live = sd(live)) %>% 
  arrange(desc(sd_live))
#대구광역시 달서구가 표준편차가 가장 크다.

data3 %>% filter(sido=="대구광역시", sigungu=="달서구")
#### 채점사항: 여기서 표준편차는 남자와 여자 거주자에 대해서 구한 표준편차입니다.


### 2. 집단별로 함수를 적용하는 방법을 활용하여 자료에서 새로운 사실을 발견해 보세요.

#시도별 외국인 거주 합계와 자녀양육 결혼이주 합계 비교하기
data4 <- data3 %>% 
  group_by(sido, gender) %>% 
  select(live) %>% 
  summarise(sum_live = sum(live)) %>% 
  arrange(desc(sum_live))

data5 <- data3 %>% 
  group_by(sido, gender) %>% 
  select(rearingChildren) %>% 
  summarise(sum_rearChild = sum(rearingChildren)) %>% 
  arrange(desc(sum_rearChild))

# 같이 표현하기 
data3 %>% 
  group_by(sido) %>% 
  select(live, rearingChildren) %>% 
  summarise(sum_live = sum(live),
            sum_rearChild = sum(rearingChildren)) %>% 
  arrange(desc(sum_live))


#그래프로 표현하기 

library(ggplot2)

ggplot(data = data4, aes(x=reorder(sido, sum_live), y=sum_live, fill = gender)) + geom_col(position = "dodge") + coord_flip() +
  scale_fill_manual(values=c("blue", "red"))

ggplot(data = data5, aes(x=reorder(sido, sum_rearChild), y=sum_rearChild, fill = gender)) + geom_col(position = "dodge") + coord_flip()

### 3. 두 개 이상의 자료를 통합(join)해서 하나의 큰 자료를 만들어 보세요.
#2012년의 자료와 2016년의 지역별 결혼이민자 현황 자료를 통합하려 한다.

data12 <- read.csv("09_김세영/지역별_결혼이민자_현황_2012년_.csv")
str(data2012)
View(data2012)

#원 자료에 변수명이 2번 나타나기 때문에 slice를 사용한다. 
data12_1 <- data12 %>% 
              slice(3:273)

str(data12_1)
View(data12_1)

data12_2 <- rename(data12_1, sido = 지역.Region ,
                    sigungu = 성별.Sex,
                    male = 남자,
                   female = 여자)

str(data12_2)

# 총계 제거하기 
data12_3 <- data12_2 %>% select(-총계)

# 소계 제거하기 
data12_4 <- data12_3 %>% filter(sigungu != '소계')

View(data12_4)



dim(data3)
dim(data12_4)

table(data3$sido)
table(data12_4$sido)

#두 DF이 행과 열의 개수가 다르다...
#합치기 불가능
left_join(data3, data12_4, by = "sido")



########## 성별로 자료 나눈뒤 join해보기


data_m <- data3 %>% 
  filter(gender == '남')

data_fem <- data3 %>%
  filter(gender =='여')

left_join(data_m, data_fem, by = "sido")
right_join(data_m, data_fem, by = "sido")

### 채점 사항: join의 의미를 조금 잘못 이해했습니다. join은 한 행의 여러 컬럼이 서로 다른 data.frame으로 존재할 때 이들을
### 합치는 것입니다. 

### 4. 이 자료를 가로형, 세로형으로 형태를 바꿔 보세요.

library(tidyr)

data3$num = rownames(data3)
rownames(data3) = NULL
rownames(data3)
data4 <- data3 %>% 
  select(sido, sigungu, gender, live, rearingChildren)
head(data4)

data5 <- data4 %>%
  gather(key='feature', value='value', live, rearingChildren)
head(data5)

data6 <- data5 %>% spread(key='feature', value='value')
head(data6)

all.equal(data5, data6)


### 5. 위의 방법을 활용하여 자료에서 새로운 사실을 발견해 보세요.


