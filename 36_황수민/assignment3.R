library(foreign)
data <- read.spss(file = "한국복지패널가구원.sav", to.data.frame=TRUE)

# p1001_15 -> 고용보험급여 수급여부
# p1002_12 -> 구직 기간(개월)

library(dplyr)
data <- data %>% 
  rename(pension=p1001_15,
         job=p1002_12)
summary(data$pension)    #범주형변수
summary(data$job)
data <- data %>% 
  mutate(Pension=ifelse(pension==1,"Y","N"))   # 고용보험급여 수급여부에 따라 그룹나눔


data %>% 
  group_by(Pension) %>% 
  summarise(min=min(job, na.rm = T))   

data %>% 
  group_by(Pension) %>% 
  summarise(max=max(job, na.rm = T))  

data %>% 
  group_by(Pension) %>% 
  summarise(mean=mean(job, na.rm = T))  

data %>% 
  group_by(Pension) %>% 
  summarise(sd=sd(job, na.rm = T))

#새로 알게된 점: 연금을 받는 사람들의 평균 구직기간이 더 길다.

data2 <- data %>% 
  rename(labor=p1002_4aq1) %>%
  filter(labor==1,4) %>% 
  select(labor, job)

data3 <- data %>% 
  rename(labor=p1002_4aq1,
         work=p1002_6) %>%
  filter(labor==1,4) %>% 
  select(labor, work)

left_join(data2, data3, by="labor")

library(tidyr)
data %>% gather(key="p1002_1", value = "value" )
