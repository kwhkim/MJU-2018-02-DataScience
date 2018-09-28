TEXT 데이터 화일을 읽을 때 고려할 점.

1. TEXT Encoding
  - readr::guess_encoding() 하지만 확실하지 않다. 예) UTF-8-BOM
  - notepad++
2. 전체적인 형식
  1) 행이름을 포함하는가? header
  2) 열이름을 포함하는가? row.names
  3) 열 구분자(delimiter) sep
3. 데이터를 표기하는 방법
  1) 주석은 어떻게 구분하는가? comment.char
  2) 따옴표(quotation mark; 문자열 속에 열 구분자를 포함시켜야 할 경우를 생각해보자): quote
  3) 소수점 표기 방법(decimal seperator): dec
4. R에 저장하는 방법
  1) stringsAsFactors




  

  





dat = mtcars
dat$etc = "참고사항"
write.table(x=dat, file='dat_UTF8.csv', sep=',', fileEncoding = 'UTF-8')
read.table('dat_UTF8.csv', sep=',', fileEncoding = 'UTF-8')

write.table(x=dat, file='dat_CP949.csv', sep=',', fileEncoding = 'CP949')
read.table('dat_CP949.csv', sep=',', fileEncoding = 'CP949')

#read.csv('서울시 한강공원 이용객 현황 (2009_2013년).csv', row.names = NULL, 
#         fileEncoding = 'UTF-8-BOM')

dat <- read.csv('서울시 한강공원 이용객 현황 (2009_2013년).csv')
dat <- readLines('서울시 한강공원 이용객 현황 (2009_2013년).csv', n=10)

library(readr)
guess_encoding('서울시 한강공원 이용객 현황 (2009_2013년).csv')

dat <- read.csv('서울시 한강공원 이용객 현황 (2009_2013년).csv', fileEncoding = 'UTF-8')

#dat <- read.table('서울시 한강공원 이용객 현황 (2009_2013년).csv', fileEncoding = 'UTF-8')
#unlike https://cran.r-project.org/doc/manuals/r-release/R-data.pdf
#This does not work.

dat <- read.csv('서울시 한강공원 이용객 현황 (2009_2013년).csv', fileEncoding = 'UTF-8-BOM')
head(dat)

# BOM
# http://blog.wystan.net/2007/08/18/bom-byte-order-mark-problem
# https://code.i-harness.com/ko-kr/q/21ef0a

x <- readLines('서울시 한강공원 이용객 현황 (2009_2013년).csv', n=1)
#Encoding(x) <- "UTF-8-BOM"
x2 <- charToRaw(x)
x3 <- x2[4:length(x2)]
x4 <- rawToChar(x3)
Encoding(x4) <- "UTF-8"
x4

readr::guess_encoding(charToRaw(x))
stringi::stri_enc_detect(x)
stringi::stri_enc_detect2(x)



url = 'http://www.nber.org/data/population-birthplace-diversity/JoEG_BP_diversity_data.dta'
read.csv("http://www.nber.org/data/population-birthplace-diversity/JoEG_BP_diversity_data.csv", 
         sep=";", header=T, row.names=NULL)
readLines('http://www.nber.org/data/population-birthplace-diversity/JoEG_BP_diversity_data.dta')
foreign::read.dta('http://www.nber.org/data/population-birthplace-diversity/JoEG_BP_diversity_data.dta') 
haven::read_dta('http://www.nber.org/data/population-birthplace-diversity/JoEG_BP_diversity_data.dta')
readstata13::readstata13('http://www.nber.org/data/population-birthplace-diversity/JoEG_BP_diversity_data.dta')

foreign::write.dta()
> haven::read_dta();
> readstata13::readstata13();

library(readxl)
readxl::excel_sheets('서울시 한강공원 이용객 현황 (2009_2013년).xls')
readxl::read_excel('서울시 한강공원 이용객 현황 (2009_2013년).xls', sheet=1) 

readxl::excel_sheets('서울시 한강공원 이용객 현황 (2009_2013년).xlsx')
readxl::read_excel('서울시 한강공원 이용객 현황 (2009_2013년).xlsx', sheet=1) 


library(rjson)
dat <- fromJSON(file='서울시 한강공원 이용객 현황 (2009_2013년).json')
save(dat, file='서울시 한강공원 이용객 현황 (2009_2013년).rdata')  # load
fromJSON(file='서울시 한강공원 이용객 현황 (2009_2013년)_2.json')

# structure of dat
# - "DESCRIPTION" - "SPORTS_FCLTY", ...
# - "DATA" - "sm", "event_marathon", "etc", "bcycl", ...
#          - "sm", "event_marathon", "etc", "bcycl", ...
#          - "sm", "event_marathon", "etc", "bcycl", ...
do.call(rbind, dat$DATA)

#file.show('서울시 한강공원 이용객 현황 (2009_2013년)_2.json', encoding = 'UTF-8-BOM')
dat <- read.csv('서울특별시 공공자전거 대여소별 이용정보(월간)_2017_1_12.csv')

dat <- read.csv('서울특별시 공공자전거 대여소별 이용정보(월간)_2017_1_12.csv', quote="\'")
head(dat)

dat <- read.table('서울특별시 공공자전거 대여소별 이용정보(월간)_2017_1_12.csv',
                  header = TRUE, row.names=NULL, fileEncoding = 'CP949'
                  )



# ICU(International Components for Unicode)


l10n_info()

건당 사용료가 다른 문자메세지 정보, 월별 사용량

이름  전화번호   가격   메세지   날짜   시간   월별 사용량


이름 : BTS
전화번호 : 010-4342-5842 
지난달 총 사용료 : 38000원
메세지 : 안녕? 날씨 좋다. "가즈아!"라고 말하고 싶다.
가격 : 30원 

이름 : 트와이스
전화번호 : 010-5821-4433
지난달 총 사용료 : 58000원
메세지 : 달빛 아래 춤추자! '너무너무너무'라고 노래 부를래.
가격 : 10원 

이름 : 케이티 킴
전화번호 : 010-5532-4432
지난달 총 사용료 : 31000원
메세지 : Memorable! 
가격 : <NA>
  
  dat <- data.frame(name = c("BTS", "트와이스", "케이티 킴"),
                    phone = c('010-4342-5842', '010-5821-4433', '010-5532-4432'),
                    usageLastMonth = c(38000, 58000,31000),
                    message = c('안녕? 날씨 좋다. "가즈아!"라고 말하고 싶다.',
                                '달빛 아래 춤추자! \'너무너무너무\'라고 노래 부를래.',
                                'Memorable'),
                    price = c(30, 10, NA))


  



# 과제 관련
# 출처를 확실히 포함할 것.

enc <- iconvlist()
enc[grep("UTF", enc)]

x <- '\xf9'
x <- '\x00\xd8'
x <- '\u00d8'
x <- '\u00b6'
#x <- '\N{CR}'
x <- '\v'
cat(x)
Encoding(x)
Encoding(x) <- 'latin-9'
cat(x)

library(dplyr)
addmargins(UCBAdmissions)
dat <- as.data.frame(UCBAdmissions)
dat %>% group_by(Gender) %>% summarise(sumFreq=sum(Freq))

dat %>% group_by(Dept, Gender) %>% summarise(n = n()) %>%
  mutate(freq = n / sum(n))

dat %>% group_by(Gender, Admit) %>% summarise(sumFreq=sum(Freq)) %>% ungroup %>%
  mutate(sum = sum(sumFreq)) %>% group_by(Gender, Admit) %>% mutate(propFreq = sumFreq/sum)

dat %>% group_by(Dept, Gender) %>% summarise(sumFreq=sum(Freq)) %>% 
  group_by(Dept) %>% 
  mutate(sum = sum(sumFreq)) %>% group_by(Dept, Gender) %>% mutate(propFreq = sumFreq/sum) %>%
  group_by(Gender) %>% summarise(avgProp = mean(propFreq))








* data in R, data(), data( , package='  ')

* write.table(x, file= , sep=' '), read.table(file= , sep=' ')

```{r, error=TRUE}
dat = mtcars
dat$etc = "참고사항"
write.table(x=dat, file='dat.csv', sep=',', fileEncoding = 'UTF-8')
read.table('dat.csv', sep=',')
library(readxl)
readxl::read_excel('서울시 한강공원 이용객 현황 (2009_2013년).xls', sheet=1) 
```



* write.csv, read.csv( , row.names=1)

* readr::read_delim(file= , delim= )

* data.table::fread(input= , file= , sep= )

* readxl::read_excel(path, sheet= )  # readxl::excel_sheets()

## Stata .dta

> foreign::read.dta(); foreign::write.dta()
> haven::read_dta();
> readstata13::readstata13();

## Etc

You can code python here!
  ```{python python.reticulate=FALSE}
for i in range(10):
  print(i)
```


명지대 응용데이터사이언스(2018년 2학기)
명지대학교 응용데이터사이언스 연계전공 응용데이터사이언스 과목

Data Science, Data Mining, Statistical Learning, Machine Learning, Statistics.
데이터 과학(데이터 사이언스), 데이터마이닝, 통계학습, 기계학습, 통계학.

Statistical Modeling.

Descriptive, Inferetial Statistics.
기술 통계, 추론 통계.
센서스 데이터 vs. 표본 데이터

Predictive Analytics
예측분석.

데이터를 분석하여 새로운 가치를 창출한다!(인사이트, 예측, ...)



survey sampling.
조사 샘플링

sample survey.
표본 조사.

sampling survey
표집 조사.

[통계용어](https://www.kss.or.kr/bbs/board.php?bo_table=psd_sec)

# Importing Big Data




