# 데이터 설명 : https://archive.ics.uci.edu/ml/datasets/spambase
datSpam <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data', sep=',', header=FALSE)
head(datSpam)
summary(datSpam)

dim(datSpam)
# spam : datSpam$V58 == 1
# normal : datSpam$V58 == 0

# 데이터 나누기

# 모형 적합

# 혼동 행렬

# 검증하기

# 모형의 종류
# 1. 의사결정나무
# 2. 배깅
# 3. 랜덤 포레스트 : 과적합은 늘어나지만, 변수 선택이 정확해서 분산도 늘어난다?
# 4. 인공신경망 : 



