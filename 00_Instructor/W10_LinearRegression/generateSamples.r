generateSamples = function(sampleSize) {
  gender = sample(c(1,0), sampleSize, replace=T)
  gender2 = ifelse(gender==1, "M", "F")
  logIncome <- income <- c(NA, sampleSize)
  for (i in 1:sampleSize) {
    income[i] = ifelse(gender[i]==1, rnorm(1,70,10), rnorm(1, 60,5))
    logIncome[i] = log(income[i])
  }
  happiness = 2.5 + 0.02*logIncome + rnorm(sampleSize, 0, 1)
  dat <<- data.frame(happiness, logIncome, gender=gender2)
}r


print(getwd())
dat <- read.csv(file='helen/Desktop/applied data/MJU-2018-02-DataScience/00_Instructor/W10_LinearRegression/LR_weight_n100.csv', header=T, row.names=1)
t.test(dat$height)
t.test(dat$weight)
