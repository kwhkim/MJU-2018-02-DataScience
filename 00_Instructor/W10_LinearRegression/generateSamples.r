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
}