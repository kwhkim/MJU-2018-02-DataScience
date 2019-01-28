factor(c("West","East","Middle","East"), levels=c("East","Middle","West"))

ordered(c("West","East","Middle","East"), levels=c("East","Middle","West"))

heights = c("Low", "Middle", "High", "Middle", "High")

strDate <- c("05-09-16", "04-10-17", "11-03-18")

date <- mdy("06-01-2018")
day(date) = day(date)-1
day(date)
date

birthdate <- c("1994-01-01", "1995-05-03", "1996-03-02")
birthdate <- as.Date(birthdate)
birthdate <- ymd(birthdate)

deathdate <- c("2100-01-01", "2080-03-02", "2099-04-04")
deathdate <- as.Date(deathdate)

difftime(birthdate, deathdate)
difftime(deathdate, birthdate, units="years")


