url0 <- 'http://www.stat.cmu.edu/~larry/=stat705/Lecture1.pdf'

for (i in c(1:19)) {
  fn <- paste0('Lecture', i, ".pdf")
  url <- paste0(url0, fn)
  download.file(url, destfile=fn, mode='wb')
}