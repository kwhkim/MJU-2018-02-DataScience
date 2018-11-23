  url0 <- '00_Instructor/W08_Rprogramming/control_and_functions.pdf'


  
for (i in c(3, 5, 7)) {
  print(i)
}

i=1
repeat {
  print(i)
  if(i==10) break
  i=i+1
}

for (i in c(1:19)) {
  fn <- paste0('Lecture', i, ".pdf")
  url <- paste0(url0, fn)
  download.file(url, destfile=fn, mode='wb')
} 

download.file('http://www.stat.cmu.edu/~larry/=stat705/Lecture1.pdf',
              destfile = 'Lecture01.pdf',
              mode = 'wb')

s2 <- ifelse(s=="here",
             "Myoungji Univ.", # if s=="here"
             "Somewhere else than Myoungji Univ") # if s!="here"

x = 1
x <- "two"
 switch(x,
        one = 1,
        two = 2,
        3)

sum_a_to_b = function(a, b) {
  s=0
  for(i in a:b) {
    s = s+i
  }
  return(s)
}

a <- sum_one_to_n(n=5)
b <- sum_one_to_n(n=10)
c <- sum_one_to_n(n=15)

sum_a_to_b(a=3, b=7)

