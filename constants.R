library(lubridate)

## CONSTANTS!
# first day of the current semester
currentSemester <-  as.Date("2018-08-20")

# Checkup dates (~2 week intervals with the end date being the last day of testing)
c1 <- interval('2018-08-24', '2018-09-07')
c2 <- interval(int_end(c1), '2018-09-21')
c3 <- interval(int_end(c2), '2018-10-05')
c4 <- interval(int_end(c3), '2018-10-19')
c5 <- interval(int_end(c4), '2018-11-09')
c6 <- interval(int_end(c5),'2018-11-28')
# checkup starts (first day of checkup testing)
cs1 <- "2018-09-03"
cs2 <- "2018-09-17"
cs3 <- "2018-10-01"
cs4 <- "2018-10-15"
cs5 <- "2018-11-05"
cs6 <- "2018-11-26"

# exam dates (2 week intervals with the end date being the last day of testing)
# e = excel & w = word
e1 <- interval('2018-09-14', '2018-09-28')
e2 <- interval('2018-10-12', '2018-10-26')
w1 <- interval('2018-11-02', '2018-11-16')
# exam starts (first day of exam testing)
es1 <- "2018-09-25"
es2 <- "2018-10-25"
ws1 <- "2018-11-12"