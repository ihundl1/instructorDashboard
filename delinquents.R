library(tidyverse)

source('connection.R')
source('constants.R ')
# Choose an exam!
# 1 = exam 1, 2 = exam 2, 3 = exam 3
exam <- 3

if (exam == 1) {
  startDate <- currentSemester
  endDate <- es1
  chunks <- exam1chunks
  name <- 'exam1delinquents.csv'
} else if (exam == 2) {
  startDate <- int_end(e1)
  endDate <- es2
  chunks <- exam2chunks
  name <- 'exam2delinquents.csv'
} else {
  startDate <- int_end(e2)
  endDate <- ws1
  chunks <- exam3chunks
  name <- 'exam3delinquents.csv'
}


source('constants.R')

# import data
roster <- tbl(conDatasource, 'student_user')
attendance <- tbl(conDatasource, 'attendance')
class <- tbl(conDatasource, 'attevent')
chunk <- tbl(conDatasource, 'exchunk')
submission <- tbl(conDatasource, 'exsubmission')

# create dataframes for all students, all submissions, all attendance
subs <- submission %>% filter(pawsId != "") %>% left_join(chunk, by = c('label' = 'chunkId')) %>%
  select(pawsId, label, mainTopic, subTopic) %>% collect()
att <- left_join(attendance, class, by = c('att_event' = 'eventId')) %>% select(pawsId, eventDate) %>%
  collect() %>% mutate(eventDate = as.Date(eventDate))
students <- roster %>% select(pawsId, section) %>% filter(section %in% inClass) %>% collect()

# filter dataframes to data of interest
goodSubs <- subs %>% filter(label %in% chunks)
goodAtt <- att %>% filter(eventDate >= as.Date(startDate), eventDate < as.Date(endDate))

# find delinquents
badStudents <- students %>% filter(!(pawsId %in% goodSubs$pawsId))
delinquents <- badStudents %>% filter(!(pawsId %in% goodAtt$pawsId))

# write results to a csv file
write.csv(delinquents, file = name)
