library(tidyverse)

source('connection.R')
source('constants.R')

# import data
roster <- tbl(conDatasource, 'student_user')
activity <- tbl(conDatasource, 'activity')
attendance <- tbl(conDatasource, 'attendance')
classes <- tbl(conDatasource, 'attevent')
section <- tbl(conDatasource, 'section')

big <- left_join(roster, section, by=c('section'='sectionId')) %>% left_join(attendance, by='pawsId') %>% 
  left_join(classes, by=c('att_event' = 'eventId'))

big <- big %>% select(pawsId:section, instructor, delivery, att_event:eventTopic)

students <- distinct(roster, pawsId) %>% collect()


