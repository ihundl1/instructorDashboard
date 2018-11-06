library(tidyverse)

source('connection.R')

activity <- tbl(conDatasource, 'activity')
users <- tbl(conDatasource, 'student_user')
pageTable <- tbl(conDatasource, 'page')

syllabusLinks <- activity %>% filter(pagePath == '/syllabus' & eventCategory == 'Outbound Link') %>% 
  left_join(users, by = 'userId') %>% distinct(pawsId, eventLabel) %>% filter(!is.na(pawsId)) %>% 
  arrange(pawsId) %>% collect()

topicLinks <- activity %>% filter(eventCategory == 'Outbound Link') %>% left_join(users, by = 'userId') %>%
  select(pawsId, pagePath, eventLabel, date) %>% filter(!is.na(pawsId)) %>%
  left_join(pageTable, by = 'pagePath') %>% filter(pageCategory != 'main' & pageCategory != 'null') %>% 
  group_by(pageCategory, date) %>% summarise(clicks = n()) %>% collect() %>% mutate(date = as.Date(date))


