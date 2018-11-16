library(tidyverse)

source('connection.R')
source('constants.R')

activity <- tbl(conDatasource, 'activity')
users <- tbl(conDatasource, 'student_user')
pageTable <- tbl(conDatasource, 'page')

scheduleLinks <- activity %>% filter(pagePath == '/schedule' & eventCategory == 'Outbound Link') %>% 
  left_join(users, by = 'userId') %>% distinct(pawsId, eventLabel) %>% filter(!is.na(pawsId)) %>% 
  arrange(pawsId) %>% collect()

topicLinks <- activity %>% filter(eventCategory == 'Outbound Link') %>% left_join(users, by = 'userId') %>%
  group_by(pagePath, eventLabel, date) %>% summarise(clicks = n()) %>%
  left_join(pageTable, by = 'pagePath') %>% filter(pageCategory != 'main' & pageCategory != 'null') %>%
  collect() %>% assignTopic() %>% as.data.frame() %>% mutate(date = as.Date(date))



