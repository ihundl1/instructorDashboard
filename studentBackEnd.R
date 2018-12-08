library(tidyverse)

source('connection.R')
#source('constants.R')

# import data
roster <- tbl(conDatasource, 'student_user')
attendance <- tbl(conDatasource, 'attendance')
classes <- tbl(conDatasource, 'attevent')
section <- tbl(conDatasource, 'section')
chunk <- tbl(conDatasource, 'exchunk')
submission <- tbl(conDatasource, 'exsubmission')
activity <- tbl(conDatasource, 'activity')
pages <- tbl(conDatasource, 'page')

# Inputs
## Semester & Section
semSec <- section %>% collect() %>% mutate(semester = substr(sectionId, 1, 6)) %>%
  mutate(sectionName = paste("Section", substr(sectionId, 7, 7)))
semList <- setNames(unique(semSec$semester), unique(semSec$semester)) %>% as.list()
## Student
nameTable <- left_join(roster, section, by = c('section' = 'sectionId')) %>% 
  filter(delivery == "inclass") %>% distinct(pawsId, lastname, firstname, section) %>% 
  arrange(lastname) %>% collect()

# Attandance
course <- classes %>% collect() %>% mutate(sectionId = substr(eventId, 1, 7)) %>%
  group_by(sectionId, eventTopic) %>% summarise(classTotal = n())

attend <- attendance %>% left_join(classes, by = c('att_event' = 'eventId')) %>% collect() %>%
  group_by(pawsId, eventTopic) %>% summarise(att = n())

big <- section %>% filter(delivery == "inclass") %>% select(sectionId, instructor) %>% 
  left_join(roster, by = c('sectionId' = 'section')) %>% collect() %>% 
  left_join(attend, by = 'pawsId') %>% left_join(course, by = c('sectionId', 'eventTopic')) %>%
  mutate(attPerc = att / classTotal) %>% mutate(missed = classTotal - att)

# Submissions
subs <- submission %>% filter(pawsId != "") %>% left_join(chunk, by = c('label' = 'chunkId')) %>% 
  group_by(pawsId, label, mainTopic, subTopic) %>% 
  summarise(submissions = n(), bestScore = max(totalscore, na.rm = TRUE)) %>% collect() %>% 
  as.data.frame()

assignments <- chunk %>% collect()

levels <- distinct(assignments, subTopic)
levels <- as.vector(levels$subTopic)
assignments$st2 <- factor(assignments$subTopic, levels = levels)


# Theory
theory <- activity %>% filter(userId != "") %>% filter(hitType == "pageview" | eventCategory == "Page Visibility") %>% 
  right_join(roster, by = "userId") %>% select(pawsId, pagePath, hitType, eventCategory, eventValue) %>%
  left_join(pages, by = "pagePath") %>% filter(pageCategory == "is" & pagePath != '/is/') %>% collect()
theory <- theory %>% mutate(topic = substr(pagePath, 5, nchar(pagePath))) %>% 
  mutate(topic = gsub("-", " ", topic))
pageviews <- theory %>% filter(hitType == "pageview") %>% group_by(pawsId, topic) %>% 
  summarize(pgViews = n())
timeviews <- theory %>% filter(eventCategory == "Page Visibility") %>% group_by(pawsId, topic) %>%
  summarise(seconds = sum(eventValue, na.rm = TRUE))
theory2 <- left_join(pageviews, timeviews, by = c("pawsId", "topic")) %>% 
  mutate(minutes = seconds/60, hours = minutes/60)




