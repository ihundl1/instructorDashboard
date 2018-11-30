library(tidyverse)

source('connection.R')
source('constants.R')

# import data
roster <- tbl(conDatasource, 'student_user')
# activity <- tbl(conDatasource, 'activity')
attendance <- tbl(conDatasource, 'attendance')
classes <- tbl(conDatasource, 'attevent')
section <- tbl(conDatasource, 'section')
chunk <- tbl(conDatasource, 'exchunk')
submission <- tbl(conDatasource, 'exsubmission')

nameTable <- left_join(roster, section, by = c('section' = 'sectionId')) %>% 
  filter(delivery == "inclass") %>% distinct(pawsId, lastname, firstname) %>% arrange(lastname) %>% 
  collect()
nameList <- c()
for (row in 1:nrow(nameTable)) {
  index <- paste0(nameTable[row, 'lastname'], ', ', nameTable[row, 'firstname'])
  nameList[index] <- as.character(nameTable[row, "pawsId"])
}

course <- classes %>% collect() %>% mutate(sectionId = substr(eventId, 1, 7)) %>%
  group_by(sectionId, eventTopic) %>% summarise(classTotal = n())

attend <- attendance %>% left_join(classes, by = c('att_event' = 'eventId')) %>% collect() %>%
  group_by(pawsId, eventTopic) %>% summarise(att = n())

big <- section %>% filter(delivery == "inclass") %>% select(sectionId, instructor) %>% 
  left_join(roster, by = c('sectionId' = 'section')) %>% collect() %>% 
  left_join(attend, by = 'pawsId') %>% left_join(course, by = c('sectionId', 'eventTopic')) %>%
  mutate(attPerc = att / classTotal) %>% mutate(missed = classTotal - att)

subs <- submission %>% filter(pawsId != "") %>% left_join(chunk, by = c('label' = 'chunkId')) %>% 
  group_by(pawsId, label, mainTopic, subTopic) %>% 
  summarise(submissions = n(), bestScore = max(totalscore, na.rm = TRUE)) %>% collect() %>% 
  as.data.frame()

assignments <- chunk %>% collect()

levels <- distinct(assignments, subTopic)
levels <- as.vector(levels$subTopic)
assignments$st2 <- factor(assignments$subTopic, levels = levels)

