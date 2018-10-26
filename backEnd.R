# data source
source('connection.R')
source('constants.R')

# import data from database
exercises <- tbl(conDatasource, 'exsubmission')
eDetails <- tbl(conDatasource, 'exchunk')
students <- tbl(conDatasource, 'student_user')
sections <- tbl(conDatasource, 'section')

# filter data from previous semesters
exercises <- exercises %>% filter(subTime > currentSemester)

# join tables
ss <- left_join(sections, students, by = c('sectionId'='section')) %>% filter(delivery == 'inclass') %>%
  select(sectionId, pawsId)
full <- right_join(ss, exercises, by = 'pawsId') %>% left_join(eDetails, by = c('label'='chunkId')) %>%
  select(-filename) %>% collect() %>% mutate(subTime = as_datetime(subTime)) %>% 
  mutate(subDate = date(subTime))

ss <- collect(ss)

# how many students are enrolled in ISDS 1102 (in-class)?
enrolled <- group_by(ss, sectionId) %>% summarise(sectionSize = n())

# how many students have submitted each assignment per day?
subs <- full %>% filter(!is.na(sectionId)) %>% group_by(label, subDate, sectionId) %>% 
  summarise(submissions = n()) %>% left_join(enrolled, by = 'sectionId')

# what percent of students have submitted the assignment?
subs <- subs %>% mutate(percent = submissions / sectionSize)




