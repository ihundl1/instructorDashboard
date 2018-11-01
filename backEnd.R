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
dailySubs <- full %>% filter(!is.na(sectionId)) %>% group_by(subDate, sectionId, pawsId) %>% 
  summarise(submissions = n()) %>% count(subDate, sectionId)

# what percent of students of have submitted assignments? Multiple times?
subs <- full %>% filter(!is.na(sectionId)) %>% group_by(label, pawsId, sectionId) %>% 
  summarise(submissions = n()) %>% group_by(label, sectionId) %>% summarise(subStudents = n())

manySubs <- full %>% filter(!is.na(sectionId)) %>% group_by(label, pawsId, sectionId) %>% 
  summarise(submissions = n()) %>% filter(submissions > 1) %>% group_by(label, sectionId) %>%
  summarise(manySubStudents = n())

fullSubs <- subs %>% left_join(manySubs, by = c('label', 'sectionId')) %>% 
  left_join(enrolled, by = "sectionId") %>%
  mutate(pSub = subStudents/sectionSize, pManySubs = manySubStudents/sectionSize)

totalSubs <- fullSubs %>% group_by(label) %>% 
  summarise(subStudents = sum(subStudents, na.rm = TRUE), manySubStudents = sum(manySubStudents, na.rm = TRUE), students = sum(sectionSize, na.rm = TRUE)) %>%
  mutate(pSub = subStudents/students, pManySubs = manySubStudents/students)

# what students have not submitted any files (per chunk)?
good <- full %>% filter(!is.na(sectionId)) %>% count(label, pawsId, sectionId)
cNames <- distinct(good, label)
goodChunks <- c()
for (g in as.vector(cNames$label)){
  goodChunks[[g]] <- subset(good, label==g, select=pawsId)
  goodChunks[[g]] <- goodChunks[[g]][["pawsId"]]
}

badChunks <- c()
for (b in as.vector(cNames$label)){
  badChunks[[b]] <- ss %>% filter(!(pawsId %in% goodChunks[[b]])) %>% select(pawsId)
  badChunks[[b]] <- badChunks[[b]][["pawsId"]]
}

# Who is attending class and open lab?

# Who is NOT attending class/open lab or submitting files?

# What outbound links are being clicked?
#   What pages are those links on / what topics are they related to?


