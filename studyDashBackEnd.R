# packages
library(RColorBrewer)

# data source
source('connection.R')
source('constants.R')

# import tables from database
activity <- tbl(conDatasource, 'activity')
pageTable <- tbl(conDatasource, 'page')
roster <- tbl(conDatasource, 'student_user')

# join tables
activity <- left_join(activity, pageTable, by = 'pagePath')

# create study table
study <- activity %>% select(instances, userId, pagePath, eventCategory, eventValue:pageCategory) %>%
  filter(date >= currentSemester)
study <- left_join(study, roster, by = 'userId')
study <- study %>% select(-lastname, -firstname) %>% filter(eventCategory == "Page Visibility") %>% 
  collect() %>% mutate(date = as.Date(date)) %>%
  group_by(pawsId, pagePath, pageCategory, eventCategory, date, section) %>% summarize(eventValue = sum(eventValue)) %>%
  mutate(visMinutes = eventValue / 60)
study <- as.data.frame(study)

# split study table
studyTopics <- split(study, study$pageCategory)
excel <- studyTopics$excel
word <- studyTopics$word
is <- studyTopics$is

## TOPICS!
# Information Systems
is <- assignTopic(is)

# Excel
excel <- assignTopic(excel)

# Word
word <- assignTopic(word)

## FUNCTIONS!
# Density plot function
densePlot <- function(m=5){
  study %>% group_by(pawsId, section) %>% 
    summarise(sessionTime = sum(visMinutes)) %>% filter(sessionTime > m) %>%
    ggplot(aes(x = sessionTime)) + geom_density() + facet_wrap(~section)
}

# Studying Plot Function
studyPlot <- function(table, m=5, i="All"){
  if (i != "All"){
    table <- table %>% filter(section == i)
  }
  table %>% filter(visMinutes >= m) %>% 
    distinct(pawsId, topic, date) %>% count(topic, date) %>%
    ggplot(aes(x = date, y = n, color = topic)) + geom_line() +
    theme_minimal() + labs(x = "Date", y = "Students") +
    scale_color_manual(values = brewer.pal(n = 8, name = "Set1"))
}

# Concepts v. Practice Plot Function
vsPlot <- function(table, m=5, i="All"){
  if (i != "All"){
    table <- table %>% filter(section == i)
  }
  table %>% filter(subtopic != "Other") %>% filter(visMinutes >= m) %>%
    distinct(pawsId, subtopic, date) %>% count(subtopic, date) %>%
    ggplot(aes(x = date, y = n, color = subtopic)) + geom_line() +
    theme_minimal() + labs(x = "Date", y = "Students") +
    scale_color_manual(values = brewer.pal(n=3, name="Set2")) + ggtitle("Concepts vs. Practice")
}

# Date Limits Function
dateLimits <- function(c){
  coord_cartesian(xlim = c(as.Date(int_start(c)), as.Date(int_end(c))))
}
