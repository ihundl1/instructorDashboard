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
study <- study %>% select(-lastname, -firstname) %>% collect() %>% 
  mutate(visMinutes = if_else(eventCategory == 'Page Visibility', eventValue / 60, 0)) %>%
  mutate(date = as.Date(date))

# split study table
studyTopics <- split(study, study$pageCategory)
excel <- studyTopics$excel
word <- studyTopics$word
is <- studyTopics$is

## TOPICS!
# Information Systems
isTopics <- c("Home", "Introduction", "Cybersecurity", "Hardware", "Software", "Networking", 
              "IS Foundations", "Value Creation", "Business Models")
is <- is %>% mutate(topic = case_when(
  grepl('intro', pagePath) ~ isTopics[2],
  grepl('cyber', pagePath) ~ isTopics[3],
  grepl('hard', pagePath) ~ isTopics[4],
  grepl('soft', pagePath) ~ isTopics[5],
  grepl('net', pagePath) ~ isTopics[6],
  grepl('info', pagePath) ~ isTopics[7],
  grepl('val', pagePath) ~ isTopics[8],
  grepl('ecom', pagePath) ~ isTopics[9],
  TRUE ~ isTopics[1]
))

# Excel
excelTopics <- c("Home", "Formatting", "Functions", "Worksheet Mgt", "Sort & Filter", 
                 "Pivot Tables", "Comprehensive Analysis")
excel <- excel %>% mutate(topic = case_when(
  grepl('analysis', pagePath) ~ excelTopics[7],
  grepl('pivot', pagePath) ~ excelTopics[6],
  grepl('sort', pagePath) ~ excelTopics[5],
  grepl('management', pagePath)~ excelTopics[4],
  grepl('functions', pagePath) ~ excelTopics[3],
  grepl('format', pagePath) ~ excelTopics[2],
  TRUE ~ excelTopics[1]
)) %>% mutate(subtopic = case_when(
  grepl('concepts', pagePath) ~ "Concepts",
  grepl('practice', pagePath) ~ "Practice",
  TRUE ~ "Other"
))

# Word
wordTopics <- c("Home", "Simple Document", "Template", "Complex Document")
word <- word %>% mutate(topic = case_when(
  grepl('simple', pagePath) ~ wordTopics[2],
  grepl('template', pagePath) ~ wordTopics[3],
  grepl('complex', pagePath) ~ wordTopics[4],
  TRUE ~ wordTopics[1]
)) %>% mutate(subtopic = case_when(
  grepl('concepts', pagePath) ~ "Concepts",
  grepl('practice', pagePath) ~ "Practice",
  TRUE ~ "Other"
))

## FUNCTIONS!
# Density plot function
densePlot <- function(m=5){
  study %>% filter(eventCategory == "Page Visibility") %>% group_by(instances, section) %>% 
    summarise(sessionTime = sum(visMinutes)) %>% filter(sessionTime > m) %>%
    ggplot(aes(x = sessionTime)) + geom_density() + facet_wrap(~section)
}

# Studying Plot Function
studyPlot <- function(table, m=5, i="All"){
  if (i != "All"){
    table <- table %>% filter(section == i)
  }
  table %>% filter(eventCategory == 'Page Visibility', visMinutes >= m) %>% 
    distinct(userId, topic, date) %>% count(topic, date) %>%
    ggplot(aes(x = date, y = n, color = topic)) + geom_line() +
    theme_minimal() + labs(x = "Date", y = "Students") +
    scale_color_manual(values = brewer.pal(n = 8, name = "Set1"))
}

# Concepts v. Practice Plot Function
vsPlot <- function(table, m=5, i="All"){
  if (i != "All"){
    table <- table %>% filter(section == i)
  }
  table %>% filter(subtopic != "Other") %>% filter(eventCategory == "Page Visibility", visMinutes >= m) %>%
    distinct(userId, subtopic, date) %>% count(subtopic, date) %>%
    ggplot(aes(x = date, y = n, color = subtopic)) + geom_line() +
    theme_minimal() + labs(x = "Date", y = "Students") +
    scale_color_manual(values = brewer.pal(n=3, name="Set2")) + ggtitle("Concepts vs. Practice")
}

# Date Limits Function
dateLimits <- function(c){
  coord_cartesian(xlim = c(as.Date(int_start(c)), as.Date(int_end(c))))
}

# QUESTIONS!
# What outbound links are being clicked?
#   What pages are those links on / what topics are they related to?
