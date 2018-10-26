library(tidyverse)
library(lubridate)
library(RColorBrewer)

source('scratchDatabase.R')


# Studying dataframe + minutes -> Split
studying <- studying %>% mutate(a_minutes = if_else(
  a_eventCategory == 'Page Visibility', a_eventValue / 60, 0))

studyTopics <- split(studying, studying$pageCategory)
excel <- studyTopics$Excel
word <- studyTopics$Word
is <- studyTopics$`Information Systems`

# Studying Density Plot Function
densePlot <- function(m=5){
  studying %>% filter(a_eventCategory == "Page Visibility") %>% group_by(a_instances, section) %>% 
    summarise(sessionTime = sum(a_minutes)) %>% filter(sessionTime > m) %>%
    ggplot(aes(x = sessionTime)) + geom_density() + facet_wrap(~section)
}

# Studying Plot Function
createPlot <- function(table, m=5, i="All"){
  if (i != "All"){
    table <- table %>% filter(section == i)
  }
  table %>% filter(a_eventCategory == 'Page Visibility', a_minutes >= m) %>% 
    distinct(a_userId, topic, a_date) %>% count(topic, a_date) %>%
    ggplot(aes(x = a_date, y = n, color = topic)) + geom_line() +
    theme_minimal() + labs(x = "Date", y = "Students Studying") +
    scale_color_manual(values = brewer.pal(n = 8, name = "Set1"))
}

# Date Limits function
dateLimits <- function(c){
  scale_x_date(limits= c(as.Date(int_start(c)), as.Date(int_end(c))))
}

# Information Systems
isTopics <- c("Home", "Introduction", "Cybersecurity", "Hardware", "Software", "Networking", 
              "IS Foundations", "Value Creation", "Business Models")
is <- is %>% mutate(topic = case_when(
  grepl('intro', a_pagePath) ~ isTopics[2],
  grepl('cyber', a_pagePath) ~ isTopics[3],
  grepl('hard', a_pagePath) ~ isTopics[4],
  grepl('soft', a_pagePath) ~ isTopics[5],
  grepl('net', a_pagePath) ~ isTopics[6],
  grepl('info', a_pagePath) ~ isTopics[7],
  grepl('val', a_pagePath) ~ isTopics[8],
  grepl('ecom', a_pagePath) ~ isTopics[9],
  TRUE ~ isTopics[1]
))

# Excel
excelTopics <- c("Home", "Formatting", "Functions", "Worksheet Mgt", "Sort & Filter", 
                 "Pivot Tables", "Comprehensive Analysis")
excel <- excel %>% mutate(topic = case_when(
  grepl('analysis', a_pagePath) ~ excelTopics[7],
  grepl('pivot', a_pagePath) ~ excelTopics[6],
  grepl('sort', a_pagePath) ~ excelTopics[5],
  grepl('management', a_pagePath)~ excelTopics[4],
  grepl('functions', a_pagePath) ~ excelTopics[3],
  grepl('format', a_pagePath) ~ excelTopics[2],
  TRUE ~ excelTopics[1]
))
excel <- excel %>% mutate(t2 = case_when(
  grepl('concepts', a_pagePath) ~ "Concepts",
  grepl('practice', a_pagePath) ~ "Practice",
  TRUE ~ "Other"
))


# Word
wordTopics <- c("Home", "Simple Document", "Template", "Complex Document")
word <- word %>% mutate(topic = case_when(
  grepl('simple', a_pagePath) ~ wordTopics[2],
  grepl('template', a_pagePath) ~ wordTopics[3],
  grepl('complex', a_pagePath) ~ wordTopics[4],
  TRUE ~ wordTopics[1]
))