# packages
library(tidyverse)
library(lubridate)

# data source
source("connection.R")
roster <- read.csv("2018FAfullroster.csv")

currentSemester <-  as.Date("2018-08-20")

# import data from database
activity <- tbl(conDatasource, 'd_activity')
activity %>% distinct(a_eventCategory)

activity %>% filter(a_eventCategory == "Page Visibility") %>% 
  distinct(a_eventCategory, a_eventValue, a_pagePath) %>% 
  arrange(desc(a_eventValue)) %>% mutate(minutes = a_eventValue / 60)
# highest value is in seconds, so visibility might be in seconds?

activity %>% filter(a_eventCategory == "Max Scroll") %>% 
  distinct(a_eventCategory, a_eventValue) %>%
  arrange(desc(a_eventValue))
# highest value is 100, so max scroll is probably % of the page

outboundLinks <- activity %>% filter(a_eventCategory == "Outbound Link") %>% 
  count(a_eventLabel, a_timestamp) %>% arrange(desc(n)) %>%
  collect()
# if I could get a list of links on each page, I could track what students are studying?

# create studying dataframe w/ pageCategory & a_date
studying <- activity %>% select(a_instances:a_eventCategory, a_eventValue) %>%
  collect() %>% mutate(pageCategory = case_when(
    grepl('/excel', a_pagePath) ~ "Excel",
    grepl('/is', a_pagePath) ~ "Information Systems",
    grepl('/word', a_pagePath) ~ "Word",
    TRUE ~ "other"
  )) %>% mutate(t1 = a_timestamp / 1000, a_date = as.POSIXct(t1,origin = "1970-01-01", tz = "America/Chicago")) %>%
  select(-a_timestamp, -t1) %>% mutate(a_date = date(a_date)) %>% filter(a_date >= currentSemester)

roster <- roster %>% select(section, userId) %>% 
  mutate(section = as.character(section), userId = as.character(userId))
studying <- right_join(studying, roster, by = c("a_userId" = "userId"))
