# packages
library(tidyverse)
library(lubridate)

# data sources
source("connection.R")
s3 <- read_csv('http://dds.cct.lsu.edu/wwwgp/s3/s3_data.csv')
# http://dds.cct.lsu.edu/wwwgp/s3/s3_data.csv

# first day of classes for the current semester
currentSemester = as.Date("2018-08-20 7:00:00")

# TAs

ta <- c('rdonov3', 'mjohn74', 'gland31', 'amerkl1', 'ctho244')

# in class sections
inClass <- c('isds1102s3', 'isds1102s4', 'isds1102s5', 'isds1102s6')

# online sections
onClass <- c('isds1102s1', 'isds1102s2', 'isds1102s8')

# import data from database
student <- tbl(conDatasource, 'd_student') %>% collect()
exercises <- tbl(conDatasource, 'd_exercises') %>% collect()
user <- tbl(conDatasource, 'd_user') %>% collect()
activity <- tbl(conDatasource, 'd_activity') %>% collect()

# convert the timestamps from POSIXct to date & time
# filter out any data from previous semesters
activity$a_timestamp <- activity$a_timestamp / 1000
activity$a_timestamp <- as.POSIXct(activity$a_timestamp,origin = "1970-01-01", tz = "America/Chicago")
activity <- filter(activity, a_timestamp >= currentSemester)

exercises$e_subTime <- exercises$e_subTime / 1000
exercises$e_subTime <- as.POSIXct(exercises$e_subTime,origin = "1970-01-01", tz = "America/Chicago")
exercises <- filter(exercises, e_subTime >= currentSemester)

# calculate the percent of in class students who have submitted each assignment
classStudents <- s3 %>%
  filter(slackDomain %in% inClass & !(pawsid %in% ta)) %>% distinct(pawsid) %>% nrow()
submissions <- s3 %>%
  filter(slackDomain %in% inClass & !(pawsid %in% ta)) %>% filter(chunk != "NANA") %>%
  distinct(pawsid, slackDomain, chunk) %>% count(slackDomain, chunk)
submissions$percent <- (submissions$n / classStudents) * 100

# calculate the number of students who have made multiple submissions for each assignment
mSubs <- s3 %>% filter(slackDomain %in% inClass & !(pawsid %in% ta)) %>% 
  filter(chunk != "NANA") %>% count(pawsid, slackDomain, chunk) %>% filter(n > 1)

# What percentage of people doing a chunk have done that chunk more than once?
percMult <- mSubs %>% count(chunk) %>% right_join(submissions, by = "chunk")
percMult$percent <- (percMult$nn / percMult$n) * 100

# Who has not submitted any files (vector)?
noSubs <- s3 %>% filter(is.na(exercise) & slackDomain %in% inClass) %>% distinct(pawsid)

# Who has not submitted to each exercise?

eSubsAll <- s3 %>% split.data.frame(f = s3$exercise)
eSubs1 <- eSubsAll$e1 %>% filter(slackDomain %in% inClass & !(pawsid %in% ta))
eSubs2 <- eSubsAll$e2 %>% filter(slackDomain %in% inClass & !(pawsid %in% ta))
noSubs1 <- s3 %>% filter(slackDomain %in% inClass & !(pawsid %in% ta)) %>% 
  distinct(pawsid) %>% filter(!(pawsid %in% eSubs1$pawsid))
noSubs2 <- s3 %>% filter(slackDomain %in% inClass & !(pawsid %in% ta)) %>% 
  distinct(pawsid) %>% filter(!(pawsid %in% eSubs2$pawsid))

# How many users access the app from each device category?
device <- distinct(activity, a_userId, a_deviceCategory) %>%
  filter(a_deviceCategory != "NA") %>% count(a_deviceCategory)

# How many pageviews does each page get?
# How much daily activity do the pages get?
activity$a_date <- as.Date(date(activity$a_timestamp))
pages <- activity %>% filter(a_hitType == "pageview") %>% count(a_pagePath, a_date)
pagePaths <- distinct(activity, a_pagePath) %>% arrange(a_pagePath)
interest <- c("/tests", "/syllabus")

plot <- pages %>% filter(a_pagePath %in% interest)

# Questions?
# who is attending class & open lab?
# who is NOT attending class & open lab or submitting files?
# When has a student started studying? (using the material)
# Date range: Tues. Sep 11 & Sep 22
#   For every student who is using the app, ch.1 & ch.2
# Define "Start Studying" (pageview, visibility, scrolling)