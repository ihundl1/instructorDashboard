library(tidyverse)
library(readr)
library(jsonlite)

source('connection.R')

# import data
roster <- tbl(conDatasource, 'student_user')
submission <- tbl(conDatasource, 'exsubmission')

# create dataframe
botDF <- left_join(roster, submission, by = 'pawsId') %>% 
  group_by(pawsId) %>% summarize(subs = n()) %>% collect() %>% 
  mutate(text = case_when(
    subs >= 50 ~ 'Great',
    subs >= 25 ~ 'Good',
    subs >= 10 ~ 'Bad',
    T ~ 'Terrible'
  )) %>% mutate(flag = 'not sent', runTime = Sys.time()) %>%
  select(pawsId, runTime, text, flag)

botDF %>% toJSON(.) %>% write_lines(., 'botFile.json')
