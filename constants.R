library(lubridate)

## CONSTANTS!
# first day of the current semester
currentSemester <-  as.Date("2018-08-20")

# Checkup dates (~2 week intervals with the end date being the last day of testing)
c1 <- interval('2018-08-24', '2018-09-07')
c2 <- interval(int_end(c1), '2018-09-21')
c3 <- interval(int_end(c2), '2018-10-05')
c4 <- interval(int_end(c3), '2018-10-19')
c5 <- interval(int_end(c4), '2018-11-09')
c6 <- interval(int_end(c5),'2018-11-28')
# checkup starts (first day of checkup testing)
cs1 <- "2018-09-03"
cs2 <- "2018-09-17"
cs3 <- "2018-10-01"
cs4 <- "2018-10-15"
cs5 <- "2018-11-05"
cs6 <- "2018-11-26"

# exam dates (2 week intervals with the end date being the last day of testing)
# e = excel & w = word
e1 <- interval('2018-09-14', '2018-09-28')
e2 <- interval('2018-10-12', '2018-10-26')
w1 <- interval('2018-11-02', '2018-11-16')
# exam starts (first day of exam testing)
es1 <- "2018-09-25"
es2 <- "2018-10-25"
ws1 <- "2018-11-12"

## TOPICS!
isTopics <- c("Home", "Introduction", "Cybersecurity", "Hardware", "Software", "Networking", 
              "IS Foundations", "Value Creation", "Business Models")
excelTopics <- c("Home", "Formatting", "Functions", "Worksheet Mgt", "Sort & Filter", 
                 "Pivot Tables", "Comprehensive Analysis")
wordTopics <- c("Home", "Simple Document", "Template", "Complex Document")


assignTopic <- function(df) {
 df %>% mutate(topic = case_when(
  pageCategory == 'is' ~ case_when(
    grepl('intro', pagePath) ~ isTopics[2],
    grepl('cyber', pagePath) ~ isTopics[3],
    grepl('hard', pagePath) ~ isTopics[4],
    grepl('soft', pagePath) ~ isTopics[5],
    grepl('net', pagePath) ~ isTopics[6],
    grepl('info', pagePath) ~ isTopics[7],
    grepl('val', pagePath) ~ isTopics[8],
    grepl('ecom', pagePath) ~ isTopics[9],
    TRUE ~ isTopics[1]
  ),
  pageCategory == 'excel' ~ case_when(
    grepl('analysis', pagePath) ~ excelTopics[7],
    grepl('pivot', pagePath) ~ excelTopics[6],
    grepl('sort', pagePath) ~ excelTopics[5],
    grepl('management', pagePath)~ excelTopics[4],
    grepl('functions', pagePath) ~ excelTopics[3],
    grepl('format', pagePath) ~ excelTopics[2],
    TRUE ~ excelTopics[1]
  ),
  pageCategory == 'word' ~ case_when(
    grepl('simple', pagePath) ~ wordTopics[2],
    grepl('template', pagePath) ~ wordTopics[3],
    grepl('complex', pagePath) ~ wordTopics[4],
    TRUE ~ wordTopics[1]
  ),
  TRUE ~ 'other'
 )) %>% mutate(subtopic = case_when(
   grepl('concepts', pagePath) ~ "Concepts",
   grepl('practice', pagePath) ~ "Practice",
   TRUE ~ "Other"
 ))
}
