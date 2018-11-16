library(tidyverse)

source("connAPI.R")

queryList <- Init(start.date = "2018-11-08",
                  end.date = "2018-11-08",
                  dimensions = "ga:dimension4,ga:dimension7,ga:pagePath,ga:eventCategory,ga:eventLabel,ga:date",
                  metrics = "ga:eventValue",
                  table.id = "ga:157074007",
                  max.results = 10000
                  )

gaQuery <- QueryBuilder(queryList)
gaData <- GetReportData(gaQuery, token)

me <- "5baeab5103afb63cc097b029"

filter(gaData, dimension7 == me & eventCategory == "Page Visibility") %>% select(dimension4, pagePath, eventValue) %>%
  mutate(minutes = eventValue / 60) %>% arrange(pagePath) %>% select(-dimension4)
