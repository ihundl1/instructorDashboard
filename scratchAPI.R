library(tidyverse)
library(RGoogleAnalyticsPremium)

source("connAPI.R")

queryList <- Init(start.date = "2018-08-21",
                  end.date = "2018-12-1",
                  dimensions = "ga:pagePath,ga:eventCategory,ga:eventLabel,ga:date,ga:browser,ga:operatingSystem",
                  metrics = "ga:eventValue, ga:timeOnScreen"
                  )

gaQuery <- QueryBuilder(queryList)
gaData <- GetFile(gaQuery, token)
