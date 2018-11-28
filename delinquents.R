library(tidyverse)

source('connection.R')
source('constants.R')

# import data
roster <- tbl(conDatasource, 'student_user')
attendance <- tbl(conDatasource, 'attendance')
class <- tbl(conDatasource, 'attevent')
chunk <- tbl(conDatasource, 'exchunk')