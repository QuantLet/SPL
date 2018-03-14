library(readr)

#set working directory 
setwd("~/EFSM/SPL_EFSM_Correlation")

#importing the big dataset 
big_table <- readRDS("~/EFSM/SPL_EFSM_Correlation/big_table.RDS")

#setting the dates
DATE1 <- as.Date("1988-01-15")
DATE2 <- as.Date("2000-12-15")
DATE3 <- as.Date("2001-01-15")
DATE4 <- as.Date("2008-12-15")
DATE5 <- as.Date("2009-01-15")
DATE6 <- as.Date("2017-10-15")

#creating a data range with the start and end date: 
period1 <- seq(DATE1, DATE2, by="days")
period2 <- seq(DATE3, DATE4, by="days")
period3 <- seq(DATE5, DATE6, by="days")

#creating the subset divided by the date
big_table_Jan1988_Dec2000 <- subset(big_table, Date %in% period1)
big_table_Jan2001_Dec2008 <- subset(big_table, Date %in% period2)
big_table_Jan2009_Oct2017 <- subset(big_table, Date %in% period3)

#creating the correlation matrix for every subperiod (and for the whole period) and rounding it
matrix_Jan1988_Dec2000 <- round (cor(big_table_Jan1988_Dec2000[, c(4, 5, 8:10, 12)]), 4)
matrix_Jan2001_Dec2008 <- round (cor(big_table_Jan2001_Dec2008[, c(4, 5, 8:10, 12)]), 4)
matrix_Jan2009_Oct2017 <- round (cor(big_table_Jan2009_Oct2017[, c(4, 5, 8:10, 12)]), 4)
matrix_whole_period <- round(cor(big_table[, c(4, 5, 8:10, 12)]), 4)

#wrinting a csv with the matrix
write.csv(matrix_Jan1988_Dec2000,"~/EFSM/SPL_EFSM_Correlation/corrmatrix1988-2000.csv")
write.csv(matrix_Jan2001_Dec2008,"~/EFSM/SPL_EFSM_Correlation/corrmatrix2001-2008.csv")
write.csv(matrix_Jan2009_Oct2017,"~/EFSM/SPL_EFSM_Correlation/corrmatrix2009-2017.csv")
write.csv(matrix_whole_period,"~/EFSM/SPL_EFSM_Correlation/corrmatrix2009-2017.csv")
