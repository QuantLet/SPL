#New Regression attempt

big_table = readRDS("~/supersolution/data/big_table.RDS")
head(big_table)

class(big_table)

#Variables in the different regressions
variables_A = c("Date", "R","YP", "MP","DEI","UI","URP","UTS")
variables_B = c("Date", "R","MP", "DEI", "UI", "URP", "UTS")
variables_C = c("Date", "R","EWNY","MP","DEI","UI","URP","UTS")
variables_D = c("Date", "R","MP","DEI","UI","URP","UTS","OG")
variables_E = c("Date","R","MP","DEI","UI","URP","UTS","CG")

#subset variables set from all_data
data_A = big_table[ ,variables_A]
data_B = big_table[ ,variables_B]
data_C = big_table[ ,variables_C]
data_D = big_table[ ,variables_D]
data_E = big_table[ ,variables_E]

#setting the subperiods
DATE1 <- as.Date("1988-01-15") #Beginning of study
DATE2 <- as.Date("2000-12-15")
DATE3 <- as.Date("2001-01-15")
DATE4 <- as.Date("2008-12-15")
DATE5 <- as.Date("2009-01-15")
DATE6 <- as.Date("2017-10-15") # End of study

# regression function
regresfunc = function(start, end, data_Let){
  data_to_reg = data_Let[which(data_Let[, "Date"] >= start & data_Let[, "Date"] < end), ] 
  reg = lm(R~.-Date, data = data_to_reg)
}

#Regressions A
reg_A_Jan1988_Dec2017 = regresfunc(DATE1,DATE6, data_A)
reg_A_Jan1988_Dec2000 = regresfunc(DATE1, DATE2, data_A)
reg_A_Jan2001_Dec2008 = regresfunc(DATE3, DATE4, data_A)
reg_A_Jan2009_Dec2017 = regresfunc(DATE5,DATE6, data_A)

#Regressions B

reg_B_Jan1988_Dec2017 = regresfunc(DATE1,DATE6, data_B)
reg_B_Jan1988_Dec2000 = regresfunc(DATE1,DATE2, data_B)
reg_B_Jan2001_Dec2008 = regresfunc(DATE3,DATE4, data_B)
reg_B_Jan2009_Dec2017 = regresfunc(DATE5,DATE6, data_B)

#Regressions C

reg_C_Jan1988_Dec2017 = regresfunc(DATE1,DATE6, data_C)
reg_C_Jan1988_Dec2000 = regresfunc(DATE1,DATE2, data_C)
reg_C_Jan2001_Dec2008 = regresfunc(DATE3,DATE4, data_C)
reg_C_Jan2009_Dec2017 = regresfunc(DATE5,DATE6, data_C)

#Regressions D

reg_D_Jan1988_Dec2017 = regresfunc(DATE1,DATE6, data_D)
reg_D_Jan1988_Dec2000 = regresfunc(DATE1,DATE2, data_D)
reg_D_Jan2001_Dec2008 = regresfunc(DATE3,DATE4, data_D)
reg_D_Jan2009_Dec2017 = regresfunc(DATE5,DATE6, data_D)

#Regressions E
reg_E_Jan1988_Dec2017 = regresfunc(DATE1,DATE6, data_E)
reg_E_Jan1988_Dec2000 = regresfunc(DATE1,DATE2, data_E)
reg_E_Jan2001_Dec2008 = regresfunc(DATE3,DATE4, data_E)
reg_E_Jan2009_Dec2017 = regresfunc(DATE5,DATE6, data_E)

#Coefficients A
coe_A_Jan1988_Dec2017 = round(summary(reg_A_Jan1988_Dec2017)$coefficients,4)
coe_A_Jan1988_Dec2000 = round(summary(reg_A_Jan1988_Dec2000)$coefficients,4)
coe_A_Jan2001_Dec2008 = round(summary(reg_A_Jan2001_Dec2008)$coefficients,4)
coe_A_Jan2009_Dec2017 = round(summary(reg_A_Jan2009_Dec2017)$coefficients,4)

#Coefficients B
coe_B_Jan1988_Dec2017 = round(summary(reg_B_Jan1988_Dec2017)$coefficients,4)
coe_B_Jan1988_Dec2000 = round(summary(reg_B_Jan1988_Dec2000)$coefficients,4)
coe_B_Jan2001_Dec2008 = round(summary(reg_B_Jan2001_Dec2008)$coefficients,4)
coe_B_Jan2009_Dec2017 = round(summary(reg_B_Jan2009_Dec2017)$coefficients,4)

#Coefficients C
coe_C_Jan1988_Dec2017 = round(summary(reg_C_Jan1988_Dec2017)$coefficients,4)
coe_C_Jan1988_Dec2000 = round(summary(reg_C_Jan1988_Dec2000)$coefficients,4)
coe_C_Jan2001_Dec2008 = round(summary(reg_C_Jan2001_Dec2008)$coefficients,4)
coe_C_Jan2009_Dec2017 = round(summary(reg_C_Jan2009_Dec2017)$coefficients,4)

#Coefficients Ds
coe_D_Jan1988_Dec2017 = round(summary(reg_D_Jan1988_Dec2017)$coefficients,4)
coe_D_Jan1988_Dec2000 = round(summary(reg_D_Jan1988_Dec2000)$coefficients,4)
coe_D_Jan2001_Dec2008 = round(summary(reg_D_Jan2001_Dec2008)$coefficients,4)
coe_D_Jan2009_Dec2017 = round(summary(reg_D_Jan2009_Dec2017)$coefficients,4)

#Coefficients Es
coe_E_Jan1988_Dec2017 = round(summary(reg_E_Jan1988_Dec2017)$coefficients,4)
coe_E_Jan1988_Dec2000 = round(summary(reg_E_Jan1988_Dec2000)$coefficients,4)
coe_E_Jan2001_Dec2008 = round(summary(reg_E_Jan2001_Dec2008)$coefficients,4)
coe_E_Jan2009_Dec2017 = round(summary(reg_E_Jan2009_Dec2017)$coefficients,4)

A_list = list(coe_A_Jan1988_Dec2017 = coe_A_Jan1988_Dec2017, coe_A_Jan1988_Dec2000 = coe_A_Jan1988_Dec2000,
              coe_A_Jan2001_Dec2008= coe_A_Jan2001_Dec2008, coe_A_Jan2009_Dec2017=coe_A_Jan2009_Dec2017,
              coe_B_Jan1988_Dec2017 = coe_B_Jan1988_Dec2017, coe_B_Jan1988_Dec2000 = coe_B_Jan1988_Dec2000,
              coe_B_Jan2001_Dec2008= coe_B_Jan2001_Dec2008, coe_B_Jan2009_Dec2017=coe_B_Jan2009_Dec2017,
              coe_C_Jan1988_Dec2017 = coe_C_Jan1988_Dec2017, coe_C_Jan1988_Dec2000 = coe_C_Jan1988_Dec2000,
              coe_C_Jan2001_Dec2008= coe_C_Jan2001_Dec2008, coe_C_Jan2009_Dec2017=coe_C_Jan2009_Dec2017,
              coe_D_Jan1988_Dec2017 = coe_D_Jan1988_Dec2017, coe_D_Jan1988_Dec2000 = coe_D_Jan1988_Dec2000,
              coe_D_Jan2001_Dec2008= coe_D_Jan2001_Dec2008, coe_D_Jan2009_Dec2017=coe_D_Jan2009_Dec2017,
              coe_E_Jan1988_Dec2017 = coe_E_Jan1988_Dec2017, coe_E_Jan1988_Dec2000 = coe_E_Jan1988_Dec2000,
              coe_E_Jan2001_Dec2008= coe_E_Jan2001_Dec2008, coe_E_Jan2009_Dec2017=coe_E_Jan2009_Dec2017)
for (i in names(A_list)){
  write.csv(A_list[[i]], paste0(i, ".csv"))
}

