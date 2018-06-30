
###############################################################################     
####    MOEmergedata.R    #####################################################     
###############################################################################     
#
# Loads cleaned data set from 'MOErawdata'. Matches the timeframes and 
# aggregates hourly into daily values. 
#
# Input:  '.Rdata' file from the 'MOErawdata' Quantlet.
#
# Ouput:  MOEdata_merge.csv     - data in table form
#         MOEdata_merge.Rdata   - data in Rdata form
#
###############################################################################

# Clear all variables.
rm(list = ls(all = TRUE))
graphics.off()

# Install and load libraries.
libraries = c("lubridate")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)



###############################################################################
####    0.  SET WORKING DIRECTORY    ##########################################
###############################################################################
####    ATTENTION: Working directory is assumed to be the root of the MOE 
####    repository, not the MOEmergedata Quantlet subdirectory!!!


# If needed, set working directory accordingly:
#setwd("path/to/MOE_repository")



###############################################################################
####    1.  LOAD ENERGY MARKET DATA    ########################################
###############################################################################


# Grab data from 'MOErawdata' Quantlet
load("MOErawdata/MOEdata_clean.Rdata")



###############################################################################
####    2.  MATCH TIMEFRAMES      #############################################
###############################################################################


###############################################################################
####    DEFINE SUBROUTINES    #################################################

time.FRAME = function(x) {
    # Chooses Time frame for all variables
    #
    # Args:
    #   x: Imported dataframe
    #
    # Returns:
    #   y: Dataframe with right time frame
    #
    # TODO: Adjust timeframe dynamically.
    #
    start.d     = ymd_hm("2015-01-01 00:00")
    stop.d      = ymd_hm("2017-12-31 23:00")
    ind.start   = which(x$TIME == start.d)
    ind.stop    = which(x$TIME == stop.d)
    ind         = (ind.start: ind.stop)
    y           = x[ind, ]
    return(y)
}


###############################################################################
####    APPLY SUBROUTINES    ##################################################

# Match timeframes.
df.dm       = time.FRAME(df.dm)
df.pun      = time.FRAME(df.pun)
df.solar    = time.FRAME(df.solar)
df.wind     = time.FRAME(df.wind)
df.solar.AT = time.FRAME(df.solar.AT)
df.wind.AT  = time.FRAME(df.wind.AT)



###############################################################################
####    3.  CALCULATE DAILY VALUES    #########################################
###############################################################################


###############################################################################
####    DEFINE SUBROUTINES    #################################################

hour.MW.comp = function(x){
  # Computes MW per hour from 15 minute intervals
  y = sum(x)*0.25
  return(y)
}


###############################################################################
####    APPLY SUBROUTINES    ##################################################

# Calculate daily averages for pun (price) and daily sums for energy production
# variables. 'df.solar' and 'df.wind' are quater-hourly data, so we have to 
# multiply result by 0.25 to go from MW to MW/h (defined in hour.MW.comp()).
df.pun      = aggregate(list("PUN" = df.pun$PUN),
                         list("TIME" = cut(df.pun$TIME, "1 day")),
                         FUN = mean)  # 'FUN = mean' because PUN is a price.
df.dm       = aggregate(list("DM" = df.dm$DEM),
                         list("TIME" = cut(df.dm$TIME, "1 day")),
                         FUN = sum)
df.solar    = aggregate(list(df.solar$`FzHertz`, df.solar$`Amprion`, 
                           df.solar$`TenneT.TSO`, df.solar$`Transnet.BW`), 
                         list("TIME" = cut(df.solar$TIME, "1 day")),
                         FUN = hour.MW.comp)  # Note use of hour.MW.comp!
df.wind     = aggregate(list(df.wind$`FzHertz`, df.wind$`Amprion`,
                              df.wind$`TenneT.TSO`, df.wind$`Transnet.BW`), 
                         list("TIME" = cut(df.wind$TIME, "day")), 
                         FUN = hour.MW.comp)  # Note use of hour.MW.comp!
df.solar.AT = aggregate(list("SOLAR.MW.AT" = df.solar.AT$`SOLAR.MW.AT`), 
                         list("TIME" = cut(df.solar.AT$TIME, "1 day")),
                         FUN = sum)
df.wind.AT  = aggregate(list("WIND.MW.AT" = df.wind.AT$`WIND.MW.AT`), 
                         list("TIME" = cut(df.wind.AT$TIME, "1 day")),
                         FUN = sum)

# Formating is lost when using aggregate(). Fix formating.
names(df.pun)         = c("TIME", "PUN")
names(df.dm)          = c("TIME", "DEM")
names(df.solar)       = c("TIME", "FzHertz", "Amprion", "TenneT.TSO", 
                          "Transnet.BW")
names(df.wind)        = c("TIME", "FzHertz", "Amprion", "TenneT.TSO", 
                          "Transnet.BW")
names(df.solar.AT)    = c("TIME", "SOLAR")
names(df.wind.AT)     = c("TIME", "WIND")
df.pun$TIME           = ymd(df.pun$TIME)
df.dm$TIME            = ymd(df.dm$TIME)
df.solar$TIME         = ymd(df.solar$TIME)
df.wind$TIME          = ymd(df.wind$TIME)
df.solar.AT$TIME      = ymd(df.solar.AT$TIME)
df.wind.AT$TIME       = ymd(df.wind.AT$TIME)



###############################################################################
####    4.  AGGREGATE 'DE' AND 'AT' DATA    ###################################
###############################################################################


# Aggregate over the four DE producers of SOLAR and WIND.
df.solar  = data.frame(df.solar$TIME, rowSums(df.solar[ ,-1]))
df.wind   = data.frame(df.wind$TIME, rowSums(df.wind[ ,-1]))

# Name the merged column.
names(df.solar)   = c("TIME", "SOLAR")
names(df.wind)    = c("TIME", "WIND")

# Merge 'DE' and 'AT' renewable data.
df.solar[,-1]   = df.solar[,-1] + df.solar.AT[,-1]
df.wind[,-1]    = df.wind[,-1] + df.wind.AT[,-1]



###############################################################################
####    5.  BIND AND SAVE DATAFRAME    ########################################
###############################################################################


# Bind dataframes.
df = cbind(df.pun, df.dm, df.solar, df.wind)
df = df[ -c(3,5,7) ]

# Save dataframe as '.Rdata' file for easy read-in in R.
save(df, file="MOEmergedata/MOEdata_merge.Rdata")

# Save dataframe as '.csv' files for use with other software.
####    TODO: Save dataframe as .csv



###############################################################################
####    4. CLEAN UP ENVIRONMENT    ############################################
###############################################################################


# Remove everything except for "df" from environment.
rm(list=ls()[! ls() %in% c("df")]) 
