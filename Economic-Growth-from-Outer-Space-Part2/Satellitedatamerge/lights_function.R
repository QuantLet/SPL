# get satellite data 
SUM.average = function(year.range) {
  # Computes average of light sum and merge to one single file
  #
  # Args:
  #   year.range: years with two satellite images data
  #
  # Returns:
  #   data.frame with country, year and sum variables
  read = function(year.range) {
    a  = data.frame()
    for (i in year.range) {
      # filename for the same year
      file.name1 = paste(paste(i, 1, sep = ''), 'csv', sep = '.')
      file.name2 = paste(paste(i, 2, sep = ''), 'csv', sep = '.')
      # read files and merge
               d = read.csv(file.name1)[c("SOVEREIGNT", "AREA", "SUM")]
              d2 = read.csv(file.name2)[c("SOVEREIGNT", "AREA", "SUM")]
               d = merge(d, d2, by = c("SOVEREIGNT", "AREA"))
           d$SUM = (d$SUM.x + d$SUM.y)/2  # compute sum of average
          d$year = i  # add variable year
               a = rbind(a, d)
    }
    return(a)
  }
  data = data.frame(read(year.range))[c("SOVEREIGNT", "AREA", 'year', "SUM")]  
  return(data)
}


getdata = function(year.range) {
  # read files and merge data
  #
  # Args:
  #   year.range: years with two satellite images data
  #
  # Returns:
  #   data.frame with country, year and sum variables
   read = function(year.range) {
      a = data.frame()  
    for (i in year.range) {
      file.name = paste(i, 'csv', sep = '.')
              d = read.csv(file.name) 
         d$year = i
              a = rbind(a, d)
    }
    return(a)
  }
  data = data.frame(read(year.range))[c("SOVEREIGNT", "AREA", 'year', "SUM")]  
  return(data)
}

#get year from World bank data format
getyear = function(var) {
  # get variable year from World Bank data 
  #
  # Args:
  #   var: years in form "X2016..YR2016."
  #
  # Returns:
  #   year in form 2016
  library(stringr)
  a = word(as.character(var), sep = fixed('..'))
  b = as.integer(str_sub(a, 2))
  return(b)
}

#melt GDP data
melt.GDP = function(df) {
  # get melted GDP data 
  #
  # Args:
  #   df: data frame needed melting
  #
  # Returns:
  #   melted data
  df = df[, -(1:3)]  # read in data starting with Country.Code 
  df = melt(df, id = c(names(df)[1]))  # melt data with Country.Code   
  df$variable = sapply(df$variable, getyear)  # change format of year variable
  return(df)
}


# calculate log difference GDP
log.dif = function(var) {
  # compute log.difference for each year
  #
  # Args:
  #   var: the variable needed for computation
  #   choice : 'log' means log form deduction, 'dif' means simple difference 
  #
  # Returns:
  #   data.frame 
  
  # transfrom long data to wide data
  temp  = subset(data, 
                 select = c("Code", "year", "SOVEREIGNT", var)) 
  temp  = temp[order(temp$year), ]  # order by year
  temp  = reshape(temp,  # change long data to wide data for computation   
                  v.names = var,        
                  idvar = "Code",
                  timevar = "year", 
                  direction = "wide" )
  
  # calculate log difference year by year
  logcompute = function(i) {
    name.1 = paste(var, '.', i+1, sep = "")
    name   = paste(var, '.', i,   sep = "")
    growth = log(temp[ ,name.1]) - log(temp[ ,name])
  } 
  a = data.frame(lapply(1992:2012, logcompute))
  names(a) = 1993:2013  # variable names
  temp  = cbind(temp[1], a)
  long  = melt(temp)
  return(long)
}

dif.merge = function(name) {
  # merge data with different varnames
  #
  # Args:
  #   var: the variable name
  names(long) = c("Code", "year", name)
  data        = merge(data, long,
                      by = c("Code", "year"),
                      all.x = T)
  return(data)
}


# calculate long difference GDP
long.dif = function(var, year, i) {
  # compute long difference for different year range 
  # e.g. log(mean(realGDP2013 + realGDP2012)) - log(mean(realGDP1992 + realGDP1993))
  #      var = realGDP, year = 1992, i = 20
  #
  # Args:
  #  var: varaible name need for computation
  #  year: begin of year for long difference
  #  i: year gap for long difference computation 
  #
  # Returns:
  #  data.frame 
  
  #subset data and transfor long to wide data form
  temp   = subset(data, select = c("Code", "year", "SOVEREIGNT", var)) 
  temp   = temp[order(temp$year), ]
  temp   = reshape(temp, 
                   v.names = var,   
                   idvar = "Code",
                   timevar = "year", 
                   direction = "wide" )
  
  #calculate mean of 2 consequtive years: year begin 
  name.1 = paste(var, '.', year+1, sep = "")
  name   = paste(var, '.', year,   sep = "")
  mean.1 = (temp[ , name.1] + temp[ , name])/2
  
  #calculate mean of 2 consequtive years: year end
  name.1 = paste(var, '.', year+i,   sep = "")
  name   = paste(var, '.', year+1+i, sep = "")
  mean.2 = (temp[ , name.1] + temp[ , name])/2
  
  #calculate growth rate 
  growth = log(mean.2) - log(mean.1)
  temp   = cbind(temp, growth)
  colnames(temp)[ncol(temp)] = as.character(paste(i, '.', year)) 
  temp   = temp[ ,-2:-24]
  names(temp)[2] = as.character(paste(var, i, '.', year))
  return(growth)
}

