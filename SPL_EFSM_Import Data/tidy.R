tidy = function(object, variable){
  object = add_column(object, EI$Date) #add "date" column
  object = object[, c(2,1)] #change order of both columns
  colnames(object) = c("Date", variable) #change column names
  path = paste("~/supersolution/data/", variable, ".RDS", sep = "") #determine file path
  saveRDS(object, path) #save as a RDS file
  return(object)
}