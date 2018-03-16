load_data = function(path) {
  data = read.csv(path)
  colnames(data) = c("Date", "Variable")
  data$Date = as.Date(data$Date,"%d/%m/%Y")
  data = data[data$Date >= "1987-12-01" & data$Date <= "2017-10-15",]
  rownames(data)<-c(1:359)
  return(data)
}