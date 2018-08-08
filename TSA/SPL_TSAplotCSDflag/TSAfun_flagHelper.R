## Function: data_arrange
    ## Args:     data         - the raw flag data that needs to be arranged
    ##           
    ##           
    ## Returns:  flag_m_new   - flag data, arranged in percentage values
    ##							to be used in a plot

#Function to arrange flag data
data_arrange = function(data, sent) {

		#convert to int matrix
	data 		= data.matrix(data)

		#constant
	num_windows = ncol(data)

		#sum the rows
	data 		= rowSums(data, na.rm = TRUE)

		#calc percentages
	data 		= 100*data/num_windows

		#melt data
	flag_m_new  = melt(data, value.name = sent)

		#add row column
	flag_m_new  = cbind(days = rownames(flag_m_new), flag_m_new)

		#return
	return(flag_m_new)
}