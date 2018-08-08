## Function: readData
    ## Args:     ind	      - the csd indicator data to be read in
    ##           
    ##           sent	      - the sentiment of the csd indicator to 
    ##           				be read in
    ##          
    ## Returns:  table		  - the data, arranged in a dataframe

# Function to read in the data
readData = function(ind, sent) {

	#name of directory
directory = paste0(sent, '_', ind, '.csv')

	#read in data
table     = read.csv(directory)
	
	#return
return(table)
}