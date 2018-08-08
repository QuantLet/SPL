## Function: org_data
    ## Args:     data         - the raw data that needs to be organised
    ##           
    ##           
    ## Returns:  x			  - an organised dataframe for use in plotting

#Function to organise data
org_data = function(x) {
    x = t(x) 
		#Transpose dataframe

	x = melt(x, varnames=c('WindowSize', 'day')) 
		#Melt dataframe for use in a plot

    return(x)
    	#return
}

## Function: merge_dataframe
    ## Args:     dfList         - a list of dataframes to be merged together
    ##           
    ##           
    ## Returns:  x			    - the final, merged dataframe

#Function to merge dataframe
merge_dataframe = function(dfList) {

	x = dfList[[1]]
	#initialise first list element
	
	for( i in 2:(length(dfList)) ) {
		#Loop through the list and merge all elements
		x = merge(x, dfList[[i]], all = TRUE)
	}

	return(x)
		#return
}