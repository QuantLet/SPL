
# Load required packages
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("data.table")) install.packages("data.table")

library(tidyverse)
library(ggplot2)
library(data.table)

# Define constants

values = c("var", "acf", "skew")

# Source helper functions
source("TSAfun_helper.R")
source("TSAfun_readData.R")


## Initial conditions
sent 	   = "NEG"
	#choose NEG/POS
indicator  = "ALL"
	#choose Variance/Autocorrelation/Skewness/W2/ALL
chosenSize = 24
	#Choose a size between 3 and 50

# Check and set sentiment dataset
if (sent == "POS") {
	var  = readData("var", "positive")
	acf  = readData("acf", "positive")
	skew = readData("skew", "positive")
	W2   = readData("W2", "positive")
} else if (sent == "NEG") {
	var  = readData("var", "negative")
	acf  = readData("acf", "negative")
	skew = readData("skew", "negative")
	W2   = readData("W2", "negative")
} else {
	stop("Sentiment value not specified")
}

# Choose indicator
if (indicator == "Variance") {
	csd_ind = var
	ind 	= "var"
} else if (indicator == "Autocorrelation") {
	csd_ind = acf
	ind 	= "acf"
} else if (indicator == "Skewness") {
	csd_ind = skew
	ind 	= "skew"
} else if (indicator == "W2") {
	csd_ind = W2
	ind 	= "W2"
} else if (indicator == "ALL") {
	ind 		= c("var", "acf", "skew")
	facetLabels = c('1' = "Variance", 
					'2' = "Autocorrelation", 
					'3' = "Skewness")
		#Label the facet levels
	dfList  	= list(var, acf, skew)
   	dfList  	= lapply(dfList, org_data)
   		#Organise data
   	subsets 	= list()
   		#Initialize list
	for(i in 1:3) {
   		subsets[[i]] = paste(ind[[i]], "_ws", chosenSize, sep="")
   			#Choose window size
	   	dfList[[i]]  = cbind(subset(dfList[[i]], 
	   		WindowSize %in% subsets[[i]]), ID = i)
	   		#Add subset to list
	}

	csd_m = merge_dataframe(dfList)
		#Merge data

} else {
	stop("Indicator not specified")
}

if (indicator != "ALL") {
	csd_m  = org_data(csd_ind)
		#Organise dataframe for use in a plot
	window = paste(ind, "_ws", chosenSize, sep="")
		#Choose window size
	
}

# Plot name 
	path   = paste("graphs/", "p_", indicator,"_", sent ,"_", 
		chosenSize,".pdf", sep = "")

# Plot graphs
if (indicator == "ALL") {
		#Plot facet
	ggplot(data = na.omit(csd_m), aes(x = day, y = value)) +
		geom_line() +
		facet_grid(ID ~ ., scales = "free", labeller = as_labeller(facetLabels)) +
		labs(title = paste("Plot of", indicator, "CSD Indicator")) +
		geom_vline(xintercept = 75, colour = "blue") +
		geom_text(aes(x=75, y=Inf,
                 label="Event"), hjust=-.1, vjust= 2, colour="blue") 
} else {
	ggplot(data = na.omit(subset(csd_m, WindowSize %in% window)), 
		aes(x = day, y = value)) +
	# Plot specific graph
	geom_line() +
	labs(title = paste("Plot of", indicator, "CSD Indicator")) +
	geom_vline(xintercept = 75, colour = "blue") +
	geom_text(aes(x=75, y=Inf,
        label="Event"), hjust=-.1, vjust= 2, colour="blue") 
}
	
ggsave(path)

# Cleanup
rm(list = ls())


