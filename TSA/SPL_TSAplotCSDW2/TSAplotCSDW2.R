
# Load required packages
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("data.table")) install.packages("data.table")

library(tidyverse)
library(ggplot2)
library(data.table)

# Source helper functions
source("TSAfun_helper.R")
source("TSAfun_readData.R")


## Initial conditions
sent 	   = "POS"
	#choose NEG/POS

chosenSize = 24
	#Choose a size between 3 and 50

# Check and set sentiment dataset
if (sent == "POS") {
	W2   = readData("W2", "positive")
	flag = readData("flag", "positive")
} else if (sent == "NEG") {
	W2   = readData("W2", "negative")
	flag = readData("flag", "negative")
} else {
	stop("Sentiment value not specified")
}

flag$out = NULL
	#remove identifier variable

# Organise dataframe for use in a plot
csd_m 	 = org_data(W2)
flag_m   = org_data(flag)

# Choose window size
window  = paste("W2_ws", chosenSize, sep="")
mean	= paste("running.mean_ws", chosenSize, sep="")
sd  	= paste("running.SD_ws", chosenSize, sep="")
sd2 	= paste("running.SD2_ws", chosenSize, sep="")
sd_neg  = paste("neg.running.SD_ws", chosenSize, sep="")
sd2_neg = paste("neg.running.SD2_ws", chosenSize, sep="")

# Plot name 
path = paste("graphs/", "p_W2full_", sent ,"_", 
	chosenSize,".pdf", sep = "")

# Plot graphs
ggplot(data = na.omit(flag_m), 
	aes(x = day, y = value)) +
    geom_point(data = na.omit(subset(csd_m, WindowSize %in% window)),
	shape = 1, colour = "red") +
	#Plot W2

    geom_line(data = na.omit(subset(flag_m, WindowSize %in% mean)),
              linetype="solid", colour = "black", aes(group = WindowSize)) +
    geom_text(x = 102, y = 0, label = "mean") +
	#Plot mean

    geom_line(data = na.omit(subset(flag_m, WindowSize %in% sd)),
              linetype="dashed", colour = "dark gray") +
    geom_text(x = 102, y = 4, label = "SD") +
	#Plot std dev

    geom_line(data = na.omit(subset(flag_m, WindowSize %in% sd2)),
              linetype="dashed", colour = "dark gray") +
    geom_text(x = 102, y = 7, label = "2SD") +
	#Plot 2*std dev

    geom_line(data = na.omit(subset(flag_m, WindowSize %in% sd_neg)),
              linetype="dashed", colour = "dark gray") +
    geom_text(x = 102, y = -4, label = "-SD") +
	#Plot neg std dev

    geom_line(data = na.omit(subset(flag_m, WindowSize %in% sd2_neg)),
              linetype="dashed", colour = "dark gray") +
    geom_text(x = 102, y = -7, label = "-2SD") +
	#Plot 2*neg std dev

# Labels
    labs(title = "Plot of W2 Indicator") +
    geom_vline(xintercept = 75, colour = "blue") +
    geom_text(aes(x=75, y=Inf,
                  label="Event"), hjust=-.1, vjust= 2, colour="blue")

ggsave(path)

# Cleanup
rm(list = ls())

