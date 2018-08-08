
# Load required packages
if(!require("reshape2")) install.packages("reshape2")
if(!require("ggplot2")) install.packages("ggplot2")

library(reshape2)
library(ggplot2)

# Set parameters
window_sizes = 3:50
subset		 = character(length(window_sizes))

for (i in window_sizes) {
	subset[i-2] = paste0("flag_ws", i)
}

# Source helper functions
source("TSAfun_flagHelper.R")
source("TSAfun_readData.R")

flag 	   = readData("flag", "positive")

# Arrange data for positive sentiment
flag 	   = flag[subset]
flag_m_pos = data_arrange(flag, "pos")

flag 	   = readData("flag", "negative")

# Arrange data for negative sentiment
flag 	   = flag[subset]
flag_m_neg = data_arrange(flag, "neg")

# Combine data for plot
flags 	   = cbind(flag_m_pos, flag_m_neg)
flags 	   = melt(flags, measure.vars = c("pos", "neg"), 
						variable.name = "sentiment")

# Plot name 
path 	   = paste("graphs/p_flags.pdf", sep = "")

# Plot graphs
ggplot(data = flags,
       aes(x = as.numeric(days), y = value, colour = sentiment)) +
       geom_jitter(width = 0.2, height = 0) +
       labs(title = paste("Percentage of windows detecting CSD")) +
       xlab("days") +
       ylab("% of windows") +
       scale_color_manual(breaks = c("pos", "neg"),
                          values = c("dark green", "red")) +
       geom_vline(xintercept = 75, colour = "blue") +
       geom_text(aes(x = 75, y = Inf, label =" Event"),
                 hjust = -.1, vjust = 2, colour = "blue") +
       ylim(0, 100)

ggsave(path)

# Cleanup
rm(list = ls())
		
