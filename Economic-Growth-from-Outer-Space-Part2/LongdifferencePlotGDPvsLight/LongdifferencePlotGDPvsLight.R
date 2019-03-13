#plot 
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(reshape, ggplot2, plyr)
####ggplot #####
mytheme = theme(plot.title         = element_text(face     = "bold",
                                                  size     = "14", 
                                                  color    = "black",
                                                  hjust    = 0.5),
                axis.title         = element_text(size     = 16, 
                                                  color    = "black"),
                axis.text          = element_text(size     = 14)
                panel.background   = element_rect(fill     = "white",
                                                  color    = "black"),
                plot.background    = element_rect(fill     = "grey93"),
                panel.grid.major.y = element_line(color    = "snow2",
                                                  linetype = 1),
                panel.grid.major.x = element_line(color    = "snow2",
                                                  linetype = 1),
                plot.margin        = margin(20, 15, 40, 30))

###############################################################
# plot figure 6c: long difference relationship between light and GDP growth
setwd('F:/google/language programming/language/data')
wide = read.csv('longdif.csv') #need to set path for longdif.csv
wide = wide[wide$Code != 'GNQ' & 
            wide$Code != 'ATA' & 
            wide$Code != 'TUV', ]
wide = wide[complete.cases(wide) & ! is.infinite(wide$lightarea9205), ]

ggplot(data = wide, aes(lightarea9205, realGDP9205, label = Code), adjust = 0.8) +
  geom_point() + 
  geom_smooth(method  = "lm", se = F) + 
  geom_text(aes(label = Code), hjust=0, vjust=0) + 
  labs(title = 'Figure 6c.Replicate GDP versus lights: long differences', 
       y     = 'ln(GDP 05-06) - ln(GDP 92-93)', 
       x     = 'ln(lights 05-06) - ln(lights 92-93)') + 
  mytheme + 
  scale_x_continuous(breaks = c(-0.8, -0.4, seq(0, 2, 0.4)))
