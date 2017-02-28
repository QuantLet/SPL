#import all relevant libraries and install packages

library(Rcmdr)
library(sandwich)
library(car)
library(splines)
library(RcmdrMisc)
attach(mtcars)

#set working directory 
setwd('C:/Users/Helen/Desktop/FinalVersion-SSPL')

# read file with economic data
EconData = read.table("EconData.txt", header=TRUE, 
                      sep = "\t")

#Column names give to variables
colnames(EconData) = c('Date','EXUS','EX', 'EXP',
                       'IMP', 'GDP_EU','GDP_US', 
                       'OIL', 'GOLD', 'SIL')
EconData = na.omit(EconData)

# set date format
EconData$DATE =as.Date(EconData$Date,"%Y-%m-%d")


###################################################
# 1.Part: descriptive Statistics for the dataset
###################################################

#first statistics
#Output statistical identification numbers 
#(Kennzahlen) like minimum, the first Quantil, 
#Median, average (MW), the third Quantil, 
#maximum - situation parametre (Lageparameter)
summary(EconData)

#Output of the dispersion (Streuung): Variance, 
#standard deviation, span (Spannweite)
#Output variance
Variance=c(var(EconData$EX, na.rm = TRUE),
           var(EconData$GDP_EU, na.rm = TRUE),
           var(EconData$GDP_US, na.rm = TRUE),
           var(EconData$IMP, na.rm = TRUE),
           var(EconData$GOLD, na.rm = TRUE),
           var(EconData$OIL, na.rm = TRUE),
           var(EconData$EXP, na.rm = TRUE),
           var(EconData$SIL, na.rm = TRUE))
#Output standard divergence
StandardDiv=c(sd(EconData$EX, na.rm = TRUE),
              sd(EconData$GDP_EU, na.rm = TRUE),
              sd(EconData$GDP_US, na.rm = TRUE),
              sd(EconData$IMP, na.rm = TRUE),
              sd(EconData$GOLD, na.rm = TRUE),
              sd(EconData$OIL, na.rm = TRUE),
              sd(EconData$EXP, na.rm = TRUE),
              sd(EconData$SIL, na.rm = TRUE))
#Output span
Span=c(diff(range(EconData$EX)),
       diff(range(EconData$GDP_EU)),
       diff(range(EconData$GDP_US)),
       diff(range(EconData$IMP)),
       diff(range(EconData$GOLD)),
       diff(range(EconData$OIL)),
       diff(range(EconData$EXP)),
       diff(range(EconData$SIL)))

# Create a table for the descriptive statistics of 
#the data
DescriptiveStat=matrix(c(Variance, StandardDiv,Span)
                       ,ncol=3)
colnames(DescriptiveStat)=c('Variance', 'Std','Span')
rownames(DescriptiveStat)=c('Exchange Rate','GDP_EU',
                            'GDP_US', 'Import','Gold',
                            'Crude Oil','Export',
                            'Silver')
DescriptiveStat=t(DescriptiveStat)
DescriptiveStat.table = as.table(DescriptiveStat)
DescriptiveStat.table


#Output further statistics
#Output of absolut and relative frequency tables
#absolute, frequencies
table(EconData$EX)
table(EconData$GDP_EU)
table(EconData$GDP_US)
table(EconData$IMP)
table(EconData$GOLD)
table(EconData$OIL)
table(EconData$EXP)
table(EconData$SIL)

# relative frequencies
prop.table(EconData$EX)
prop.table(EconData$GDP_EU)
prop.table(EconData$GDP_US)
prop.table(EconData$IMP)
prop.table(EconData$GOLD)
prop.table(EconData$OIL)
prop.table(EconData$EXP)
prop.table(EconData$SIL)

#Output Normal distribution test -all variables 
#are not normal distributed, smaller than 0,05
df=matrix(c(EconData$EX,EconData$GDP_EU,
            EconData$GDP_US, EconData$IMP, 
            EconData$GOLD,EconData$OIL,
            EconData$EXP,EconData$SIL),ncol=8)
colnames(df) = c('Exchange Rate', 'GDP EU',
                 'GDP US','Import','Gold',
                 'Crude Oil','Export','Silver')
df=data.frame(df)

lshap = lapply(df, shapiro.test)
lres  = sapply(lshap, `[`, c("statistic",
                             "p.value"))
lres

colors()
#output Normal Probability Plot of Residuals, 
#save as .png
# Install packages if not installed
library("lattice")
png('QQnorm1.png')
par(mfrow=c(2,4))
qqnorm(EconData$EX, main= "Exchange Rate", 
       col="red",pch=19, cex=0.5)
qqline(EconData$EX, col="red",pch=19, cex=0.5)
qqnorm(EconData$GDP_EU, main= "GDP EU", 
       col="orange",pch=19, cex=0.5)
qqline(EconData$GDP_EU, col="orange",pch=19, 
       cex=0.5)
qqnorm(EconData$GDP_US, main= "GDP US", 
       col="purple",pch=19, cex=0.5)
qqline(EconData$GDP_US, col="purple",pch=19, 
       cex=0.5)
qqnorm(EconData$IMP,main= "Import", 
       col="blue", pch=19, cex=0.5)
qqline(EconData$IMP, col="blue", pch=19, cex=0.5)
qqnorm(EconData$EXP,main= "Export", 
       col="cyan", pch=19, cex=0.5)
qqline(EconData$EXP, col="cyan", pch=19, cex=0.5)
qqnorm(EconData$GOLD,main= "Gold price", 
       col="tan", pch=19, cex=0.5)
qqline(EconData$GOLD, col="tan", pch=19, cex=0.5)
qqnorm(EconData$SIL, main= "Silver price", 
       col="green",pch=19, cex=0.5)
qqline(EconData$SIL, col="green",pch=19, cex=0.5)
qqnorm(EconData$OIL, main= "Oil price", 
       col="brown", pch=19, cex=0.5)
qqline(EconData$OIL, col="brown", pch=19, cex=0.5)
dev.off()

# output stem and leaf plot
stem(EconData$EX)
stem(EconData$GDP_EU)
stem(EconData$GDP_US)
stem(EconData$IMP)
stem(EconData$GOLD)
stem(EconData$OIL)
stem(EconData$EXP)
stem(EconData$SIL)

#output histogramm, save as .png
png('Histogramm1.png')
par(mfrow=c(2,4))
plot(density(EconData$EX), main="Exchange Rate", 
     col="red")
hist(EconData$EX, main= "Exchange Rate", 
     col="red",pch=19, cex=0.5)
plot(density(EconData$GDP_EU),main="GDP EU", 
     col="orange")
hist(EconData$GDP_EU, main= "GDP EU", 
     col="orange",pch=19, cex=0.5)
plot(density(EconData$GDP_US), main="GDP US", 
     col="purple")
hist(EconData$GDP_US,main= "GDP US", 
     col="purple",pch=19, cex=0.5)
plot(density(EconData$IMP),main="Import", 
     col="blue")
hist(EconData$IMP,main= "Import", col="blue",
     pch=19, cex=0.5)
dev.off()

png('Histogramm2.png')
par(mfrow=c(2,4))
plot(density(EconData$EXP),main="Export", 
     col="cyan")
hist(EconData$EXP,main= "Export", col="cyan",
     pch=19, cex=0.5)
plot(density(EconData$GOLD),main="Gold", 
     col="tan")
hist(EconData$GOLD,main= "Gold price", 
     col="tan",pch=19, cex=0.5)
plot(EconData$SIL,main="Silver", 
     col="green")
hist(EconData$SIL,main= "Silver price", 
     col="green",pch=19, cex=0.5)
plot(density(EconData$OIL),main="Oil price", 
     col="brown")
hist(EconData$OIL,main= "Oil price", 
     col="brown",pch=19, cex=0.5)
dev.off()

#Output Boxplot, save as .png
png('Boxplot1.png')
par(mfrow=c(2,4))
boxplot(EconData$EX,main= "Exchange Rate", 
        col="red",pch=19, cex=0.5)
boxplot(EconData$GDP_EU,main= "GDP EU", 
        col="orange",pch=19, cex=0.5)
boxplot(EconData$GDP_US,main= "GDP US", 
        col="purple",pch=19, cex=0.5)
boxplot(EconData$IMP,main= "Import", 
        col="blue",pch=19, cex=0.5)
boxplot(EconData$EXP,main= "Export", 
        col="cyan",pch=19, cex=0.5)
boxplot(EconData$GOLD,main= "Gold price", 
        col="tan",pch=19, cex=0.5)
boxplot(EconData$SIL,main= "Silver price", 
        col="green",pch=19, cex=0.5)
boxplot(EconData$OIL,main= "Oil price", 
        col="brown",pch=19, cex=0.5)
dev.off()

#Output graphs
#output Plot (Streudiagramm)
png('Scatterplot1.png')
par(mfrow=c(2,4))
plot(EconData$EX, pnorm(EconData$EX), 
     main= "Exchange Rate", type="p", pch=19, 
     col="red", cex=0.5)
lines(EconData$EX, dnorm(EconData$EX))
plot(EconData$GDP_EU, pnorm(EconData$GDP_EU), 
     main= "GDP EU", type="p", pch=19, 
     col="orange", cex=0.5)
lines(EconData$GDP_EU, dnorm(EconData$GDP_EU))
plot(EconData$GDP_US, pnorm(EconData$GDP_US), 
     main= "GDP US", type="p", pch=19, 
     col="purple", cex=0.5)
lines(EconData$GDP_US, dnorm(EconData$GDP_US))
plot(EconData$IMP, pnorm(EconData$IMP), 
     main= "Import", type="p", pch=19, 
     col="blue", cex=0.5)
lines(EconData$IMP, dnorm(EconData$IMP))
plot(EconData$EXP, pnorm(EconData$EXP),
     main= "Export", type="p", pch=19, 
     col="cyan", cex=0.5)
lines(EconData$EXP, dnorm(EconData$EXP))
plot(EconData$GOLD, pnorm(EconData$GOLD),
     main= "Gold price", type="p", pch=19, 
     col="tan", cex=0.5)
lines(EconData$GOLD, dnorm(EconData$GOLD))
plot(EconData$SIL, pnorm(EconData$SIL),
     main= "Silver price", type="p", pch=19, 
     col="green", cex=0.5)
lines(EconData$SIL,dnorm(EconData$SIL))
plot(EconData$OIL, pnorm(EconData$OIL), 
     main= "Oil price", type="p", pch=19, 
     col="brown", cex=0.5)
lines(EconData$OIL, dnorm(EconData$OIL))
dev.off()

#Output 3D Scatterplot
scatter3d(EconData$EX, EconData$IMP, EconData$EXP, 
          main="3D Scatterplot for Exchange Rate, 
          Export, Import")

###################################################
#  Regression Analysis
###################################################

#Compute correlation of import with exchange rate 
#and export with exchange rate
corr1 = cor(EconData$IMP, EconData$EX)
corr2 = cor(EconData$EXP, EconData$EX)

#linear regression analysis
ra_EXP = lm(EXP ~ EX + GDP_EU + GDP_US + OIL 
            + GOLD + SIL, data = EconData)

ra_IMP = lm(IMP ~ EX + GDP_EU + GDP_US + OIL 
            + GOLD + SIL, data = EconData)

#summary of the regression output
summary(ra_EXP)
summary(ra_IMP)
