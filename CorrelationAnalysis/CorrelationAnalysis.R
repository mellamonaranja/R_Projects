#Correlation Analysis#

#Import the data
corr=read.csv("/Users/joohyunyoon/Downloads/caldis.csv",header = TRUE, na.strings = '.')

View(corr)
str(corr)

attach(corr)

#Get the Descriptive Statistic
library(psych)
describe(corr)

#Get the Correlation Graph
pairs.panels(corr)

#Create the graph
plot(Distance~Calories, data = corr, col=topo.colors(5))
abline(lm(Distance~Calories, data = corr),col='purple', lty=5)

#Analysis Correlation
corr=corr[c(-154:-155),]
cor(corr,use = 'complete.obs',method = c('pearson'))
plot(Distance~Calories,data = corr, col=topo.colors(5))
abline(lm(Distance~Calories, data = corr),col='purple', lty=5)
detach(corr)
