#Paired Sample T-Test#

#Import the data
cadencesw=read.csv("C:/cadence.csv",header = TRUE, na.strings = '.')
View(cadencesw)
str(cadencesw)

attach(cadencesw)

#Get the Descriptive Statistic
library(psych)
describe(cadencesw)

#Get the difference between summer and winter
dif=c(winter-summer)
describe(dif)

#Separate the facet
opar=par(no.readonly = TRUE)
layout(matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE))

#Create the graph(histogram, boxplot)
hist(summer,main = 'Summer Cadence',col = topo.colors(5),xlab = 'Cadence',ylim = c(0,25))
hist(winter,main = 'Winter Cadence',col = topo.colors(5),xlab = 'Cadence',ylim = c(0,25))
boxplot(dif,main='Difference between Summer and Winter',outlier.shape=2)

#Statistic Analysis
t.test(summer,winter,alternative = c('two.sided'),
       paired = TRUE,
       conf.level = 0.95)

#Create Statistic Analysis Graph
mu=0
se=2.58
data=rnorm(1000,mu,se)
data=sort(data)
plot(data,dnorm(data,mu,se),type = 'l',main = 'Difference between Summer and Winter',xlim = c(-10,10),col='pink')
abline(v=mu,col='blue',lty=5)
abline(v=mu+1.96*se, col="purple", lty=5)
abline(v=mu-1.96*se, col="green", lty=5)
abline(v=-6.30, col="red", lty=5)

#Appendix, Hypothesis : Summer cadence might smaller than Winter

t.test(summer,winter,
       alternative = c('less'),
       paired = TRUE,
       conf.level = 0.95)

mu=0
se=2.58
data=rnorm(1000,mu,se)
data=sort(data)
plot(data,dnorm(data,mu,se),type = 'l',main = 'Difference between Summer and Winter',xlim = c(-10,10),col='pink')
abline(v=mu,col='blue',lty=5)
abline(v=mu-1.645*se, col="purple", lty=5)
abline(v=-6.30, col="red", lty=5)
