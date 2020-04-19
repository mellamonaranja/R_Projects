

#Import the data
runba=read.csv("./run.ba.csv",
               header = TRUE, na.strings = '.')
View(runba)

#Change factor to vector
runba$Date=factor(runba$Date,levels = c(1,2),labels=c('before','after'))
View(runba)
str(runba)

attach(runba)

#Get the Descriptive Statistic
library(psych)
tapply(Date, Distance, summary)
mean(Date, na.rm = TRUE)
describe(Date)
describe(Distance)
describeBy(Distance,Date,mat=TRUE)

#Separate the facet
opar=par(no.readonly = TRUE)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))

#Create the graph(boxplot, histogram)
boxplot(Distance~Date, outlier.color='red', outlier.shape=2)

hist(Distance[Date=='before'],ylim = c(0,10),
     main = 'Distance before Marathon', breaks = 15, 
     col = heat.colors(5),xlab = 'Distance')

hist(Distance[Date=='after'],ylim = c(0,10),
     main = 'Distance after Marathon', breaks = 15, 
     col = heat.colors(5),xlab = 'Distance')

par(opar)

#Analysis Of Variance
var.test(Distance~Date, data = runba)
t.test(Distance~Date,
       data=runba,
       alternative=c('two.sided'),
       var.equal=TRUE, #If two different variance, var.equal=FALSE
       conf.level=0.95)

detach(runba)

#Create Statistic Analysis Graph
x=10.68677
se=0.59
data=rnorm(1000,x,se)
data=sort(data)
plot(data, dnorm(data,x,se),col=heat.colors(5),type='l',main = 'Distance before and after Marathon',xim=c(10.00,11.00),ylim = c(0,1))
abline(v=x,col='red',lty=6)

#Duplicates the graphs
par(new=TRUE)
x=10.64935
se=1.25
data=rnorm(1000,x,se)
data=sort(data)
plot(data, dnorm(data,x,se),col='blue',type='l',main = 'Distance before and after Marathon',xim=c(10.00,11.00),ylim = c(0,1))
abline(v=x,col='blue',lty=6)

#Appendix, Non-parametric statistics, if p>0.05
shapiro.test(runba$Distance[runba$Date=='before'])
shapiro.test(runba$Distance[runba$Date=='after'])

#Appendix, Non-parametric statistics, if p<0.05
wilcox.test(Distance~Date, data=runba)

#Appendix, create the graph
library(ggplot2)
layout(matrix(c(1, 2,2,3), byrow = TRUE))
ggplot(runba, aes(x=Distance))+
    geom_histogram(binwidth = 10,col='white',
                   fill=heat.colors(5),alpha=0.5)+
    ggtitle('Density by Distance')+
    facet_grid(.~Date)

ggplot(runba, aes(x=Distance, colour=Date))+
    geom_density(fill='blue',alpha=0.3)+
    geom_line(stat = 'density')+
    expand_limits(y=0)+
    ggtitle('Density by Distance')
