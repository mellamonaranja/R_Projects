#Simple Linear Regression#

#Import the data
regData=read.csv("/Users/joohyunyoon/Downloads/caldis.csv",header = TRUE, na.strings = '.')

View(regData)
str(regData)

#Simple Linear Regression Analysis
regData=regData[c(-154:-155),]
plot(Distance~Calories,data = regData, col=topo.colors(5))
abline(lm(Distance~Calories, data = regData),col='purple', lty=5)

regModel=lm(Distance~Calories, data=regData)
anova(regModel)
summary(regModel)

#Residuals vs Fitted
#Nomal Q-Q
#Scale-Location
#Residuals vs Leverage(cook's distance) 4/n-k-1

opar=par(no.readonly = TRUE)
par(mfrow=c(2,2))
plot(regModel,col=topo.colors(5))
par(opar)

#Residuals Nomal Distriburion
shapiro.test(regModel$residuals)

#Verify Outlier
install.packages('car')
library(car)
influencePlot(regModel,id.method='identify',col=topo.colors(5))

detach(regData)
