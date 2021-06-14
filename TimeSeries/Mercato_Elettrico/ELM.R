library(nnfor)


install.packages("remotes")
remotes::install_github("ahaeusser/tscv")

source("correlogrammi.R")
library(tseries)
library(forecast)


setwd("/home/napo/Documents/DSLAB/")

testandtrain1 <- read.csv("prezziore19_1619.csv")
subsettt1 <- subset(x = testandtrain1,select = c(6))

giorni1 <- 1:1461
subsettt1 <- data.frame(giorni,subsettt)
colnames(subsettt)<-c("GIORNO","PUN")

dummies <- read.csv("dummies.csv")
subsettt <- merge(subsettt,dummies,by.x=c("GIORNO"),by.y = "X")



trainingset <- subsettt[c(1:1095),]
testset <- subsettt[c(1096:1116),] 
trainingetestset <- subsettt[c(1:1116),]

traints_l <- tsclean(msts(trainingset$PUN,seasonal.periods = c(7,365)))
testts <- tsclean(ts(testset$PUN))

#traints <- tsclean(ts(trainingset$PUN))
#testts <- tsclean(ts(testset$PUN))



  
elm.fit<-elm(lags = c(7),traints_2, difforder = 0, 
             xreg = cbind(dummies=dummies$dummies),
             xreg.lags = list(0),
             xreg.keep = list(TRUE),
             comb = 'mean')
             
print(elm.fit)
plot(elm.fit)

elm.frc<-forecast(elm.fit,xreg=cbind(dummies=dummies$dummies),PI=F)

plot(elm.frc)
tsfrc <-ts(elm.frc)
accuracy(elm.frc,testts_2)
lines(seq(4,5,by=4/1096),as.vector(testts_2[1:275]),col='red',lwd=2)



autoplot(ts(traints_2), series="Training ETS",ylab="PUN ???",xlab="TIME") +
  autolayer(ts(fitted(elm.fit)),
            series="fitted values ETS")

length(elm.fit$fitted)

library(tsutils)

residual11 <- residout(traints - elm.fit$fitted, outplot = F)


correlogrammi(residual11$residuals)

   library(forecast)
checkresiduals(elm.frc)

residuals(object = elm.fit$fitted)
shapiro.test(modello_train$residuals) 
 
  res = residual()
 
  #plot the results
ylim <- c(min(20), max(140))
xlim <- c(min(1050),max(1116))
plot(trainingetestset$PUN, col="blue", ylim=ylim, xlim=xlim, type="l")
par(new=TRUE)
plot(elm.frc$mean, col="red", ylim=ylim, xlim=xlim,type="l")
par(new=TRUE)
plot(trainingset$PUN, col="green", ylim=ylim, xlim=xlim, type="l")



 
