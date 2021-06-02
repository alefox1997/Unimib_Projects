#primo corrplot
test1=test[,c(2,3,4,5,6,22)]
names(test1)[names(test1) == 'Valutazione.1'] <- 'Valutazione'
test1$Valutazione=as.numeric(gsub(",", ".", test1$Valutazione))
M1=cor(test1)
library(corrplot)
corrplot(M1,type='lower',method='color', addCoef.col = "black", number.digits = 2, number.cex = 0.75)

#secondo corrplot 
test2=test[,c(7,8,9,10,11,23)]
colnames(test2)=colnames(test1)
test2$Valutazione=as.numeric(gsub(",", ".", test1$Valutazione))
M2=cor(test2)
library(corrplot)
corrplot(M2,type='lower',method='color', addCoef.col = "black", number.digits = 2, number.cex = 0.75)

#terzo corrplot 
test3=test[,c(12,13,14,15,16,24)]
colnames(test3)=colnames(test1)
test3$Valutazione=as.numeric(gsub(",", ".", test1$Valutazione))
M3=cor(test3)
library(corrplot)
corrplot(M3,type='lower',method='color', addCoef.col = "black", number.digits = 2, number.cex = 0.75)

#quarto corrplot
test4=test[,c(17,18,19,20,21,25)]
colnames(test4)=colnames(test1)
test4$Valutazione=as.numeric(gsub(",", ".", test4$Valutazione))
M4=cor(test4)
library(corrplot)
corrplot(M4,type='lower',method='color', addCoef.col = "black", number.digits = 2, number.cex = 0.75)
