library("readxl")
library("tseries")
library(ggplot2)
library(forecast)
library(lubridate)
library(dplyr)
library(scales)
library(csv)
library(plotly)
library(microbenchmark)


#https://rpubs.com/AlgoritmaAcademy/multiseasonality
#https://otexts.com/fpp2/complexseasonality.html
#https://otexts.com/fpp2/components.html
#https://iopscience.iop.org/article/10.1088/1757-899X/352/1/012055/pdf#:~:text=Weather%20can%20be%20predicted%20using,on%20trend%20and%20seasonal%20components.
#http://mercatoelettrico.org/It/download/DatiStorici.aspx


#PREZZO:

mydata19_p<- read_excel("C:/Users/aless/OneDrive/Desktop/AleBicocca/Data Science/Anno 1/Data Science Lab/Project work/Dati/Anno 2019.xlsx",sheet="Prezzi-Prices")
mydata18_p<- read_excel("C:/Users/aless/OneDrive/Desktop/AleBicocca/Data Science/Anno 1/Data Science Lab/Project work/Dati/Anno 2018.xlsx",sheet = "Prezzi-Prices")
mydata17_p<- read_excel("C:/Users/aless/OneDrive/Desktop/AleBicocca/Data Science/Anno 1/Data Science Lab/Project work/Dati/Anno 2017.xlsx",sheet = "Prezzi-Prices")
mydata16_p<-read_excel("C:/Users/aless/OneDrive/Desktop/AleBicocca/Data Science/Anno 1/Data Science Lab/Project work/Dati/Anno 2016.xlsx",sheet = "Prezzi-Prices")
dummies<-read.csv("C:/Users/aless/OneDrive/Desktop/AleBicocca/Data Science/Anno 1/Data Science Lab/Project work/Dati/dummies.csv")
train_dumm=dummies[c(1:1096),]
test_dumm= dummies[c(1097:1116),]


ts16_p=mydata16_p[,c(1,2,3)] 
ts17_p=mydata17_p[,c(1,2,3)]
ts18_p=mydata18_p[,c(1,2,3)]
ts19_p=mydata19_p[,c(1,2,3)]


colnames(ts16_p)=c("Data","Ora","PUN")
ts16_p$Data=as.Date(as.character(ts16_p$Data),format="%Y%m%d")
colnames(ts17_p)=c("Data","Ora","PUN")
ts17_p$Data=as.Date(as.character(ts17_p$Data),format="%Y%m%d")
colnames(ts18_p)=c("Data","Ora","PUN")
ts18_p$Data=as.Date(as.character(ts18_p$Data),format="%Y%m%d")
colnames(ts19_p)=c("Data","Ora","PUN")
ts19_p$Data=as.Date(as.character(ts19_p$Data),format="%Y%m%d")

dataset_p=rbind(ts16_p,ts17_p,ts18_p,ts19_p)

#creiamo datset che contenga solo la 19esima ora e valutiamo le caratteristiche principali della serie:
prezzo19=subset(dataset_p,Ora==19)
tsclean(prezzo19$PUN) #oppure tsoutliers



#vediamo la sedicesima ora di ogni anno:

pz19_16=subset(ts16_p,Ora==19)
pz19_17=subset(ts17_p,Ora==19)
pz19_18=subset(ts18_p,Ora==19)
pz19_19=subset(ts19_p,Ora==19)


#valutiamo la serie intera inizialmente (come autoplot):

ggplot(prezzo19, aes(Data, PUN)) +
  geom_line() +
  theme(panel.border = element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"), panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Data", y = "PUN ???")

#valutiamo l'ora 19 esima anno per anno:

ggplot(pz19_16, aes(Data, PUN)) +
  geom_line() +
  theme(panel.border = element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"), panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Data", y = "PUN ???")
ggplotly()

ggplot(pz19_17, aes(Data, PUN)) +
  geom_line() +
  theme(panel.border = element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"), panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Data", y = "PUN ???")


ggplot(pz19_18, aes(Data, PUN)) +
  geom_line() +
  theme(panel.border = element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"), panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Data", y = "PUN ???")

ggplot(pz19_19, aes(Data, PUN)) +
  geom_line() +
  theme(panel.border = element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"), panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Data", y = "PUN ???")


#creiamo training e test set per poter ottenere poi una previsione:
train_p= prezzo19[c(1:1096),]
test_p= prezzo19[c(1097:1116),]   #primi 20 giorni del 2019


#STLM code:

#https://otexts.com/fpp2/stl.html

#STL significa seasonal and trend decomposition using loess (local regression),dove loess è un metodo per gestire relazioni non lineari.
# loess is a regression technique that uses local weighted regression to fit a smooth curve through points in a sequence, which in our
#case is the Time Series data.
#E' un metodo utilizzato per gestire stagionalità complesse all'interno delle serie storiche
#in fase di forecast si sfrutta il modello ETS (error, trend, seasonal), supportato da STL, a formare STLModel e 
#successivamente un modello arima semplice

#creiamo la serie con doppia stagionalità e plottiamo i suoi grafici:

price=tsclean(msts(prezzo19$PUN,start=c(2016),end=c(2019,21),seasonal.periods =c(7,365.33)))
price  %>% mstl() %>% autoplot() 
#proporremo lambda=0 per tenere conto del fatto che la serie è di tipo economico
#trend crescente

#iniziamo a lavorare su train set, creandone la serie:
price_train=tsclean(msts(train_p$PUN,start=c(2016),end=c(2019),seasonal.periods =c(7,365.33)))
autoplot(price_train)
price_train  %>% mstl() %>% autoplot()

#come per la serie intera, si evincono le stagionalità, le quali non presentano trend crescente, dunque il loro parametro di ets rimane N(None)
#The first panel from top is the original, observed time series. Note that a series with multiplicative effects can often be transformed into one with additive effect through a log transformation.
#The bottom-most (ultimo) panel the error component, which is determined by removing the trend and seasonal figure

#valutiamo i correlogrammi della serie:

setwd("C:/Users/aless/OneDrive/Desktop/AleBicocca/SSE/3 ANNO/serie storiche/temp")
source("correlogrammi.R")
source("seasplot.R")
par(mfrow=c(1,3))
correlogrammi(price_train)
#acf non si annnulla quasi mai
seasplot(price_train,12)

#creiamo serie test:
price_test=tsclean(ts(test_p$PUN,frequency= 7,start=c(2019),end=c(2019,20)))
#chiaramente la serie non va fino al 2022
autoplot(price_test)
price_test  %>% mstl() %>% autoplot() 


#previsione fatta su train:

#stlm takes a time series y, applies an STL decomposition, and models the seasonally adjusted data (time series depurata dall'effetto
#della stagionalità) ,using the
#model passed as modelfunction or specified using method. It returns an object that includes the original 
#STL decomposition and a time series model fitted to the seasonally adjusted data. 
#This object can be passed to the forecast for forecasting.

#purtroppo STL+ETS non funziona con regressori esterni, dunque non si può inserire il comando xreg. Lo faremo nell'arima

#The ETS algorithm is especially useful for datasets with seasonality and other prior assumptions about the data. 
#ETS computes a weighted average over all observations in the input time series dataset as its prediction. The weights are 
#exponentially decreasing over time, rather than the constant weights in simple moving average methods. The weights are dependent 
#on a constant parameter, which is known as the smoothing parameter.

stlm_model_train <- stlm(price_train,lambda = 0,method = "ets",etsmodel = "ZZZ")
f_ets=forecast(stlm_model_train, level = 90,h = 20) 
plot(f_ets)


    
#ets appartiene alla tipologia exponential smoothing


#etsmodel=ZZZ suggerisce di usare un ANN come modello, per cui la parte di remainder risulta essere di tipo moltiplicativo
# e le componenti stagionalità e trend risultano essere nè additive ne moltiplicative.

#effettuiamo check dei residui del modello e check dei valori fittati sul train:
autoplot(price_train, series="Training ETS",ylab="PUN ???",xlab="TIME") +
  autolayer(fitted(stlm_model_train),
            series="Fitted Values ETS") +theme(legend.position = 'bottom')


checkresiduals(stlm_model_train)
shapiro.test(stlm_model_train$residuals) #rifiutata la normalità
#si evince che i residui seguando un random walk a media zero, tuttavia abbiamo problemi con la normalità e con i correlogrammi
par(mfrow=c(1,3))
correlogrammi(stlm_model_train$residuals)
#H0 LB: no correlazione tra residui, che con pvalue basso rifiuto

# i parametri non riescono a gestire del tutto l'autocorrelazione tra errori, infatti alcuni correlogrammi risultano 
#uscire dalle barre di confidenza e LBTest dimostra questo



#valutazione dell'accuracy su train e test:
acc_train=accuracy(f_ets,test=price_train)
acc_test=accuracy(f_ets,test=price_test)
mmp_train=round(acc_train[,c("MAE","MAPE")],2)
mmp_test=round(acc_test[,c("MAE","MAPE")],2)

df_ets=data.frame(mmp_train,mmp_test)
colnames(df_ets)=c("TRAIN","TEST")


#MAE: media aritmetica degli errori presi in valore assoluto
#MAPE: media aritmetica degli errori relativi presi in valore assoluto moltiplicati per cento






#proviamo a condurre l'analisi utilizzando ARIMA come metodo invece che ETS:

#ARIMA model to the de-seasonalized data, forecasts it, and then re-seasonalizes the forecasts. 
#It is not set up to handle model regressors. If you include an xreg term it will use it when fitting the model, 
#but then it will complain when forecasting because it won't know how to deal with future regressors.
#ARIMA è usato prevalentemente per serie stazionarie, ed è uno dei metodi più efficaci in quanto a forecasting delle TS


#previsione fatta su train:

stlm_model_train_a <- stlm(price_train,lambda = 0,method = "arima")
f_ari=forecast(stlm_model_train_a, h = 20)
plot(f_ari)


#se si inseriscono regressori esterni, si va a modellare un arima per gli errori(p,d,q)

#effettuiamo check dei residui del modello e check dei valori fittati sul train:
autoplot(price_train, series="Training ARIMA",ylab="PUN ???",xlab="TIME") +
  autolayer(fitted(stlm_model_train_a),
            series="Fitted Values ARIMA") +theme(legend.position = 'bottom')

checkresiduals(stlm_model_train_a,main='Residuals from STL + ARIMA(2,1,2)')
shapiro.test(stlm_model_train_a$residuals)
#sembra tutto in ordine e molto simile al caso precedente
correlogrammi(stlm_model_train_a$residuals)


#valutazione della predizione:
acc_train_a=accuracy(f_ari,test=price_train)
acc_test_a=accuracy(f_ari,test=price_test)
mmp_train_a=round(acc_train_a[,c("MAE","MAPE")],2)
mmp_test_a=round(acc_test_a[,c("MAE","MAPE")],2)

df_arima=data.frame(mmp_train_a,mmp_test_a)
colnames(df_arima)=c("TRAIN","TEST")


#valori risultano leggermente migliori rispetto a quelli di ETS in termini di errore

#MAE: media aritmetica degli errori presi in valore assoluto
#MAPE: media aritmetica degli errori relativi presi in valore assoluto moltiplicati per cento



#secondo grafico da inserire nella presentazione per entrambi i modelli:

#nella serie storica price_test, inserisici 365 come frequency invece che 7:
price_test=tsclean(ts(test_p$PUN,frequency= 365,start=c(2019),end=c(2019,20)))

stlm_model_train%>%
  forecast(h=20) %>%
  autoplot(xlab="TIME",ylab="PUN ???",xlim=c(2018.80,2019.08),ylim=c(50,150))+autolayer(price_test,series = 'Test Data',lwd=1.1) +theme(legend.position = 'bottom')
 



stlm_model_train_a%>%
  forecast(h=20) %>%
  autoplot(xlab="TIME",ylab="PUN ???",xlim=c(2018.80,2019.08),ylim=c(50,150))+autolayer(price_test,series = 'Test Data',lwd=1.1)+theme(legend.position = 'bottom')


#ricorda di riporre 7 come valore frequency in price_test
price_test=tsclean(ts(test_p$PUN,frequency= 7,start=c(2019),end=c(2019,20)))






