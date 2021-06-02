#si importa il file dall'enviroment
#nella prima fase si legge analizza il dataset e lo si rende utilizzabile per le prossime analisi
options(scipen = 999)
auto=car.data
library(funModeling)
df_status(auto)
attach(auto)
auto$Years=2018-auto$Year
auto$Price_difference= auto$Present_Price - auto$Selling_Price
auto$Owner=as.factor(auto$Owner)
levels(auto$Owner)=c('Prima','Seconda','Quarta')
which(auto$Kms_Driven==500000) #500000 è errore di digitazione, quindi si corregge con 50000
auto[197,]$Kms_Driven=50000
auto2=auto[,-c(1,2)]
desc=df_status(auto2)
which(auto$Year==2018)
detach(auto)
attach(auto2)

#ora si può iniziare ad effettuare qualche analisi statistica di tipo descrittivo

#qualitative nominali, Fuel_type e Transmission
t1=table(Fuel_Type)*100/length(Fuel_Type)  #si evince un numero maggiore di macchine a benzina
table(Transmission)*100/length(Transmission) #numero maggiore nettamente di macchine a cambio manuale
table(Seller_Type)*100/length(Seller_Type) #maggior numero di commercianti
table(Owner)*100/length(Owner)#maggior parte di vetture di prima mano
#in questo modo ottengo le percentuali relative alle modalità delle factor presenti nel dataset
C=c("red","blue","green")
lbls=c("CNG","Diesel","Petrol")
pct=c(0.66,19.93,79.40)
lbls=paste(lbls,pct)
lbls=paste(lbls,"%",sep="")
pie(t1,labels=lbls,main="Fuel Type percentage",col=C)
which(Fuel_Type=='CNG') #solo 2 obs

#vogliamo fare l'incorcio con transmission per vedere se c'è connessione quindi ricodifico la variabile Years
auto2$Age[auto2$Years<=4] = "bassa"
auto2$Age[auto2$Years>4 & auto2$Years<=10] = "media"
auto2$Age[auto2$Years>10] = "alta"
auto2$Age=factor(auto2$Age)
auto2$Age=ordered(auto2$Age,levels=c("bassa","media","alta"))

df_status(auto2)
attach(auto2)
tabella=table(auto2$Age,auto2$Transmission)
tabella
chisq.test(tabella)    # trova chi square
chi=chisq.test(tabella)# create an object
chi$statistic        # invocalo dall' oggetto
chi_norm<-chi$statistic/(nrow(auto2)*min(nrow(tabella)-1,ncol(tabella)-1))
chi_norm  # chi quadrato/chi quadrato max
# il test chi quadro ci induce a non rifiutare l'ipotesi nulla di assenza di connessione tra Transmission e Age
prop.table(tabella)#percentuale di ogni cella sul totale
prop.table(tabella,1)#frequenze percentuali di Y condizionato a X
prop.table(tabella,2)#viceversa
#facciamo un barplot condizionato
barplot(t(tabella),main="Age by Transmission",col=c("red","blue"),legend.text=c("Automatic","Manual"))
#il grafico conferma il chi test. Non sembra esserci una connessione.
#costruiamo uno scatterplot condizionato tra kms driven, price_difference condizionato alla variabile age

c1=c("red","black","green")
plot(Kms_Driven,Price_difference*1000,col=c1[Age],xlim=c(0,240000),ylim=c(0,25000),ylab="Price_difference")
legend("topright", legend = levels(Age),pch=21,col=c1)
quantile(Price_difference,probs=c(0.9))
abline(h=7350)
par(mfrow=c(2,2))
bas=subset(auto2,auto2$Age=="bassa")
med=subset(auto2,auto2$Age=="media")
alt=subset(auto2,auto2$Age=="alta")
plot(bas$Kms_Driven,bas$Price_difference*1000,xlim=c(0,240000),ylim=c(0,25000),ylab="Price_difference",data=bas,main='Age=bassa',xlab='Kms_driven')
abline(modbas)
plot(alt$Kms_Driven,alt$Price_difference*1000,xlim=c(0,240000),ylim=c(0,25000),ylab="Price_difference",data=alt,main='Age=alta',xlab='Kms_driven')
abline(modalt)
plot(med$Kms_Driven,med$Price_difference*1000,xlim=c(0,240000),ylim=c(0,25000),ylab="Price_difference",data=med,main='Age=media',xlab='Kms_driven')
abline(modmed)

plot(med[-58,]$Kms_Driven,med[-58,]$Price_difference*1000,xlim=c(0,240000),ylim=c(0,25000),ylab="Price_difference",xlab='Kms_driven senza punto influente',data=med,main='Age=media')
abline(modmed2,col=2)

#sembra esserci una lieve correlazione positiva tra la differenza di prezzo e i kilometri percorsi. Inoltre si può
modalt=lm(Price_difference*1000~Kms_Driven, data=alt)
summary(modalt)
modmed=lm(Price_difference*1000~Kms_Driven, data=med)
summary(modmed)
which(med$Kms_Driven>200000)
modmed2=lm(Price_difference*1000~Kms_Driven, data=med[-58,])
summary(modmed2)
modbas=lm(Price_difference*1000~Kms_Driven, data=bas)
summary(modbas)
cor(med$Price_difference,med$Kms_Driven)
cor(alt$Price_difference,alt$Kms_Driven)
#notare che le auto che hanno percorso più kilometri hanno età media o alta.le differenze di prezzo maggiori si notano 
#in auto di età media o alta. 



#quantitative discrete, con pochi valori (Years), vediamo percentuali e grafici

table(Years)*100/length(Years)
library(car)
boxplot(Years, ylab="Age",main='Anni delle vetture') #con questo boxplot vengono riportati gli outliers anche
#questo comando ci mostra la divisione percentale dei valori relativi agli anni delle auto
summary(Years)#media è maggiore di mediana dunque abbiamo distribuzione leggermente asimmetrica positivamente per la variabile Years
skewness(Years)
skewness(Kms_Driven)
skewness(Selling_Price)
summary(Present_Price)
skewness(Present_Price)
#quantitative continue (Kmdrivens,selling price)
max(Kms_Driven)
min(Kms_Driven)
table(cut(Kms_Driven, breaks=c(0,10000,25000,40000,75000,220000)))
hist(Kms_Driven)
boxplot( Kms_Driven, ylab="Kms_driven",main='Km percorsi dalle vetture',ylim=c(0,240000)) #errore di digitazione 500000 km?

max(Selling_Price)
min(Selling_Price)
table(cut(Selling_Price, breaks=c(0,2.5,5,15,25,35)))
boxplot(Selling_Price*1000, ylab="Selling_Price",main='Valore attuale delle auto') #in migliaia di euro
hist(Selling_Price)
summary(Selling_Price) #asimmetria positiva
hist(Years,main='anni delle vetture')
hist(Present_Price,main='')

#effettuiamo un boxplot condizionato con una qualitativa e una quantitativa

boxplot(Present_Price~Transmission,col=rainbow(3),ylab="Present_Price",xlab="Transmission",main="Boxplot comparing Present_Price by Transmission")
boxplot(Present_Price*1000~Fuel_Type,col=rainbow(3),ylab="Present_Price",xlab="Fuel_Type",main="Boxplot comparing Present_Price by Fuel_Type")
die=subset(auto2,Fuel_Type=="Diesel")
mean(die$Present_Price)
median(die$Present_Price)
#asimmetria positiva
pet=subset(auto2,Fuel_Type=='Petrol')
mean(pet$Present_Price)
median(pet$Present_Price)
#asimmetria positiva
library(moments)
skewness(die$Present_Price)
skewness(pet$Present_Price)
#petrol risulta più simmetrica di diesel

#vediamo quale delle variabili quantitative risulta la più variabile in termini percentuali, mediante il calcolo del cv
#years
cv1=round((mean(Years)/sd(Years)),3)
#selling price
cv2=round((mean(Selling_Price)/sd(Selling_Price)),3)
#present price
cv3=round((mean(Present_Price)/sd(Present_Price)),3)
#Kms drivem
cv4=round((mean(Kms_Driven)/sd(Kms_Driven)),3)
cv=c(cv1,cv2,cv3,cv4)
cv #dunque la più variabile tra le variabili quantitative risulta essere la variabile Years

#valutiamo un po' di medie condizionate
#present price by transmission
attach(auto2)
means <- aggregate(Present_Price, by=list(Transmission), mean)
means #medie condizionate differenti di prezzo, non si riscontra indipendenza in media

#present price by fuel type
means2 <- aggregate(Present_Price, by=list(Fuel_Type), mean)
means2 #no indipenenza in media

means3 <- aggregate(Kms_Driven, by=list(Age), mean)
means3 #no indipendenza in media, eta quadro=0 in quanto le medie relative ai km guidati, condizionate
#alla variabile eta, sono differenti tra loro

#effettuiamo la stima intervallare per la media della variabile target Selling_Price con un grado di fiducia del 95%
#i dati non provengono da una distribuzione normale e sigma è ignota, si utilizza quindi la varianza campionaria 
#R utilizza di default la varianza campionaria
attach(auto2)
S=sqrt(var(Selling_Price))
xmedio=mean(Selling_Price)
q=qnorm(0.975,0,1)
l1=xmedio-(q*(S/sqrt(301)))
l2=xmedio+(q*(S/sqrt(301)))
confintmu=c(l1,l2)
confintmu


#effettuiamo la stima intervallare per la proporzione di auto con rivenditore Individual con un grado di fiducia di 0.95
table(auto2$Seller_Type=="Individual")
prop=106/301
S2=sqrt(prop*(1-prop))
lp1=prop-(q*(S2/sqrt(301)))
lp2=prop+(q*(S2/sqrt(301)))

confintp=c(lp1,lp2)
confintp


#INFERENZA
#test per la differenza tra medie di due popolazioni
attach(auto2)
man=subset(auto2,Transmission=="Manual")
vettman=man$Selling_Price              

automatic = subset(auto2,Transmission=="Automatic") 
vettauto=automatic$Selling_Price               

t.test(vettman,vettauto)

#il pvalue ci fa rifiutare l'ipotesi nulla di differenza tra le medie uguale a zero

#ANOVA
av1 = aov(Selling_Price ~ Fuel_Type, auto2) 
summary(av1)
#il p-value ci fa rifiutare l'ipotesi nulla di uguaglianza tra le medie di ciascun gruppo, quindi esiste almeno una media diversa
#in teoria il test anova va effettuato con dei blocchi di osservazioni provenienti da distribuzione normale, ma anche nell'esempio della prof 
#la variabile salary non ha distribuzione normale


#OLS
#eliminiamo le variabili che non ci servono dal dataset con variabili standardizzate, togliamo Age e Price_Difference


#osserviamo la matrice di correlazione tra le variabili esplicative per effettuare qualche analisi primaria sulla possibile presenza di multicollinearità tra variabili quantitative
datanum=auto2[,c(2,3,8)]
C=cor(datanum)
library(corrplot)
corrplot(C,method="number",col=c(1:10))
#regressione lineare multipla
auto2$Selling_Price=auto2$Selling_Price*1000
auto2$Present_Price=auto2$Present_Price*1000
attach(auto2)
mod=lm(Selling_Price~Present_Price+Kms_Driven+Fuel_Type+Seller_Type+Transmission+Owner+Years,data=datafin)
summary(mod)
mod2=lm(Selling_Price~Present_Price+Kms_Driven+Fuel_Type+Seller_Type+Transmission+Owner+Years,data=auto2)
summary(mod2)

#residui standardizzati
par(mfrow=c(1,1))
plot(mod2)

#Normal Probability plot dei residui
qqnorm(mod2.stdres, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="QQnorm") 
qqline(mod2.stdres) 

library(car)
soglia=2
vif=sqrt(vif(mod2))
#nessuna variabile ha una radice quadrata del vif>2 quindi non cè nessun problema di multicollinearità
