TStickers=read.csv("C:\\Users\\aless\\OneDrive\\Desktop\\AleBicocca\\Data Science\\Anno 2\\Lab Economics\\Forte_economics\\Progetto\\Dati\\TS_tickers.csv")
coppie=read.csv("C:\\Users\\aless\\OneDrive\\Desktop\\AleBicocca\\Data Science\\Anno 2\\Lab Economics\\Forte_economics\\Progetto\\Dati\\coppie.csv")
library(riingo)
library(quantmod)
library(egcm) 
library(ggplot2)
egcm.set.default.pvalue(0.01)
#Coppie cointegrate:(IJK,MDYG),(ABT,STE),(POOL,LOW)
riingo_set_token('e890a696bafd550e1cea3378c5fe0853338f0aa1')
Tit1_prices=riingo_prices(c("ABT"), start_date = "2015-01-01", end_date = "2020-12-31", resample_frequency = "daily")
Tit2_prices=riingo_prices(c("STE"), start_date = "2015-01-01", end_date = "2020-12-31", resample_frequency = "daily")
Tit1_data=Tit1_prices[,c(1,2,8)]
Tit2_data=Tit2_prices[,c(1,2,8)]
df=rbind(Tit1_data,Tit2_data)
# Multiple line plot
ggplot(df, aes(x = date, y = adjClose)) + 
  geom_line(aes(color = ticker), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()
#Cointegration tests
res <- egcm(Tit1_data[,3], Tit2_data[,3])
summary(res)
is.cointegrated(res)
plot(res)




#TENTATIVO LS CON ABT E STE
Tr=nrow(Tit1_data)
T_trn=round(0.7*Tr)
y1=Tit1_data[,3]
y2=Tit2_data[,3]
y1T=y1[1:T_trn,]
y2T=y2[1:T_trn,]
ls_coeffs <-coef(lm(y1T$adjClose~ y2T$adjClose))
ls_coeffs
mu=ls_coeffs[1]
gamma=ls_coeffs[2]

tmp <- cbind(y1, mu + gamma*y2)
colnames(tmp) <- c(colnames(y1), paste("mu + gamma x", colnames(y2)))
plot(tmp$adjClose,type="l")
lines(tmp[,2],type="l",col='red')
abline(v=nrow(y1T))
spread <-cbind(y1 - gamma*y2,Tit1_data$date)
plot(spread$adjClose,type='l')
abline(v=nrow(y1T))


#TRADING THE SPREAD
days=nrow(Tit1_data)
rownames(y1)=as.Date(Tit1_data$date)
y1=xts(y1,as.Date(Tit1_data$date))
y1
w_ref <- c(1, -gamma)/(1+gamma)
sum(abs(w_ref))
w_spread <- matrix(w_ref, days, 2, byrow = TRUE)
library(xts)
w_spread <- xts(w_spread,index(y1))
colnames(w_spread) <- c("w1", "w2")
w_spread
rownames(y2)=Tit2_data$date
y2=xts(y2,as.Date(Tit2_data$date))
# resulting normalized spread
spread <- rowSums(cbind(y1, y2) * w_spread)
spread <- xts(spread,index(y1))
colnames(spread) <- "spread"
plot(spread)
{ plot(spread, main = "Spread (from normalized portfolio)")
  addEventLines(xts("", index(y1[T_trn])), lwd = 2, col = "blue") }

#inseriamo bande nel punto della deviazione stadard dello z-score
spread_mean <- mean(spread[1:T_trn], na.rm = TRUE)
spread_var <- as.numeric(var(spread[1:T_trn], na.rm = TRUE))
Z_score <- (spread-spread_mean)/sqrt(spread_var)
colnames(Z_score) <- "Z-score"
threshold_long <- threshold_short <- Z_score
threshold_short[] <- 1
threshold_long[] <- -1




par(mfrow=c(2,1))
{ plot(Z_score, main = "Z-score")
  lines(threshold_short, lty = 2)
  lines(threshold_long, lty = 2) 
  addEventLines(xts("", index(y1[T_trn])), lwd = 2, col = "blue") }

#INSERIMENTO DEL SEGNALE
# we define a function for convenience and future use
generate_signal <- function(Z_score, threshold_long, threshold_short) {
  signal <- Z_score
  colnames(signal) <- "signal"
  signal[] <- NA
  
  #initial position
  signal[1] <- 0
  if (Z_score[1] <= threshold_long[1]) {
    signal[1] <- 1
  } else if (Z_score[1] >= threshold_short[1])
    signal[1] <- -1
  
  # loop
  for (t in 2:nrow(Z_score)) {
    if (signal[t-1] == 0) {  #if we were in no position
      if (Z_score[t] <= threshold_long[t]) {
        signal[t] <- 1
      } else if(Z_score[t] >= threshold_short[t]) {
        signal[t] <- -1
      } else signal[t] <- 0
    } else if (signal[t-1] == 1) {  #if we were in a long position
      if (Z_score[t] >= 0) signal[t] <- 0
      else signal[t] <- signal[t-1]
    } else {  #if we were in a short position
      if (Z_score[t] <= 0) signal[t] <- 0
      else signal[t] <- signal[t-1]
    }
  }
  return(signal)
}

# now just invoke the function
signal <- generate_signal(Z_score, threshold_long, threshold_short)
signal

{ plot(cbind(Z_score, signal), main = "Z-score and trading signal", legend.loc = "topleft")}
abline(v=1058)

#profit and loss strategy
spread_return <- diff(spread)
traded_return <- spread_return * lag(signal)   # NOTE THE LAG!!
traded_return[is.na(traded_return)] <- 0
colnames(traded_return) <- "traded spread"
par(mfrow=c(1,1))

{ plot(traded_return, main = "Return of traded spread")
addEventLines(xts("", index(y1[T_trn])), lwd = 2, col = "blue") }


{ plot(1 + cumsum(traded_return), main = "Cum P&L of traded spread (no reinvestment)")
  addEventLines(xts("", index(y1[T_trn])), lwd = 2, col = "blue") }

library(PerformanceAnalytics)
{ chart.CumReturns(traded_return, main = "Cum P&L of traded spread (w/ reinvestment)", 
                   geometric = TRUE, wealth.index = TRUE)
  addEventLines(xts("", index(y1[T_trn])), lwd = 2, col = "blue") }

#TUNING ROLLING LS
#estimate_mu_gamma_rolling_LS <- function(Y, pct_training = 0.3) {
  #Tr<- nrow(Y)
  #T_start <- round(pct_training*Tr)
  #T_lookback <- seq(850,1100,by=10)#lookback window length
  #T_shift <- 20#how often is refreshed
  # init empty variables
  #gamma_rolling_LS <- mu_rolling_LS <- xts(rep(NA, Tr), index(Y))
  #colnames(mu_rolling_LS) <- "mu-rolling-LS"
  #colnames(gamma_rolling_LS) <- "gamma-rolling-LS"
  #ind1 <- c()
  #ind2 <- c()
  # loop
  #count=0
  #for (val in T_lookback){
    #count=count+1
    #rmse=c()
    #R2=c()
    #t0_update <- seq(from = min(T_start, val), to = Tr-T_shift, by = T_shift)
    #i=0
    #for (t0 in t0_update) {
      #i=i+1
      #T_lookback_ <- ifelse(t0-val+1 >= 1, val, T_start)
      #ls_coeffs <- coef(lm(Y[(t0-T_lookback_+1):t0, 1] ~ Y[(t0-T_lookback_+1):t0, 2],
                           #weights = last(1:val, T_lookback_)))
      #reg=lm(Y[(t0-T_lookback_+1):t0, 1] ~ Y[(t0-T_lookback_+1):t0, 2],
             #weights = last(1:val, T_lookback_))
      #mod=summary(reg)
      #R2[i]=mod$r.squared
      #rmse[i]=sqrt(mean(reg$residuals^2))
      #mu_rolling_LS[t0+1] <- ls_coeffs[1]
      #gamma_rolling_LS[t0+1] <- ls_coeffs[2]
    #}
    #mrmse=round(mean(rmse),3)
    #mrquadro=round(mean(R2),3)
    #ind1[count]=mrmse
    #ind2[count]=mrquadro
  #}
  #df=data.frame(ind1,ind2)
  # complete values
  #mu_rolling_LS <- na.locf(mu_rolling_LS)
  #mu_rolling_LS <- na.locf(mu_rolling_LS, fromLast = TRUE)
  #gamma_rolling_LS <- na.locf(gamma_rolling_LS)
  #gamma_rolling_LS <- na.locf(gamma_rolling_LS, fromLast = TRUE)
  # smoothing
  #L <- 15
  #mu_rolling_LS[] <- filter(mu_rolling_LS, rep(1, L)/L, sides = 1)
  #mu_rolling_LS <- na.locf(mu_rolling_LS, fromLast = TRUE)
  #gamma_rolling_LS[] <- filter(gamma_rolling_LS, rep(1, L)/L, sides = 1)
  #gamma_rolling_LS <- na.locf(gamma_rolling_LS, fromLast = TRUE)
  #return(list(tuning=df))
#}

estimate_mu_gamma_rolling_LS <- function(Y, pct_training = 0.3) {
  Tr <- nrow(Y)
  T_start <- round(pct_training*Tr)
  T_lookback <- 1000  # lookback window length
  T_shift <- 20  # how often is refreshed
  # init empty variables
  gamma_rolling_LS <- mu_rolling_LS <- xts(rep(NA, Tr), index(Y))
  colnames(mu_rolling_LS) <- "mu-rolling-LS"
  colnames(gamma_rolling_LS) <- "gamma-rolling-LS"
  # loop
  t0_update <- seq(from = min(T_start, T_lookback), to = Tr-T_shift, by = T_shift)
  for (t0 in t0_update) {
    T_lookback_ <- ifelse(t0-T_lookback+1 >= 1, T_lookback, T_start)
    ls_coeffs <- coef(lm(Y[(t0-T_lookback_+1):t0, 1] ~ Y[(t0-T_lookback_+1):t0, 2],
                         weights = last(1:T_lookback, T_lookback_)))
    mu_rolling_LS[t0+1] <- ls_coeffs[1]
    gamma_rolling_LS[t0+1] <- ls_coeffs[2]
  }
  # complete values
  mu_rolling_LS <- na.locf(mu_rolling_LS)
  mu_rolling_LS <- na.locf(mu_rolling_LS, fromLast = TRUE)
  gamma_rolling_LS <- na.locf(gamma_rolling_LS)
  gamma_rolling_LS <- na.locf(gamma_rolling_LS, fromLast = TRUE)
  # smoothing
  L <- 15
  mu_rolling_LS[] <- filter(mu_rolling_LS, rep(1, L)/L, sides = 1)
  mu_rolling_LS <- na.locf(mu_rolling_LS, fromLast = TRUE)
  gamma_rolling_LS[] <- filter(gamma_rolling_LS, rep(1, L)/L, sides = 1)
  gamma_rolling_LS <- na.locf(gamma_rolling_LS, fromLast = TRUE)
  return(list(mu = mu_rolling_LS, gamma = gamma_rolling_LS))
}




#creo un oggetto xts con due colonne che sono le serie storiche end-of-days dei due titoli
Y_ <- merge(y1,y2)
names(Y_) <- c("ABT","STE")
if(anyNA(Y_)) 
  Y_ <- na.approx(Y_)
plot(Y_, legend.loc = "bottomleft", main = "Prices")

#Stimo i parametri rolling-LS su tutto periodo considerato
rolling_LS <- estimate_mu_gamma_rolling_LS(Y_)
mu_rolling_LS=rolling_LS$mu
gamma_rolling_LS=rolling_LS$gamma
#parametri=rolling_LS$tuning
#rownames(parametri)=seq(850,1100,by=10)
#plot(parametri,xlab='RMSE',ylab='R-squared',main='Tuning',col=ifelse(row.names(parametri)==1000, "red", "black"),pch=19)
#text(parametri$ind1,parametri$ind2,row.names(parametri), cex=0.6, pos=1, col="black")
tmp <- cbind(y1, mu_rolling_LS + gamma_rolling_LS*y2)
colnames(tmp) <- c(colnames(y1), paste("mu + gamma x", colnames(y2)))
plot(tmp$adjClose,type="l",main="Actual vs Fitted Values")
lines(tmp[,2],type="l",col='red')  



#I due grafici contengono il tracciamento delle stime di mu e gamma nel tempo
par(mfrow=c(2,1))
{ plot(rolling_LS$mu, 
       legend.loc = "left", main = "Tracking of mu")
  addEventLines(xts("", index(Y_[round(0.3*nrow(Y_))])), lwd = 2, col = "blue") }
{ plot( rolling_LS$gamma, 
       legend.loc = "left", main = "Tracking of gamma")
  addEventLines(xts("", index(Y_[round(0.3*nrow(Y_))])), lwd = 2, col = "blue") }


#A questo punto si calcola lo spread per rolling-LS
compute_spread <- function(Y, g, m, name = NULL) {
  w_spread <- cbind(1, -g)/cbind(1+g, 1+g)
  spread <- rowSums(Y * w_spread) - m/(1+g)
  colnames(spread) <- name
  return(spread)
}
par(mfrow=c(1,1))
spread_rolling_LS <- compute_spread(Y_, rolling_LS$gamma, rolling_LS$mu, "rolling-LS")
plot(spread_rolling_LS,legend.loc = "topleft", main = "Spreads")




#Si usa una funzione per generare lo spread normalizzato, Z-score
library(TTR)

generate_Z_score_EMA <- function(spread, n = 120) {
  spread.mean <- EMA(spread, n)
  spread.mean <- na.locf(spread.mean, fromLast = TRUE)
  spread.demeaned <- spread - spread.mean
  spread.var <- EMA(spread.demeaned^2, n)
  spread.var <- na.locf(spread.var, fromLast = TRUE)
  Z.score <- spread.demeaned/sqrt(spread.var)
  return(Z.score)
}


#si decide di introdurre delle bande fisse per generare il segnale, localizzate a +- 1.5* sd(z_score)
Z_score=generate_Z_score_EMA(spread_rolling_LS)
tres=1.5*sd(Z_score)

#Questa funzione genera il trading signal e computa il P&L
pairs_trading <- function(Y, g, m, name = NULL, threshold =tres , plot = FALSE) {
  # spread and spread portfolio
  w_spread <- cbind(1, -g)/cbind(1+g, 1+g)
  spread <- rowSums(Y * w_spread) - m/(1+g)
  
  # Z-score
  Z_score <- generate_Z_score_EMA(spread)
  threshold_long <- threshold_short <- Z_score
  threshold_short[] <- threshold
  threshold_long[] <- -threshold
  
  # trading signal
  signal <- generate_signal(Z_score, threshold_long, threshold_short)
  
  # combine the ref portfolio with trading signal
  w_portf <- w_spread * lag(cbind(signal, signal))   # NOTE THE LAG!!
  
  # # fix the portfolio (gamma and mu) during a trade
  # lag_signal <- as.numeric(lag(signal))
  # for (t in 2:nrow(w_portf)) {
  #   if (lag_signal[t] != 0 && lag_signal[t] == lag_signal[t-1])
  #     w_portf[t, ] <- w_portf[t-1, ]
  # }
  
  # now compute the PnL from the log-prices and the portfolio
  X <- diff(Y)  #compute log-returns from log-prices
  portf_return <- xts(rowSums(X * w_portf), index(X))
  portf_return[is.na(portf_return)] <- 0
  colnames(portf_return) <- name
  
  # plots
  if (plot) {
    tmp <- cbind(Z_score, signal)
    colnames(tmp) <- c("Z-score", "signal")
    par(mfrow = c(3, 1))
    { plot(tmp, legend.loc = "topleft",
           main = paste("Z-score and trading on spread based on", name))
      lines(threshold_short, lty = 2)
      print(lines(threshold_long, lty = 2)) }
    print(plot(1+cumsum(portf_return), main = paste("Cum P&L no reinvestment", name)))#P&L no reinvestment
    print(plot(cumprod(1 + portf_return), main = paste("Cum P&L for spread based on", name)))#P&L with reinvestment
  }
  
  return(portf_return)
}



return_rolling_LS <- pairs_trading(Y_, rolling_LS$gamma, rolling_LS$mu, 
                                   "rolling-LS", plot = TRUE)







