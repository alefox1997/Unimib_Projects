TStickers=read.csv("C:\\Users\\aless\\OneDrive\\Desktop\\AleBicocca\\Data Science\\Anno 2\\Lab Economics\\Forte_economics\\Progetto\\Dati\\TS_tickers.csv")
coppie=read.csv("C:\\Users\\aless\\OneDrive\\Desktop\\AleBicocca\\Data Science\\Anno 2\\Lab Economics\\Forte_economics\\Progetto\\Dati\\coppie.csv")
library(riingo)
library(quantmod)
library(egcm) 
library(ggplot2)
egcm.set.default.pvalue(0.01)
#Coppie cointegrate:(IJK,MDYG),(ABT,STE),(POOL,LOW)
riingo_set_token('e890a696bafd550e1cea3378c5fe0853338f0aa1')
Tit1_prices=riingo_prices(c("RF"), start_date = "2015-01-01", end_date = "2020-12-31", resample_frequency = "daily")
Tit2_prices=riingo_prices(c("NTRS"), start_date = "2015-01-01", end_date = "2020-12-31", resample_frequency = "daily")
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
#applico il logaritmo-opzionale(o normalizzazione o logaritmo)
Tit1_data[,3]=log(Tit1_data[,3])
Tit2_data[,3]=log(Tit2_data[,3])
dfl=rbind(Tit1_data,Tit2_data)
ggplot(dfl, aes(x = date, y = adjClose)) + 
  geom_line(aes(color = ticker), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()
#egcm con logaritmo
res <- egcm(Tit1_data[,3], Tit2_data[,3],log=TRUE)
summary(res)
plot(res)
is.cointegrated(res)


y1=xts(Tit1_data$adjClose,as.Date(Tit1_data$date))
y2=xts(Tit2_data$adjClose,as.Date(Tit2_data$date))
Y_ <- merge(y1,y2)
names(Y_) <- c("RF","NTRS")
if(anyNA(Y_)) 
  Y_ <- na.approx(Y_)
plot(Y_, legend.loc = "bottomleft", main = "Prices")




#######################################################################LS
estimate_mu_gamma_LS <- function(Y, pct_training = 0.7) {
  Tr <- nrow(Y)
  T_trn <- round(pct_training*Tr)
  # LS regression
  ls_coeffs <- coef(lm(Y[1:T_trn, 1] ~ Y[1:T_trn, 2]))
  mod=summary(lm(Y[1:T_trn, 1] ~ Y[1:T_trn, 2]))
  mu <- xts(rep(ls_coeffs[1], Tr), index(Y))
  colnames(mu) <- "mu-LS"
  gamma <- xts(rep(ls_coeffs[2], Tr), index(Y))
  colnames(gamma) <- "gamma-LS"
  return(list(mu = mu, gamma = gamma))
}#mod

LS <- estimate_mu_gamma_LS(Y_)
LS_mu=LS$mu
LS_gamma=LS$gamma
tmp <- cbind(y1, LS_mu + LS_gamma*y2)
colnames(tmp) <- c(colnames(y1), paste("mu + gamma x", colnames(y2)))
plot(tmp[,1],type="l",ylim=c(1.4,3),main="Actual vs Fitted Values")
lines(tmp[,2],type="l",col='red')
addEventLines(xts("", index(tmp[1058])), lwd = 2, col = "blue")




compute_spread <- function(Y, g, m, name = NULL) {
  w_spread <- cbind(1, -g)/cbind(1+g, 1+g)
  spread <- rowSums(Y * w_spread) - m/(1+g)
  colnames(spread) <- name
  return(spread)
}
spread_LS <- compute_spread(Y_, LS$gamma, LS$mu, "LS")
plot(spread_LS)

library(TTR)

generate_Z_score_EMA <- function(spread, n = 120) {
  ## traditional rolling windowed mean and variance
  # first, the mean
  spread.mean <- EMA(spread, n)
  spread.mean <- na.locf(spread.mean, fromLast = TRUE)
  spread.demeaned <- spread - spread.mean
  # second, the variance
  spread.var <- EMA(spread.demeaned^2, n)
  spread.var <- na.locf(spread.var, fromLast = TRUE)
  # finally compute Z-score
  Z.score <- spread.demeaned/sqrt(spread.var)
  return(Z.score)
}

Z_score=generate_Z_score_EMA(spread_LS)
plot(Z_score)
############################################################Bollinger Bands
myBBands <- function (price,n,sd){
  mavg <- SMA(price,n)
  sdev <- rep(0,n)
  N <- nrow(price)
  for (i in (n+1):N){
    sdev[i]<- sd(price[(i-n+1):i])
  }
  up <- mavg + sd*sdev
  dn <- mavg - sd*sdev
  pctB <- (price - dn)/(up - dn)
  output <- cbind(dn, mavg, up, pctB)
  colnames(output) <- c("dn", "mavg", "up", 
                        "pctB")
  return(output)
}

bb <-myBBands(Z_score,n=20,sd=2)
mavg=bb$mavg
lmavg=lag(mavg)
lmavg[1:20]=mean(Z_score)
up=bb$up
down=bb$dn
tail(bb,n=5)
chartSeries(Z_score,
            theme=chartTheme('white'))
addBBands(n=20,sd=2)
up[1:20]=2*sd(Z_score)
down[1:20]=-2*sd(Z_score)
##########################################################

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
      if (Z_score[t] >= lmavg[t]) signal[t] <- 0
      else signal[t] <- signal[t-1]
    } else {  #if we were in a short position
      if (Z_score[t] <= lmavg[t]) signal[t] <- 0
      else signal[t] <- signal[t-1]
    }
  }
  return(signal)
}




pairs_trading <- function(Y, gamma, mu, name = NULL, plot = FALSE) {
  # spread and spread portfolio
  w_spread <- cbind(1, -gamma)/cbind(1+gamma, 1+gamma)
  spread <- rowSums(Y * w_spread) - mu/(1+gamma)
  
  # Z-score
  Z_score <- generate_Z_score_EMA(spread)
  
  # trading signal
  signal <- generate_signal(Z_score, down, up)
  
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
      lines(up, lty = 2)
      print(lines(down, lty = 2)) }
    print(plot(cumprod(1 + portf_return), main = paste("Cum P&L for spread based on", name)))
    print(plot(1+cumsum(portf_return), main = paste("Cum P&L no reinvestment", name)))
  }
  
  return(portf_return)
}


return_LS <- pairs_trading(Y_, LS$gamma, LS$mu, 
                           "LS", plot = TRUE)

















##########################################################ROLLING LS
estimate_mu_gamma_rolling_LS <- function(Y, pct_training = 0.3) {
  Tr <- nrow(Y)
  T_start <- round(pct_training*Tr)
  T_lookback <- 1000 # lookback window length
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


rolling_LS <- estimate_mu_gamma_rolling_LS(Y_)
mu_rolling_LS=rolling_LS$mu
gamma_rolling_LS=rolling_LS$gamma
#parametri=rolling_LS$tuning
#rownames(parametri)=seq(850,1100,by=10)
#plot(parametri,xlab='RMSE',ylab='R-squared',main='Tuning',col=ifelse(row.names(parametri)==1000, "red", "black"),pch=19)
#text(parametri$ind1,parametri$ind2,row.names(parametri), cex=0.6, pos=1, col="black")
par(mfrow=c(1,1))
tmp2 <- cbind(y1, mu_rolling_LS + gamma_rolling_LS*y2)
plot(tmp2[,1],type="l",main="Actual vs fitted")
lines(tmp2[,2],type="l",col='red')  

#tracking di mu e gamma
par(mfrow=c(2,1))
{plot(rolling_LS$mu)
addEventLines(xts("", index(y1[454])), lwd = 2, col = "blue")}
{plot(rolling_LS$gamma)
addEventLines(xts("", index(y1[454])), lwd = 2, col = "blue")}

spread_rolling_LS <- compute_spread(Y_, rolling_LS$gamma, rolling_LS$mu, "rolling-LS")
plot(spread_rolling_LS)


Z_score_rolling=generate_Z_score_EMA(spread_rolling_LS)
plot(Z_score)

############################################################Bollinger Bands
myBBands <- function (price,n,sd){
  mavg <- SMA(price,n)
  sdev <- rep(0,n)
  N <- nrow(price)
  for (i in (n+1):N){
    sdev[i]<- sd(price[(i-n+1):i])
  }
  up <- mavg + sd*sdev
  dn <- mavg - sd*sdev
  pctB <- (price - dn)/(up - dn)
  output <- cbind(dn, mavg, up, pctB)
  colnames(output) <- c("dn", "mavg", "up", 
                        "pctB")
  return(output)
}

bb <-myBBands(Z_score_rolling,n=20,sd=2)
up=bb$up
down=bb$dn
mavg=bb$mavg
lmavg=lag(mavg)
lmavg[1:20]=mean(Z_score_rolling)
tail(bb,n=5)
chartSeries(Z_score_rolling,
            theme=chartTheme('white'))
addBBands(n=20,sd=2)
up[1:20]=2*sd(Z_score_rolling)
down[1:20]=-2*sd(Z_score_rolling)
##########################################################


return_rolling_LS <- pairs_trading(Y_, rolling_LS$gamma, rolling_LS$mu, 
                           "LS", plot = TRUE)








########################################################################FILTRO DI KALMAN
library(KFAS)
estimate_mu_gamma_Kalman <- function(Y) {
  Train <- nrow(Y)
  # init empty variables
  gamma_Kalman_filtering <- mu_Kalman_filtering <- xts(rep(NA, Train), index(Y))
  colnames(mu_Kalman_filtering) <- "mu-Kalman"
  colnames(gamma_Kalman_filtering) <- "gamma-Kalman"
  # Kalman parameters
  Tt <- diag(2)
  Rt <- diag(2)
  Qt <- 1e-5*diag(2)  # state transition variance very small
  Zt <- array(as.vector(t(cbind(1, as.matrix(Y[, 2])))), dim = c(1, 2, Train))  # time-varying
  Ht <- matrix(1e-3)  # observation variance
  # the prior in the code: P1cov = kappa*P1Inf + P1, kappa = 1e7
  init <- estimate_mu_gamma_LS(Y)
  a1 <- matrix(c(init$mu[1], init$gamma[1]), 2, 1)
  P1 <- 1e-5*diag(2)  # variance of initial point
  P1inf <- 0*diag(2)
  # create Kalman model
  model <- SSModel(as.matrix(Y[, 1]) ~ 0 + SSMcustom(Z=Zt, T=Tt, R=Rt, Q=Qt, a1=a1, P1=P1, P1inf=P1inf), H=Ht)
  # run Kalman filtering
  out <- KFS(model)
  mu_Kalman_filtering[] <- out$a[-1, 1]  # a is Kalman filtering (alphahat is Kalman smoothing) (a(T+1)=alphahat(T))
  gamma_Kalman_filtering[] <- out$a[-1, 2]
  # smoothing
  L <- 30
  mu_Kalman_filtering[] <- filter(mu_Kalman_filtering, rep(1, L)/L, sides = 1)
  mu_Kalman_filtering <- na.locf(mu_Kalman_filtering, fromLast = TRUE)
  gamma_Kalman_filtering[] <- filter(gamma_Kalman_filtering, rep(1, L)/L, sides = 1)
  gamma_Kalman_filtering <- na.locf(gamma_Kalman_filtering, fromLast = TRUE)
  return(list(mu = mu_Kalman_filtering, gamma = gamma_Kalman_filtering))
}


Kalman <- estimate_mu_gamma_Kalman(Y_)
plot(Kalman$mu)
plot(Kalman$gamma)
spread_Kalman <- compute_spread(Y_, Kalman$gamma, Kalman$mu, "Kalman")
plot(spread_Kalman)


library(TTR)

generate_Z_score_EMA <- function(spread, n = 120) {
  ## traditional rolling windowed mean and variance
  # first, the mean
  spread.mean <- EMA(spread, n)
  spread.mean <- na.locf(spread.mean, fromLast = TRUE)
  spread.demeaned <- spread - spread.mean
  # second, the variance
  spread.var <- EMA(spread.demeaned^2, n)
  spread.var <- na.locf(spread.var, fromLast = TRUE)
  # finally compute Z-score
  Z.score <- spread.demeaned/sqrt(spread.var)
  return(Z.score)
}

Z_score_Kalman=generate_Z_score_EMA(spread_Kalman)

############################################################Bollinger Bands
myBBands <- function (price,n,sd){
  mavg <- SMA(price,n)
  sdev <- rep(0,n)
  N <- nrow(price)
  for (i in (n+1):N){
    sdev[i]<- sd(price[(i-n+1):i])
  }
  up <- mavg + sd*sdev
  dn <- mavg - sd*sdev
  pctB <- (price - dn)/(up - dn)
  output <- cbind(dn, mavg, up, pctB)
  colnames(output) <- c("dn", "mavg", "up", 
                        "pctB")
  return(output)
}

bb <-myBBands(Z_score_Kalman,n=20,sd=2)
up=bb$up
down=bb$dn
mavg=bb$mavg
lmavg=lag(mavg)
lmavg[1:20]=mean(Z_score_Kalman)
tail(bb,n=5)
chartSeries(Z_score_Kalman,
            theme=chartTheme('white'))
addBBands(n=20,sd=2)
up[1:20]=2*sd(Z_score_Kalman)
down[1:20]=-2*sd(Z_score_Kalman)
##########################################################


pairs_trading <- function(Y, gamma, mu, name = NULL, plot = FALSE) {
  # spread and spread portfolio
  w_spread <- cbind(1, -gamma)/cbind(1+gamma, 1+gamma)
  spread <- rowSums(Y * w_spread) - mu/(1+gamma)
  
  # Z-score
  Z_score <- generate_Z_score_EMA(spread)
  
  # trading signal
  signal <- generate_signal(Z_score, down, up)
  
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
      lines(up, lty = 2)
      print(lines(down, lty = 2)) }
    print(plot(cumprod(1 + portf_return), main = paste("Cum P&L for spread based on", name)))
    print(plot(1+cumsum(portf_return), main = paste("Cum P&L no reinvestment", name)))
  }
  
  return(portf_return)
}


return_LS <- pairs_trading(Y_,Kalman$gamma,Kalman$mu,plot = TRUE)
