TStickers=read.csv("C:\\Users\\aless\\OneDrive\\Desktop\\AleBicocca\\Data Science\\Anno 2\\Lab Economics\\Forte_economics\\Progetto\\Dati\\TS_tickers.csv")
coppie=read.csv("C:\\Users\\aless\\OneDrive\\Desktop\\AleBicocca\\Data Science\\Anno 2\\Lab Economics\\Forte_economics\\Progetto\\Dati\\coppie.csv")
library(riingo)
library(quantmod)
library(egcm) 
library(ggplot2)
egcm.set.default.pvalue(0.01)
#Coppie cointegrate:(IJK,MDYG),(ABT,STE),(POOL,LOW)
riingo_set_token('e890a696bafd550e1cea3378c5fe0853338f0aa1')
Tit1_prices=riingo_prices(c("IJK"), start_date = "2015-01-01", end_date = "2020-12-31", resample_frequency = "daily")
Tit2_prices=riingo_prices(c("MDYG"), start_date = "2015-01-01", end_date = "2020-12-31", resample_frequency = "daily")
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


y1=xts(Tit1_data$adjClose,as.Date(Tit1_data$date))
y2=xts(Tit2_data$adjClose,as.Date(Tit2_data$date))
Y_ <- merge(y1,y2)
names(Y_) <- c("IJK","MDYG")
if(anyNA(Y_)) 
  Y_ <- na.approx(Y_)
plot(Y_, legend.loc = "bottomleft", main = "Prices")


estimate_mu_gamma_LS <- function(Y, pct_training = 0.7) {
  Tr <- nrow(Y)
  T_trn <- round(pct_training*Tr)
  # LS regression
  ls_coeffs <- coef(lm(Y[1:T_trn, 1] ~ Y[1:T_trn, 2]))
  mu <- xts(rep(ls_coeffs[1], Tr), index(Y))
  colnames(mu) <- "mu-LS"
  gamma <- xts(rep(ls_coeffs[2], Tr), index(Y))
  colnames(gamma) <- "gamma-LS"
  return(list(mu = mu, gamma = gamma))
}

LS <- estimate_mu_gamma_LS(Y_)
LS_mu=LS$mu
LS_gamma=LS$gamma
tmp <- cbind(y1, LS_mu + LS_gamma*y2)
colnames(tmp) <- c(colnames(y1), paste("mu + gamma x", colnames(y2)))
plot(tmp[,1],type="l")
lines(tmp[,2],type="l",col='red')





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



par(mfrow=c(1,1))
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
up=bb$up
down=bb$dn
tail(bb,n=5)
chartSeries(Z_score,
            theme=chartTheme('white'))
addBBands(n=20,sd=2)
up[1:20]=2*sd(Z_score)
down[1:20]=-2*sd(Z_score)
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
    print(plot(cumprod(1 + portf_return), main = paste("Cum P&L with reinvestment", name)))
    print(plot(1+cumsum(portf_return), main = paste("Cum P&L no reinvestment", name)))
  }
  
  return(portf_return)
}


return_LS=pairs_trading(Y_,LS$gamma,LS$mu,plot=TRUE)
