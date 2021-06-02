#install.packages("riingo")
#install.packages('devtools')
#devtools::install_github("DavisVaughan/riingo")

#Caricamento librerie
library(riingo)
library(devtools)
library(dplyr)
library(egcm)

#Salvo il token riingo
riingo_set_token('e890a696bafd550e1cea3378c5fe0853338f0aa1')

#filtro i tickers per stock, USD e solo quelle create dal 2015-01-01 in poi
tickers <- supported_tickers()
stocks<-tickers[tickers$assetType=='Stock' & tickers$priceCurrency=='USD' & tickers$startDate<'2015-01-02' & (tickers$exchange =="NASDAQ" | tickers$exchange =="NYSE"),]

#seleziono solo le stock senza NA
stocks_fltr<-stocks[-which(apply(is.na(stocks),MARGIN=1,sum)==6),]
dim(stocks_fltr)

#filtro le stock restanti per i tickers di S&P 500
SP550_tickers<-c('MMM', 'ABT', 'ABBV', 'ABMD', 'ACN', 'ATVI', 'ADBE', 'AMD', 'AAP', 'AES', 'AFL', 'A', 'APD', 'AKAM', 'ALK', 'ALB', 'ARE', 'ALXN', 'ALGN', 'ALLE', 'LNT', 'ALL', 'GOOGL', 'GOOG', 'MO', 'AMZN', 'AEE', 'AAL', 'AEP', 'AXP', 'AIG', 'AMT', 'AWK', 'AMP', 'ABC', 'AME', 'AMGN', 'APH', 'ADI', 'ANSS', 'ANTM', 'AON', 'AOS', 'APA', 'AAPL', 'AMAT', 'APTV', 'ADM', 'ANET', 'AJG', 'AIZ', 'T', 'ATO', 'ADSK', 'ADP', 'AZO', 'AVB', 'AVY', 'BLL', 'BAC', 'BK', 'BAX', 'BDX', 'BBY', 'BIO', 'BIIB', 'BLK', 'BA', 'BKNG', 'BWA', 'BXP', 'BSX', 'BMY', 'AVGO', 'BR', 'CHRW', 'COG', 'CDNS', 'CZR', 'CPB', 'COF', 'CAH', 'KMX', 'CCL', 'CTLT', 'CAT', 'CBOE', 'CBRE', 'CDW', 'CE', 'CNC', 'CNP', 'CERN', 'CF', 'SCHW', 'CHTR', 'CVX', 'CMG', 'CB', 'CHD', 'CI', 'CINF', 'CTAS', 'CSCO', 'C', 'CFG', 'CTXS', 'CLX', 'CME', 'CMS', 'KO', 'CTSH', 'CL', 'CMCSA', 'CMA', 'CAG', 'COP', 'ED', 'STZ', 'COO', 'CPRT', 'GLW', 'COST', 'CCI', 'CSX', 'CMI', 'CVS', 'DHI', 'DHR', 'DRI', 'DVA', 'DE', 'DAL', 'XRAY', 'DVN', 'DXCM', 'FANG', 'DLR', 'DFS', 'DISCA', 'DISCK', 'DISH', 'DG', 'DLTR', 'D', 'DPZ', 'DOV', 'DTE', 'DUK', 'DRE', 'DD', 'EMN', 'ETN', 'EBAY', 'ECL', 'EIX', 'EW', 'EA', 'EMR', 'ENPH', 'ETR', 'EOG', 'EFX', 'EQIX', 'EQR', 'ESS', 'EL', 'ES', 'RE', 'EXC', 'EXPE', 'EXPD', 'EXR', 'XOM', 'FFIV', 'FB', 'FAST', 'FRT', 'FDX', 'FIS', 'FITB', 'FE', 'FRC', 'FISV', 'FLT', 'FLIR', 'FMC', 'F', 'FTNT', 'FBHS', 'BEN', 'FCX', 'GPS', 'GRMN', 'IT', 'GNRC', 'GD', 'GE', 'GIS', 'GM', 'GPC', 'GILD', 'GL', 'GPN', 'GS', 'GWW', 'HAL', 'HBI', 'HIG', 'HAS', 'HCA', 'PEAK', 'HSIC', 'HSY', 'HES', 'HLT', 'HFC', 'HOLX', 'HD', 'HON', 'HRL', 'HST', 'HWM', 'HPQ', 'HUM', 'HBAN', 'HII', 'IEX', 'IDXX', 'INFO', 'ITW', 'ILMN', 'INCY', 'INTC', 'ICE', 'IBM', 'IP', 'IPG', 'IFF', 'INTU', 'ISRG', 'IVZ', 'IPGP', 'IQV', 'IRM', 'JKHY', 'J', 'JBHT', 'SJM', 'JNJ', 'JCI', 'JPM', 'JNPR', 'KSU', 'K', 'KEY', 'KEYS', 'KMB', 'KIM', 'KMI', 'KLAC', 'KR', 'LB', 'LHX', 'LH', 'LRCX', 'LVS', 'LEG', 'LDOS', 'LEN', 'LLY', 'LNC', 'LYV', 'LKQ', 'LMT', 'L', 'LOW', 'LUMN', 'LYB', 'MTB', 'MRO', 'MPC', 'MKTX', 'MAR', 'MMC', 'MLM', 'MAS', 'MA', 'MKC', 'MXIM', 'MCD', 'MCK', 'MDT', 'MRK', 'MET', 'MTD', 'MGM', 'MCHP', 'MU', 'MSFT', 'MAA', 'MHK', 'TAP', 'MDLZ', 'MPWR', 'MNST', 'MCO', 'MS', 'MOS', 'MSI', 'MSCI', 'NDAQ', 'NTAP', 'NFLX', 'NWL', 'NEM', 'NWSA', 'NWS', 'NEE', 'NLSN', 'NKE', 'NI', 'NSC', 'NTRS', 'NOC', 'NLOK', 'NCLH', 'NOV', 'NRG', 'NUE', 'NVDA', 'NVR', 'NXPI', 'ORLY', 'OXY', 'ODFL', 'OMC', 'OKE', 'ORCL', 'PCAR', 'PKG', 'PH', 'PAYX', 'PAYC', 'PENN', 'PNR', 'PBCT', 'PEP', 'PKI', 'PRGO', 'PFE', 'PM', 'PSX', 'PNW', 'PXD', 'PNC', 'POOL', 'PPG', 'PPL', 'PFG', 'PG', 'PGR', 'PLD', 'PRU', 'PTC', 'PEG', 'PSA', 'PHM', 'PVH', 'QRVO', 'PWR', 'QCOM', 'DGX', 'RL', 'RJF', 'RTX', 'O', 'REG', 'REGN', 'RF', 'RSG', 'RMD', 'RHI', 'ROK', 'ROL', 'ROP', 'ROST', 'RCL', 'SPGI', 'CRM', 'SBAC', 'SLB', 'STX', 'SEE', 'SRE', 'NOW', 'SHW', 'SPG', 'SWKS', 'SNA', 'SO', 'LUV', 'SWK', 'SBUX', 'STT', 'STE', 'SYK', 'SIVB', 'SYF', 'SNPS', 'SYY', 'TMUS', 'TROW', 'TTWO', 'TPR', 'TGT', 'TEL', 'TDY', 'TFX', 'TER', 'TSLA', 'TXN', 'TXT', 'TMO', 'TJX', 'TSCO', 'TT', 'TDG', 'TRV', 'TRMB', 'TFC', 'TWTR', 'TYL', 'TSN', 'UDR', 'ULTA', 'USB', 'UAA', 'UNP', 'UAL', 'UNH', 'UPS', 'URI', 'UHS', 'UNM', 'VLO', 'VTR', 'VRSN', 'VRSK', 'VZ', 'VRTX', 'VFC', 'VIAC', 'V', 'VNO', 'VMC', 'WRB', 'WAB', 'WMT', 'WBA', 'DIS', 'WM', 'WAT', 'WEC', 'WFC', 'WELL', 'WST', 'WDC', 'WU', 'WY', 'WHR', 'WMB', 'WYNN', 'XEL', 'XLNX', 'XYL', 'YUM', 'ZBRA') #rimossi: 'AMCR' 'BKR' 'BRK.B' 'BF.B' 'CARR' 'CTVA' 'DOW' 'DXC' 'ETSY' 'EVRG' 'FTV' 'FOXA' 'FOX' 'HPE' 'IR' 'KHC' 'LW' 'LIN' 'OTIS' 'PYPL' 'UA' 'VTRS' 'WRK' 'WLTW' 'ZBH' #rimossi: 'AMCR' 'BKR' 'BRK.B' 'BF.B' 'CARR' 'CTVA' 'DOW' 'DXC' 'ETSY' 'EVRG' 'FTV' 'FOXA' 'FOX' 'HPE' 'IR' 'KHC' 'LW' 'LIN' 'OTIS' 'PYPL' 'UA' 'VTRS' 'ZION' 'ZTS'
stock_fltr_SP500<-subset(stocks_fltr,ticker %in% SP550_tickers)
dim(stock_fltr_SP500)

#estrazione serie e costruzione data frame basato su adjClose
x<-riingo_prices('MMM',start_date = '2015-01-01',end_date = '2020-12-31', resample_frequency = "daily") #serie di prova, utile a creare il df
df<-data.frame(as.Date(x$date))

for(i in SP550_tickers){
  x<-riingo_prices(i,start_date = '2015-01-01',end_date = '2020-12-31', resample_frequency = "daily")
  df[i]<-x$adjClose
}

#ciclo per cointegration test
len<-length(SP550_tickers)
df_coint<-data.frame(matrix(0,ncol = len,nrow=len))
colnames(df_coint)<-c(SP550_tickers)
df_1<-df[,-1]

j=1           
for(i in 1:len){
  for(x in j:len){
    res <- egcm(df_1[,i], df_1[,x])
    df_coint[i,x]<-is.cointegrated(res)
  }
  j=j+1
}



#individuo le coppie che passano il primo test di integrazione (non completo)
dim(df_coint)
rownames(df_coint)<-SP550_tickers

diag(df_coint)<-0



#verifica delle delle coppie che passano la cointegrazione
res<-egcm(df[,'APD'], df[,'ADBE'])
summary(res)

#si può pensare di selezionare coppie dello stesso settore per scrematura ulteriore
#scrematura per dickey fuller test
which(df_coint!=0)
coppie <- cbind(row(df_coint)[which(!df_coint == 0)], col(df_coint)[which(!df_coint == 0)]) #circa 4000 coppie
pval <- c()
for (i in 1:3866){
  test <- egcm(df_1[,coppie[i,1]], df_1[,coppie[i,2]], urtest = 'adf')
  pval[i] <- test$r.p
}
pval


coppie <- as.data.frame(coppie)
coppie$Tick1 <- colnames(df_1[,coppie[,1]])
coppie$Tick2 <- colnames(df_1[,coppie[,2]])
coppie$pval <- pval
coppie <- coppie[,c(3,4,5)]
coppie2 <- coppie[order(pval),]
coppie3 <- subset(coppie2,pval < 0.005)  #scrematura fino a 85 coppie

library(ggplot2)
df=rbind(df_1("CTAS"),df_1("CPRT"))
ggplot(df, aes(x = date, y = adjClose)) + 
  geom_line(aes(color = ticker), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

#write.csv(coppie3,"C:\\Users\\aless\\OneDrive\\Desktop\\AleBicocca\\Data Science\\Anno 2\\Lab Economics\\Forte_economics\\coppie.csv")
#write.csv(df_1,"C:\\Users\\aless\\OneDrive\\Desktop\\AleBicocca\\Data Science\\Anno 2\\Lab Economics\\Forte_economics\\TS_tickers.csv")
#write.csv(df_coint,"C:\\Users\\aless\\OneDrive\\Desktop\\AleBicocca\\Data Science\\Anno 2\\Lab Economics\\Forte_economics\\matrix.csv")


#aggiusto nomi
library(stringr)
coppie3<-read.csv('C:/Users/aless/OneDrive/Desktop/AleBicocca/Data Science/Anno 2/Lab Economics/Forte_economics/Progetto/Dati/coppie.csv')
coppie4<-coppie3
coppie4$Tick1<- str_replace(coppie4$Tick1,".[0-9]+","")
coppie4$Tick2<- str_replace(coppie4$Tick2,".[0-9]+","")

#importo dizionario
dizionario<-read.csv('C:/Users/aless/OneDrive/Desktop/AleBicocca/Data Science/Anno 2/Lab Economics/Forte_economics/Progetto/Dati/dizionario.csv')

#merge per Ticker1
dizionario<-dizionario[,2:4]
colnames(dizionario)<-c("ticker1","company1","sector1")
coppie5<-merge(coppie4,dizionario,by.x = 'Tick1',by.y = 'ticker1')

#merge per Ticker2
dizionario2<-dizionario
colnames(dizionario2)<-c("ticker2","company2", "sector2")
coppie5<-merge(coppie5,dizionario2,by.x = 'Tick2',by.y = 'ticker2')

#riordino elementi
coppie6<-subset(coppie5,select=c(2,4,5,1,3,6,7,3))

sub=subset(coppie5, sector1 == sector2)






