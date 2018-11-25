library(quantmod)
library(gridExtra)
library(ggplot2)
library(lubridate)

vix_analysis <- function(data){
  
  VIXCLS <- data.frame(na.omit(data))
  names(VIXCLS)[6] <- "VIXCLS"
  VIXCLS$Date <- as.Date(row.names(VIXCLS))
  VIXCLS$Mean <- mean(VIXCLS$VIXCLS, na.rm = TRUE)
  VIXCLS$sd <- VIXCLS$Mean+sd(VIXCLS$VIXCLS, na.rm = TRUE)
  VIXCLS$ssd <- VIXCLS$Mean+2*sd(VIXCLS$VIXCLS, na.rm = TRUE)
  VIXCLS$negsd <- VIXCLS$Mean-sd(VIXCLS$VIXCLS, na.rm = TRUE)
  VIXCLS$negssd <- VIXCLS$Mean-2*sd(VIXCLS$VIXCLS, na.rm = TRUE)
  
  Dis_merge <- VIXCLS
  
  vixLT <- ggplot(Dis_merge[Dis_merge$Date >= "2017-01-01",], aes(x=Date, y=VIXCLS, group = 1))+
    geom_line(aes(x=Date, y=VIXCLS), col = "red")+
    ylab("")+
    xlab("")+
    ggtitle(paste("VIX YTD", VIXCLS$Date[nrow(VIXCLS)]))+
    geom_hline(yintercept = mean(Dis_merge$VIXCLS[Dis_merge$Date >= "2017-01-01"]))+
    #geom_hline(yintercept = mean(Dis_merge$VIXCLS[Dis_merge$Date >= "2017-01-01"])+sd(Dis_merge$VIXCLS[Dis_merge$Date >= "2017-01-01"]), col = "orange", linetype = "dashed")+
    geom_hline(yintercept = mean(Dis_merge$VIXCLS[Dis_merge$Date >= "2017-01-01"])+2*sd(Dis_merge$VIXCLS[Dis_merge$Date >= "2017-01-01"]), col = "red", linetype = "dashed")+
    geom_hline(yintercept = mean(Dis_merge$VIXCLS[Dis_merge$Date >= "2017-01-01"])-sd(Dis_merge$VIXCLS[Dis_merge$Date >= "2017-01-01"]), col = "blue", linetype = "dashed")+
    scale_x_date(date_breaks = "1 month", date_labels = "%b")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
  
  vixST <- ggplot(Dis_merge[Dis_merge$Date >= ymd(Dis_merge$Date[nrow(Dis_merge)])-years(1),], aes(x=Date, y=VIXCLS, group = 1))+
    geom_line(aes(x=Date, y=VIXCLS), col = "red")+
    ylab("")+
    xlab("")+
    ggtitle("VIX to Year Mean")+
    geom_hline(yintercept = mean(Dis_merge$VIXCLS[Dis_merge$Date >= ymd(Dis_merge$Date[nrow(Dis_merge)])-years(1)]))+
    #geom_hline(yintercept = mean(Dis_merge$VIXCLS[Dis_merge$Date >= ymd(Dis_merge$Date[nrow(Dis_merge)])-years(1)])+sd(Dis_merge$VIXCLS[Dis_merge$Date >= ymd(Dis_merge$Date[nrow(Dis_merge)])-years(1)]), col = "orange", linetype = "dashed")+
    geom_hline(yintercept = mean(Dis_merge$VIXCLS[Dis_merge$Date >= ymd(Dis_merge$Date[nrow(Dis_merge)])-years(1)])+2*sd(Dis_merge$VIXCLS[Dis_merge$Date >= ymd(Dis_merge$Date[nrow(Dis_merge)])-years(1)]), col = "red", linetype = "dashed")+
    geom_hline(yintercept = mean(Dis_merge$VIXCLS[Dis_merge$Date >= ymd(Dis_merge$Date[nrow(Dis_merge)])-years(1)])-sd(Dis_merge$VIXCLS[Dis_merge$Date >= ymd(Dis_merge$Date[nrow(Dis_merge)])-years(1)]), col = "blue", linetype = "dashed")+
    #geom_hline(yintercept = VIXCLS$negssd)+
    scale_x_date(date_breaks = "2 month", date_labels = "%b")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
  
  vix_dist <- ggplot(VIXCLS, aes(x=VIXCLS))+
    geom_histogram(aes(x=VIXCLS), col = "black", bins = 100)+
    ylab("")+
    xlab("")+
    ggtitle("Volatility Distribution")+
    geom_vline(xintercept = VIXCLS[nrow(VIXCLS),"VIXCLS"], col = "red")+
    #geom_point(data=VIXCLS[(row.names(VIXCLS)[nrow(VIXCLS)] == row.names(VIXCLS)),], col = "red")+
    geom_vline(xintercept = mean(VIXCLS$VIXCLS, na.rm = T)+sd(VIXCLS$VIXCLS, na.rm = T), linetype = "dashed", col ="red")+
    geom_vline(xintercept = mean(VIXCLS$VIXCLS, na.rm = T)-sd(VIXCLS$VIXCLS, na.rm = T), linetype = "dashed", col = "blue")+
    geom_vline(xintercept = mean(VIXCLS$VIXCLS, na.rm = T)+2*sd(VIXCLS$VIXCLS, na.rm = T), linetype = "dashed", col ="red")+
    geom_vline(xintercept = mean(VIXCLS$VIXCLS, na.rm = T)-1.5*sd(VIXCLS$VIXCLS, na.rm = T), linetype = "dashed", col = "blue")+
    geom_vline(xintercept = mean(VIXCLS$VIXCLS, na.rm = T), linetype = "dashed", col ="black")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
  
  grid.arrange(arrangeGrob(vixLT, vixST, ncol = 2), arrangeGrob(vix_dist, ncol = 1))
}

#getSymbols("VIXCLS", src = "FRED")
getSymbols("^VIX", from = "1990-01-01")
vix_analysis(VIX)

cross_asset_vol <- function(data, n_ema = 12){
  etf_prices <- do.call(merge, lapply(data, function(x) Ad(get(x))))
  
  etf_prices <- etf_prices[index(etf_prices)>="2010-03-11",] #OVX limits this!
  
  etf_prices[is.na(etf_prices)] <- 0
  for(i in 1:nrow(etf_prices)){
    for(j in 1:ncol(etf_prices)){
      if(etf_prices[i,j] == 0){
        etf_prices[i,j] <- etf_prices[i-1,j]
      }
    }
  }
  
  etf_returns <- etf_prices
  
  colnames(etf_returns) <- vols$sector
  etf_returns <- data.frame(etf_returns)
  etf_returns$Mean <- rowMeans(etf_returns)
  #etf_returns$Sum <- rowSums(etf_returns[,c("VIX","OVX","GVZ","EVZ")])
  etf_returns$Date <- as.Date(row.names(etf_returns))
  etf_returns$ma <- EMA(etf_returns[,"Mean"], n=n_ema)
  
  ggplot(etf_returns, aes(x=Date, y=Mean))+
    geom_line()+
    geom_line(aes(y=ma, x=Date), col = "red", alpha = 1) +
    ylab("")+
    xlab("")+
    ggtitle(paste("Cross-Asset Volatility", etf_returns$Date[nrow(etf_returns)]))+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
}

ticker <- c("^VIX","^OVX","^GVZ","^EVZ","^VXN", "^VVIX")
sector <- c("VIX", "OVX", "GVZ", "EVZ","^VXN","^VVIX")
symbols_etf_vol <- getSymbols(ticker, from = "2010-03-11")#OVX limits this!
vols <- data.frame(ticker, sector)
cross_asset_vol(symbols_etf_vol, n_ema = 41)

range_analysis <- function(data){
  GSPC <- to.weekly(GSPC)
  
  GSPC$Range <- abs(GSPC$GSPC.High / GSPC$GSPC.Low)
  GSPC$Delt_range <- abs(Delt(GSPC$GSPC.Adjusted))
  
  df <- data.frame(GSPC)
  df$Date <- as.Date(row.names(df))
  df$ma <- SMA(df$Range, n=12)
  df$ma_delt <- SMA(df$Delt_range, n=12)
  
  a1 <- ggplot(df, aes(x=Date, y=Range))+
    geom_line()+
    geom_line(aes(x=Date, y=ma), col = "red", alpha = 0.7)+
    geom_hline(yintercept = 1, linetype = "dashed")+
    ylab("")+
    xlab("")+
    ggtitle(paste("Weekly Range (High/Low)", df$Date[nrow(df)]))+
    scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
  
  b1 <- ggplot(df[df$Date >= (Sys.Date()-years(5)),], aes(x=Date, y=Range))+
    geom_line()+
    geom_line(aes(x=Date, y=ma), col = "red")+
    geom_hline(yintercept = 1, linetype = "dashed")+
    ylab("")+
    xlab("")+
    ggtitle("")+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
  
  c1 <- ggplot(df, aes(x=Range))+
    geom_histogram(col = "black", bins = 100)+
    geom_vline(xintercept = df[nrow(df), "Range"], col = "red")+
    ylab("")+
    xlab("")+
    ggtitle("")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
  
  
  a2 <- ggplot(df, aes(x=Date, y=Delt_range))+
    geom_line()+
    geom_line(aes(x=Date, y=ma_delt), col = "red", alpha = 0.7)+
    geom_hline(yintercept = 0, linetype = "dashed")+
    ylab("")+
    xlab("")+
    ggtitle(paste("Weekly Range (Close/Close)", df$Date[nrow(df)]))+
    scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
  
  b2 <- ggplot(df[df$Date >= (Sys.Date()-years(5)),], aes(x=Date, y=Delt_range))+
    geom_line()+
    geom_line(aes(x=Date, y=ma_delt), col = "red")+
    geom_hline(yintercept = 0, linetype = "dashed")+
    ylab("")+
    xlab("")+
    ggtitle("")+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
  
  c2 <- ggplot(df, aes(x=Delt_range))+
    geom_histogram(col = "black", bins = 100)+
    geom_vline(xintercept = df[nrow(df), "Delt_range"], col = "red")+
    ylab("")+
    xlab("")+
    ggtitle("")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
  
  grid.arrange(arrangeGrob(a1, b1, ncol = 2), arrangeGrob(c1, ncol = 1))
  grid.arrange(arrangeGrob(a2, b2, ncol = 2), arrangeGrob(c2, ncol = 1))
}

getSymbols("^GSPC", from = "1950-01-01")
range_analysis(GSPC)
