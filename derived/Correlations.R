library(quantmod)   
library(lubridate)
library(ggplot2)
library(reshape2)
library(gridExtra)
Sys.setlocale("LC_TIME", "C")

sector_correlations <- function(data, width = 120, since = "2017-01-01"){

  etf_prices <- do.call(merge, lapply(data, function(x) Ad(get(x))))
  
  etf_prices <- etf_prices[index(etf_prices)>=since,]
  
  etf_prices[is.na(etf_prices)] <- 0
  for(i in 1:nrow(etf_prices)){
    for(j in 1:ncol(etf_prices)){
      if(etf_prices[i,j] == 0){
        etf_prices[i,j] <- etf_prices[i-1,j]
      }
    }
  }
  
  etf_returns <- do.call(merge, lapply(etf_prices, 
                                       function(x) periodReturn(x, period = 'daily', type = 'log')))
  
  colnames(etf_returns) <- etf_ticker_sector$sector
  
  etf_returns$Mean.Sector.Corr <- rowMeans(etf_returns[,-which(names(etf_returns) %in% "Index")])
  
  
  sector_index_correlation <- function(x, window) {
    merged_xts <- merge(x, etf_returns$'Index')
    merged_xts$rolling_test <- rollapply(merged_xts, window, 
                                         function(x) cor(x[,1], x[,2], use = "pairwise.complete.obs",
                                                         method = "pearson"), #only pearson works
                                         by.column = FALSE)
    names(merged_xts) <- c("Sector Returns", "SPY Returns", "Sector/SPY Correlation")
    merged_xts
  }
  
  width <- width
  correlation <- data.frame(sector_index_correlation(etf_returns$'Mean.Sector.Corr', width))
  names(correlation)[3] <- 'Mean.Sector.Corr'
  correlation$Consumer.Discretionary <- data.frame(sector_index_correlation(etf_returns$'Consumer.Discretionary', width))$Sector.SPY.Correlation
  correlation$Financials <- data.frame(sector_index_correlation(etf_returns$'Financials', width))$Sector.SPY.Correlation
  correlation$Industrials <- data.frame(sector_index_correlation(etf_returns$'Industrials', width))$Sector.SPY.Correlation
  correlation$Information.Technology <- data.frame(sector_index_correlation(etf_returns$'Information.Technology', width))$Sector.SPY.Correlation
  correlation$Materials <- data.frame(sector_index_correlation(etf_returns$'Materials', width))$Sector.SPY.Correlation
  
  correlation$Consumer.Staples <- data.frame(sector_index_correlation(etf_returns$'Consumer.Staples', width))$Sector.SPY.Correlation
  correlation$Energy <- data.frame(sector_index_correlation(etf_returns$'Energy', width))$Sector.SPY.Correlation
  correlation$Health.Care <- data.frame(sector_index_correlation(etf_returns$'Health.Care', width))$Sector.SPY.Correlation
  correlation$Utilities <- data.frame(sector_index_correlation(etf_returns$'Utilities', width))$Sector.SPY.Correlation
  
  #correlation$Cyclical.Mean <- rowMeans(correlation[,-which(names(correlation) %in% c("Mean.Sector.Corr", 
  #                                                                                    "Sector.Returns","SPY.Returns","date",
  #                                                                                    "Consumer.Staples",
  #                                                                                    "Energy","Health.Care",
  #                                                                                    "Utilities"))])
  
  #correlation$Defensive.Mean <- rowMeans(correlation[,-which(names(correlation) %in% c("Mean.Sector.Corr", 
  #                                                                                     "Sector.Returns","SPY.Returns","date",
  #                                                                                     "Consumer.Discretionary",
  #                                                                                     "Financials","Industrials",
  #                                                                                     "Information.Technology",
  #                                                                                     "Materials"))])
  
  #correlation$Total.Mean <- rowMeans(correlation[,-which(names(correlation) %in% c("Mean.Sector.Corr", "Sector.Returns","SPY.Returns","date"))])
  correlation$date <- as.Date(row.names(correlation))
  longtemp <- melt(correlation, measure.vars = names(correlation)[4:(ncol(correlation)-1)])
  
  if(width < 120){
    ind1 <- "2 month"
    ind2 <- "%b"
  }else{
    ind1 <- "2 month"
    ind2 <- "%b"
  }

  
  ggplot(longtemp, aes(y=value, x=date))+
    geom_line()+
    geom_point(size = 0.1)+
    ylab("")+
    xlab("")+
    scale_x_date(date_breaks = ind1, date_labels = ind2)+
    facet_wrap(~ variable, ncol = 3, scales = "free")+
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
ticker <- c("XLY", "XLP", "XLE", "XLF", "XLV",   
            "XLI", "XLB", "XLK", "XLU", "SPY") 
symbols_etf <- getSymbols(ticker, from = "2007-01-01")

sector <- c("Consumer Discretionary", "Consumer Staples", 
            "Energy", "Financials", "Health Care", "Industrials", 
            "Materials", "Information Technology", "Utilities", "Index")
etf_ticker_sector <- data.frame(ticker, sector)
sector_correlations(symbols_etf, width = 30, since = "2017-01-01")
sector_correlations(symbols_etf, width = 60, since = "2017-01-01")
sector_correlations(symbols_etf, width = 120, since = "2017-01-01")
#sectors tend to become more correlated during moves down and initial rises from lows and
#then less correlated during periods of short-term topping

#correlations tend to rise during most, but not all, crisis periods and fall back once the crisis has passed
#correlations tend to rise during weak macro-economic conditions, and fall back when growth is strong
#high correlations tend to be associated with high levels of volatility, and vice versa.
#understandably, correlations are relatively low right now, which implies that markets are not stressed. 
#however, correlations are like anything else, and have a tendency to revert-back-to-the-mean.

corr_all_equity <- function(data, width = 120){
  
  etf_prices <- do.call(merge, lapply(data, function(x) Ad(get(x))))
  etf_prices[is.na(etf_prices)] <- 0
  for(i in 1:nrow(etf_prices)){
    for(j in 1:ncol(etf_prices)){
      if(etf_prices[i,j] == 0){
        etf_prices[i,j] <- etf_prices[i-1,j]
      }
    }
  }
  z <- etf_prices
  
  colnames(z) <- etf_ticker_sector$sector
  z.logrtn <- diff(log(z))
  c <- cor(z.logrtn,use="complete.obs")
  ut <- upper.tri(c)
  n <- paste(rownames(c)[row(c)[ut]],rownames(c)[col(c)[ut]])
  
  rollingcorr.1m <- rollapply(z.logrtn,
                              width=width,
                              FUN = function(Z)
                              {
                                return(cor(Z,use="pairwise.complete.obs")[ut])
                              },
                              by.column=FALSE, align="right")
  colnames(rollingcorr.1m) <- n
  
  rollingcorr.1m.df <- data.frame(rollingcorr.1m)
  rollingcorr.1m.df$Mean_all <- rowMeans(rollingcorr.1m.df, na.rm = T)
  rollingcorr.1m.df$Date <- as.Date(row.names(rollingcorr.1m.df))
  
  ggplot(data = rollingcorr.1m.df, aes(x = Date, y = Mean_all)) +
    geom_ribbon(aes(ymin = 0, ymax = rollingcorr.1m.df[,"Mean_all"])) +
    ggtitle("Equity correlations (average of sector correlations for S&P500)")+ 
    ylab("")+
    xlab("")+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          text = element_text(size=10),
          legend.position = "none")
}

ticker <- c("XLY", "XLP", "XLE", "XLF", "XLV",   
            "XLI", "XLB", "XLK", "XLU")  

sector <- c("Consumer Discretionary", "Consumer Staples", 
            "Energy", "Financials", "Health Care", "Industrials", 
            "Materials", "Information Technology", "Utilities")
etf_ticker_sector <- data.frame(ticker, sector)

corr_all_equity(symbols_etf[symbols_etf %in% c("XLY", "XLP", "XLE", "XLF", "XLV",   
                                               "XLI", "XLB", "XLK", "XLU")], width = 120)
corr_all_equity(symbols_etf[symbols_etf %in% c("XLY", "XLP", "XLE", "XLF", "XLV",   
                                   "XLI", "XLB", "XLK", "XLU")], width = 360)

corr_to_spy_sec <- function(data, width = 120){
  
  etf_prices <- do.call(merge, lapply(data, function(x) Ad(get(x))))
  etf_prices[is.na(etf_prices)] <- 0
  for(i in 1:nrow(etf_prices)){
    for(j in 1:ncol(etf_prices)){
      if(etf_prices[i,j] == 0){
        etf_prices[i,j] <- etf_prices[i-1,j]
      }
    }
  }
  etf_returns <- do.call(merge, lapply(etf_prices, 
                                       function(x) periodReturn(x, period = 'daily', type = 'log')))
  
  colnames(etf_returns) <- etf_ticker_sector$sector
  
  sector_index_correlation <- function(x, window) {
    merged_xts <- merge(x, etf_returns$'SPY')
    merged_xts$rolling_test <- rollapply(merged_xts, window, 
                                         function(x) cor(x[,1], x[,2], use = "pairwise.complete.obs"), 
                                         by.column = FALSE)
    names(merged_xts) <- c("Sector Returns", "SPY Returns", "Sector/SPY Correlation")
    data.frame(merged_xts)
  }
  
  width <- width
  correlation <- data.frame(sector_index_correlation(etf_returns[,1], width))
  for(i in 2:(length(colnames(etf_returns))-1)){
    correlation <- cbind(correlation, sector_index_correlation(etf_returns[,i], width)$Sector.SPY.Correlation)
  }
  correlation$date <- as.Date(row.names(correlation))
  names(correlation)[3:(ncol(correlation)-1)] <- colnames(etf_returns)[1:(ncol(etf_returns)-1)]
  correlation$Mean_all <- rowMeans(correlation[,-which(names(correlation) %in% c("date"))], na.rm = T)
  #longtemp <- melt(correlation, measure.vars = names(correlation)[3:(ncol(correlation)-1)])
  correlation <- correlation[correlation$Mean_all >= 0.1,]
  ggplot(correlation, aes(y=Mean_all, x=date))+
    geom_ribbon(aes(ymin = 0, ymax = correlation[,"Mean_all"])) +
    ggtitle("Equity correlations to S&P500 (average of sector correlations to S&P500)")+ 
    ylab("")+
    xlab("")+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          text = element_text(size=10),
          legend.position = "none")
}


ticker <- c("XLY", "XLP", "XLE", "XLF", "XLV",   
            "XLI", "XLB", "XLK", "XLU", "SPY")  

sector <- c("Consumer Discretionary", "Consumer Staples", 
            "Energy", "Financials", "Health Care", "Industrials", 
            "Materials", "Information Technology", "Utilities", "SPY")
etf_ticker_sector <- data.frame(ticker, sector)
corr_to_spy_sec(symbols_etf, width = 120)
corr_to_spy_sec(symbols_etf, width = 360)


corr_all_equity <- function(data, width = 120){
  
  etf_prices <- do.call(merge, lapply(data, function(x) Ad(get(x))))
  etf_prices <- etf_prices[index(etf_prices)>="2007-04-11",]
  etf_prices$TLT.Adjusted <- 1/etf_prices$TLT.Adjusted
  
  z <- etf_prices
  
  colnames(z) <- etf_ticker_sector$sector
  z.logrtn <- diff(log(z))
  c <- cor(z.logrtn,use="complete.obs")
  ut <- upper.tri(c)
  n <- paste(rownames(c)[row(c)[ut]],rownames(c)[col(c)[ut]])
  
  rollingcorr.1m <- rollapply(z.logrtn,
                              width=width,
                              FUN = function(Z)
                              {
                                return(cor(Z,use="pairwise.complete.obs")[ut])
                              },
                              by.column=FALSE, align="right")
  colnames(rollingcorr.1m) <- n
  
  rollingcorr.1m.df <- data.frame(rollingcorr.1m)
  rollingcorr.1m.df$Mean_all <- rowMeans(rollingcorr.1m.df, na.rm = T)
  rollingcorr.1m.df$Date <- as.Date(row.names(rollingcorr.1m.df))
  
  etf_prices <- do.call(merge, lapply(data, function(x) Ad(get(x))))
  etf_prices <- data.frame(etf_prices)
  etf_prices$Date <- as.Date(row.names(etf_prices))
  
  ggplot(data = rollingcorr.1m.df, aes(x = Date, y = Mean_all)) +
    geom_ribbon(aes(ymin = 0, ymax = rollingcorr.1m.df[,"Mean_all"])) +
    geom_line(data = etf_prices, aes(x=Date, y=SPY.Adjusted/150))+
    ggtitle(paste("Cross-Asset Correlation", rollingcorr.1m.df$Date[nrow(rollingcorr.1m.df)]))+ 
    ylab("")+
    xlab("")+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          text = element_text(size=10),
          legend.position = "none")
}

#WHAT ASSTES TO USE

ticker <- c("SPY","IWM","QQQ","USO","DBC","HYG","EEM","FXI","VEU","EZU","TLT","VNQ") 
sector <- c("SPY","IWM","QQQ","USO","DBC","HYG","EEM","FXI","VEU","EZU","TLT","VNQ")

symbols_etf_corr <- getSymbols(ticker, from = "2007-04-11")#HYG limits this!
etf_ticker_sector <- data.frame(ticker, sector)
corr_all_equity(symbols_etf_corr, width = 120)


corr_all_equity_country <- function(data, width = 120){
  data <- symbols_etf_corr
  etf_prices <- do.call(merge, lapply(data, function(x) Ad(get(x))))
  etf_prices <- etf_prices[index(etf_prices)>="2010-03-11",]
  
  z <- etf_prices
  
  colnames(z) <- etf_ticker_sector$sector
  z.logrtn <- diff(log(z))
  c <- cor(z.logrtn,use="complete.obs")
  ut <- upper.tri(c)
  n <- paste(rownames(c)[row(c)[ut]],rownames(c)[col(c)[ut]])
  
  rollingcorr.1m <- rollapply(z.logrtn,
                              width=width,
                              FUN = function(Z)
                              {
                                return(cor(Z,use="pairwise.complete.obs")[ut])
                              },
                              by.column=FALSE, align="right")
  colnames(rollingcorr.1m) <- n
  
  rollingcorr.1m.df <- data.frame(rollingcorr.1m)
  rollingcorr.1m.df$Mean_all <- rowMeans(rollingcorr.1m.df, na.rm = T)
  rollingcorr.1m.df$Date <- as.Date(row.names(rollingcorr.1m.df))
  
  etf_prices <- do.call(merge, lapply(data, function(x) Ad(get(x))))
  etf_prices <- data.frame(etf_prices)
  etf_prices$Date <- as.Date(row.names(etf_prices))
  
  ggplot(data = rollingcorr.1m.df, aes(x = Date, y = Mean_all)) +
    geom_ribbon(aes(ymin = 0, ymax = rollingcorr.1m.df[,"Mean_all"])) +
    geom_line(data = etf_prices[etf_prices$Date >= "2010-03-11",], aes(x=Date, y=SPY.Adjusted/150))+
    ggtitle(paste("Cross-Asset Correlation for Country ETFs", rollingcorr.1m.df$Date[nrow(rollingcorr.1m.df)]))+ 
    ylab("")+
    xlab("")+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          text = element_text(size=10),
          legend.position = "none")
}

ticker <- c("SPY","EFA", "EEM", "EWA", "EWO", "EWK", "EWZ", "EWC", "FXI", "EWQ", "EWG", "EWH", "INP", "EWI",
            "EWJ", "EWM", "EWW", "EWN", "RSX", "EWS", "EWY", "EWP", "EWD", "EWL", "EWT", "EWU") 
sector <- c("SPY","EFA", "EEM", "EWA", "EWO", "EWK", "EWZ", "EWC", "FXI", "EWQ", "EWG", "EWH", "INP", "EWI",
            "EWJ", "EWM", "EWW", "EWN", "RSX", "EWS", "EWY", "EWP", "EWD", "EWL", "EWT", "EWU")

symbols_etf_corr <- getSymbols(ticker, from = "2007-04-11")
etf_ticker_sector <- data.frame(ticker, sector)
corr_all_equity_country(symbols_etf_corr, width = 120)


