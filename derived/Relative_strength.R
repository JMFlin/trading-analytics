library(quantmod)   
library(lubridate)
library(ggplot2)
library(gridExtra)
library(directlabels)
Sys.setlocale("LC_TIME", "C")

relative_str <- function(data, six_month=TRUE, since){
  #df <- merge(EWJ,SPY,VEU,EEM,FEZ,QQQ,IWM,MTUM,SPHB,IVE,IVW)
  data <- df
  data <- data.frame(Ad(data))
  data <- data.frame(data[year(row.names(data)) == year(Sys.Date()),])
  len <- function(x){
    length(x)
  }
  
  mlag <- function(m, nlag = 1){
    if(is.null(dim(m))){
      n = len(m)
      if(nlag > 0) {
        m[(nlag+1):n] = m[1:(n-nlag)]
        m[1:nlag] = NA
      }else if(nlag < 0){
        m[1:(n+nlag)] = m[(1-nlag):n]
        m[(n+nlag+1):n] = NA
      }
    } else {
      n = nrow(m)
      if(nlag > 0) {
        m[(nlag+1):n,] = m[1:(n-nlag),]
        m[1:nlag,] = NA
      } else if(nlag < 0) {
        m[1:(n+nlag),] = m[(1-nlag):n,]
        m[(n+nlag+1):n,] = NA
      }
    }
    return(m);
  }
  
  prices <- data[,1]
  n <- len(tickers)  
  ret <- prices / mlag(prices) - 1
  
  dates <- row.names(data)
  years <- year(dates)    
  #index <- which(month(dates) == month(Sys.Date()))
  
  trading.days <- sapply(tapply(ret, years, function(x) coredata(x)), function(x) x[1:length(ret)])

  current.year <- trading.days[, ncol(trading.days)]
  
  current.year <- ifelse(is.na(current.year), 0, current.year)
  current.year <- 100*(cumprod(1 + current.year)- 1)
  
  df <- data.frame(current.year)
  names(df) <- c("curr")
  df$Date <- factor(row.names(data), as.character(row.names(data)))
  #df$name <- strtrim(names(data),3)[1]
  df$name <- gsub("\\..*$","", names(data)[1])
  
  for(i in 2:ncol(data)){
    
    prices <- data[,i]
    n <- len(tickers)  
    ret <- prices / mlag(prices) - 1
    
    dates <- row.names(data)
    years <- year(dates)    
    #index <- which(month(dates) == month(Sys.Date()))
    
    trading.days <- sapply(tapply(ret, years, function(x) coredata(x)), function(x) x[1:length(ret)])
    current.year <- trading.days[, ncol(trading.days)]
    current.year <- ifelse(is.na(current.year), 0, current.year)

    current.year <- 100*(cumprod(1 + current.year)- 1)
    
    df_tmp <- data.frame(current.year)
    names(df_tmp) <- c("curr")
    df_tmp$Date <- factor(row.names(data), as.character(row.names(data)))
    #df_tmp$name <- strtrim(names(data),3)[i]
    df_tmp$name <- gsub("\\..*$","", names(data)[i])
    
    df <- rbind(df, df_tmp)
  }
  names(df)[3] <- "ETF"
  df$Date <- as.Date(df$Date)
  p <- list()
  p[[1]] <- ggplot(df, aes(y=curr, x=Date, col = ETF, group = ETF))+
    geom_line()+
    #ggtitle(paste("Seasonalities for", lubridate::month(Sys.Date(), label = TRUE, abbr = FALSE), year(Sys.Date())))+
    ggtitle("YTD ETF Performances")+
    ylab("")+
    xlab("")+
    geom_dl(aes(label = ETF), method = list(dl.trans(x = x + .2), "last.bumpup", cex = 0.7))+
    geom_hline(yintercept = 0, linetype = "dashed")+
    scale_x_date(expand=c(0.1, 0), date_breaks = "1 month", date_labels = "%b %d")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          legend.position="none",
          text = element_text(size=10), 
          plot.title = element_text(size = 10))
  
  if(six_month==TRUE){
    data <- data.frame(data[month(row.names(data)) >= since,])
    prices <- data[,1]
    n <- len(tickers)  
    ret <- prices / mlag(prices) - 1
    
    dates <- row.names(data)
    years <- year(dates)    
    #index <- which(month(dates) == month(Sys.Date()))
    
    trading.days <- sapply(tapply(ret, years, function(x) coredata(x)), function(x) x[1:length(ret)])
    
    current.year <- trading.days[, ncol(trading.days)]
    
    current.year <- ifelse(is.na(current.year), 0, current.year)
    current.year <- 100*(cumprod(1 + current.year)- 1)
    
    df <- data.frame(current.year)
    names(df) <- c("curr")
    df$Date <- factor(row.names(data), as.character(row.names(data)))
    #df$name <- strtrim(names(data),3)[1]
    df$name <- gsub("\\..*$","", names(data)[1])
    
    for(i in 2:ncol(data)){
      
      prices <- data[,i]
      n <- len(tickers)  
      ret <- prices / mlag(prices) - 1
      
      dates <- row.names(data)
      years <- year(dates)    
      #index <- which(month(dates) == month(Sys.Date()))
      
      trading.days <- sapply(tapply(ret, years, function(x) coredata(x)), function(x) x[1:length(ret)])
      current.year <- trading.days[, ncol(trading.days)]
      current.year <- ifelse(is.na(current.year), 0, current.year)
      
      current.year <- 100*(cumprod(1 + current.year)- 1)
      
      df_tmp <- data.frame(current.year)
      names(df_tmp) <- c("curr")
      df_tmp$Date <- factor(row.names(data), as.character(row.names(data)))
      #df_tmp$name <- strtrim(names(data),3)[i]
      df_tmp$name <- gsub("\\..*$","", names(data)[i])
      
      df <- rbind(df, df_tmp)
    }
    names(df)[3] <- "ETF"
    df$Date <- as.Date(df$Date)
    p[[2]] <- ggplot(df, aes(y=curr, x=Date, col = ETF, group = ETF))+
      geom_line()+
      #ggtitle(paste("Seasonalities for", lubridate::month(Sys.Date(), label = TRUE, abbr = FALSE), year(Sys.Date())))+
      ggtitle(paste("ETF Performances since", month(df$Date[1], label = TRUE, abbr = FALSE)))+
      ylab("")+
      xlab("")+
      geom_dl(aes(label = ETF), method = list(dl.trans(x = x + .2), "last.bumpup", cex = 0.7))+
      geom_hline(yintercept = 0, linetype = "dashed")+
      scale_x_date(expand=c(0.1, 0), date_breaks = "1 month", date_labels = "%b %d")+
      theme_bw()+
      theme(axis.line = element_line(), 
            axis.text=element_text(color='black'), 
            axis.title = element_text(colour = 'black'), 
            legend.text=element_text(), 
            legend.title=element_text(),
            legend.position="none",
            text = element_text(size=10), 
            plot.title = element_text(size = 10))
  }
  p
}

tickers <- c("SPY", "VEU", "EZU","QQQ","IWM", "MTUM", "SPHB", "SPLV", "HYG",
             "IVE", "IVW", "VGK", "IWC","MDY", "VWO", "FXI","SLY", "ENZL", "VNQI", "VNQ", "DBC")
getSymbols(tickers, src = "yahoo", from = '2017-01-01')
df <- merge(SPY,VEU,VGK,QQQ,IWM,MTUM,SPHB,IVE,IVW,EZU,VGK,IWC,MDY,VWO,FXI,HYG,ENZL,VNQI,VNQ,SPLV,DBC)

relative_str(df, six_month=TRUE, since=month(Sys.Date()))

tickers <- c("FXY", "TLT", "IEI", "GLD", "TIP", "IEF", "AGG", "MUB", "VCIT", "EMB", "FXA", "XLU")
getSymbols(tickers, src = "yahoo", from = '2017-01-01')
df <- merge(FXY,TLT,IEI,GLD,TIP,IEF,AGG,MUB,VCIT,EMB,FXA,XLU)

relative_str(df, six_month=TRUE, since=0)

#Don't forget to go long VXX and SH

tickers <- c("SPY","EFA", "EEM", "EWA", "EWO", "EWK", "EWZ", "EWC", "EWQ", "EWG", "EWH", "INP", "EWI",
            "EWJ", "EWM", "EWW", "EWN", "RSX", "EWS", "EWY", "EWP", "EWD", "EWL", "EWT", "EWU") 

getSymbols(tickers, src = "yahoo", from = '2017-01-01')
df <- merge(SPY,EFA,EEM,EWA,EWO,EWK,EWZ,EWC,FXI,EWQ,EWG,EWH,INP,EWI,
            EWJ,EWM,EWW,EWN,RSX,EWS,EWY,EWP,EWD,EWL,EWT,EWU)

relative_str(df, six_month=TRUE, since=0)


tickers <- c("SPY",'XLE','XLV','XLI','XLU','XLP','IYZ','XLK','XLY','XLF','XLB')

getSymbols(tickers, src = "yahoo", from = '2017-01-01')
df <- merge(SPY,XLE,XLV,XLI,XLU,XLP,IYZ,XLK,XLY,XLF,XLB)

relative_str(df, six_month=TRUE, since=0)
