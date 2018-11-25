library(quantmod)   
library(lubridate)
library(ggplot2)
library(gridExtra)
Sys.setlocale("LC_TIME", "C")

SIT_seasonality <- function(data){
  #data <- df
  data <- data.frame(Ad(data))
  
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
  index <- which(month(dates) == month(Sys.Date()))
  
  trading.days <- sapply(tapply(ret[index], years[index], function(x) coredata(x)), function(x) x[1:22])
  
  avg.trading.days <- apply(trading.days[, -ncol(trading.days)], 1, mean, na.rm=T)
  med.trading.days <- apply(trading.days[, -ncol(trading.days)], 1, median, na.rm=T)
  current.year <- trading.days[, ncol(trading.days)]
  
  avg.trading.days <- 100*(cumprod(1 + avg.trading.days)-1)
  med.trading.days <- 100*(cumprod(1 + med.trading.days)-1)
  current.year <- 100*(cumprod(1 + current.year)- 1)
  
  df <- data.frame(avg.trading.days, med.trading.days, current.year)
  names(df) <- c("avg", "med", "curr")
  df$Date <- factor(row.names(df), as.character(row.names(df)))
  #df$name <- strtrim(names(data),3)[1]
  df$name <- gsub("\\..*$","", names(data)[1])
  
  for(i in 2:ncol(data)){
    
    prices <- data[,i]
    n <- len(tickers)  
    ret <- prices / mlag(prices) - 1
    
    dates <- row.names(data)
    years <- year(dates)    
    index <- which(month(dates) == month(Sys.Date()))
    
    trading.days <- sapply(tapply(ret[index], years[index], function(x) coredata(x)), function(x) x[1:22])
    
    avg.trading.days <- apply(trading.days[, -ncol(trading.days)], 1, mean, na.rm=T)
    med.trading.days <- apply(trading.days[, -ncol(trading.days)], 1, median, na.rm=T)
    current.year <- trading.days[, ncol(trading.days)]
    
    avg.trading.days <- 100*(cumprod(1 + avg.trading.days)-1)
    med.trading.days <- 100*(cumprod(1 + med.trading.days)-1)
    current.year <- 100*(cumprod(1 + current.year)- 1)
    
    df_tmp <- data.frame(avg.trading.days, med.trading.days, current.year)
    names(df_tmp) <- c("avg", "med", "curr")
    df_tmp$Date <- factor(row.names(df_tmp), as.character(row.names(df_tmp)))
    #df_tmp$name <- strtrim(names(data),3)[i]
    df_tmp$name <- gsub("\\..*$","", names(data)[i])
    df <- rbind(df, df_tmp)
  }
  
  ggplot(df, aes(y=avg, x=Date, group = 1))+
    geom_line()+
    geom_line(aes(y=curr, x=Date), color = 'red', linetype="longdash")+
    geom_point()+
    geom_point(aes(y=curr, x=Date), color = 'red')+
    geom_line(aes(y=med, x=Date), color = 'darkgreen', linetype="dashed")+
    geom_point(aes(y=med, x=Date), color = 'darkgreen')+
    geom_hline(yintercept = 0, linetype = "dashed")+
    facet_wrap(~name, scale = "free", ncol = 2)+
    #ggtitle(paste("Seasonalities for", lubridate::month(Sys.Date(), label = TRUE, abbr = FALSE), year(Sys.Date())))+
    ggtitle("")+
    ylab("")+
    xlab("")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          text = element_text(size=10),
          plot.title = element_text(size = 10))
}

tickers <- c('SPY',"TLT","^VIX","GLD","UUP", "^GSPC")
getSymbols(tickers, src = 'yahoo', from = '1950-01-01')
df <- merge(SPY,TLT,VIX,GLD,UUP) #GSPC
SIT_seasonality(df)
#names(df)

#July is a seasonal tailwind, and several sentiment indicators suggest a bias higher (to the top of the range) is warranted. 
#On strength this month, beware; it is followed by the two worst months of the year.