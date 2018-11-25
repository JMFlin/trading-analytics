library(quantmod)
library(ggplot2)
library(scales)
library(reshape)
library(PerformanceAnalytics)
Sys.setlocale("LC_TIME", "C")

drawdowns_func_min <- function(data){
  
  clean.rtn.xts <- function(univ.rtn.xts.obj,na.replace=0){
    univ.rtn.xts.obj[is.na(univ.rtn.xts.obj)] <- na.replace
    univ.rtn.xts.obj 
  }
  
  # Create cumulative return function
  cum.rtn <- function(clean.xts.obj, g = TRUE){
    x <- clean.xts.obj
    if(g == TRUE){
      y <- cumprod(x+1)-1
    } else {
      y <- cumsum(x)
    }
    y
  }
  
  # Create function to calculate drawdowns
  dd.xts <- function(clean.xts.obj, g = TRUE){
    x <- clean.xts.obj
    if(g == TRUE){
      y <- PerformanceAnalytics:::Drawdowns(x)
    } else {
      y <- PerformanceAnalytics:::Drawdowns(x,geometric = FALSE)
    }
    y
  }
  
  cps.df <- function(xts.obj,geometric){
    x <- clean.rtn.xts(xts.obj)
    series.name <- colnames(xts.obj)[1]
    #tmp <- cum.rtn(x,geometric)
    tmp <- dd.xts(x,geometric)
    colnames(tmp) <- c("Drawdown") # names with space
    tmp.df <- as.data.frame(coredata(tmp))
    tmp.df$Date <- as.Date(index(tmp))
    tmp.df.long <- melt(tmp.df,id.var="Date")
    tmp.df.long$asset <- rep(series.name,nrow(tmp.df.long))
    tmp.df.long
  }
  
  data <- periodReturn(data[,grep(".Adjusted", names(data))], period='daily')
  
  df <- cps.df(data, geometric = TRUE)
  
  df$ind <- ifelse(df$value == 0, 0 , 1)
  vect <- c()
  p <- 0
  for(i in 1:nrow(df)){
    if(df$ind[i] == 1){
      p <- p+1
      vect <- append(vect,p,length(vect))
    }else{
      p <- 0
      vect <- append(vect,p,length(vect))
    }
  }
  df$ind2 <- vect
  
  vect_ret <- c()
  p <- 0
  vect <- c()
  for(i in 1:nrow(df)){
    if(df$ind[i] == 1){
      p <- df$value[i]
      vect <- append(vect,p,length(vect))
    }else{
      if(sum(vect) != 0){
        vect_ret <- append(vect_ret,min(vect),length(vect_ret))
      }
      vect <- c()
      p <- 0
    }
  }
  df_2 <- df[df$value %in% vect_ret[vect_ret < -0.2],]
  as.Date(df_2$Date[nrow(df_2)])
}

bull <- function(data){
  
  drawdowns_func_min <- function(data){
    
    clean.rtn.xts <- function(univ.rtn.xts.obj,na.replace=0){
      univ.rtn.xts.obj[is.na(univ.rtn.xts.obj)] <- na.replace
      univ.rtn.xts.obj 
    }
    
    # Create cumulative return function
    cum.rtn <- function(clean.xts.obj, g = TRUE){
      x <- clean.xts.obj
      if(g == TRUE){
        y <- cumprod(x+1)-1
      } else {
        y <- cumsum(x)
      }
      y
    }
    
    # Create function to calculate drawdowns
    dd.xts <- function(clean.xts.obj, g = TRUE){
      x <- clean.xts.obj
      if(g == TRUE){
        y <- PerformanceAnalytics:::Drawdowns(x)
      } else {
        y <- PerformanceAnalytics:::Drawdowns(x,geometric = FALSE)
      }
      y
    }
    
    cps.df <- function(xts.obj,geometric){
      x <- clean.rtn.xts(xts.obj)
      series.name <- colnames(xts.obj)[1]
      #tmp <- cum.rtn(x,geometric)
      tmp <- dd.xts(x,geometric)
      colnames(tmp) <- c("Drawdown") # names with space
      tmp.df <- as.data.frame(coredata(tmp))
      tmp.df$Date <- as.Date(index(tmp))
      tmp.df.long <- melt(tmp.df,id.var="Date")
      tmp.df.long$asset <- rep(series.name,nrow(tmp.df.long))
      tmp.df.long
    }
    
    data <- periodReturn(data[,grep(".Adjusted", names(data))], period='daily')
    
    df <- cps.df(data, geometric = TRUE)
    
    df$ind <- ifelse(df$value == 0, 0 , 1)
    vect <- c()
    p <- 0
    for(i in 1:nrow(df)){
      if(df$ind[i] == 1){
        p <- p+1
        vect <- append(vect,p,length(vect))
      }else{
        p <- 0
        vect <- append(vect,p,length(vect))
      }
    }
    df$ind2 <- vect
    
    vect_ret <- c()
    p <- 0
    vect <- c()
    for(i in 1:nrow(df)){
      if(df$ind[i] == 1){
        p <- df$value[i]
        vect <- append(vect,p,length(vect))
      }else{
        if(sum(vect) != 0){
          vect_ret <- append(vect_ret,min(vect),length(vect_ret))
        }
        vect <- c()
        p <- 0
      }
    }
    df_2 <- df[df$value %in% vect_ret[vect_ret < -0.2],]
    as.Date(df_2$Date[nrow(df_2)])
  }
  
  DATE <- drawdowns_func_min(data)
  
  SPY <- data.frame(data)
  SPY$Date <- as.Date(row.names(SPY))
  spy_df <- data.frame(Date = SPY$Date[SPY$Date >= DATE], 
                       SPY = SPY[,grep(".Adjusted", names(data))][SPY$Date >= DATE], 
                       reg = lm(SPY[,grep(".Adjusted", names(data))][SPY$Date >= DATE] ~ SPY$Date[SPY$Date >= DATE])$residuals)
  
  a <- ggplot(spy_df, aes(x=Date, y=SPY, group = 1))+
    geom_line(aes(x=Date, y=SPY))+
    ylab("")+
    xlab("")+
    ggtitle("Bull Market to Date")+
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
  
  b <- ggplot(spy_df, aes(x=Date, y=reg, group = 1))+
    geom_line(aes(x=Date, y=reg))+
    ylab("")+
    xlab("")+
    geom_hline(yintercept = 0)+
    geom_hline(yintercept = 0-2*sd(spy_df$reg), col = "blue", linetype = "dashed")+
    geom_hline(yintercept = 0+2*sd(spy_df$reg), col = "red", linetype = "dashed")+
    ggtitle("Residuals")+
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
  
  grid.arrange(a,b,ncol = 1)
}


bull_mean_mad <- function(my_data){

  my_data <- data.frame(my_data)
  my_data$Date <- as.Date(row.names(my_data))

  median_mad <- function(data, price){
    cum_med <- runMedian(data[,price], n = 1, non.unique = "mean", cumulative = TRUE)
    cum_mad <- runMAD(data[,price], n = 1, center = NULL, stat = "median", constant = 1.4826, non.unique = "mean", cumulative = TRUE)
    return(data.frame(Date = data[,"Date"], Price = data[,price], cum_med, cum_mad))
  }

  df <- my_data[,grep(".Adjusted", names(my_data))]
  df_2 <- data.frame(median_mad(my_data, names(my_data)[grep(".Adjusted", names(my_data))]))
  names(df_2) <- c("Date", names(my_data)[grep(".Adjusted", names(my_data))][1], paste0(strtrim(names(my_data)[grep(".Adjusted", names(my_data))][1],3),".med"), paste0(strtrim(names(my_data)[grep(".Adjusted", names(my_data))][1],3), ".mad"))

  if(length(names(my_data)[grep(".Adjusted", names(my_data))]) != 1){
    df$Date <- my_data$Date
    for(i in names(df)[2:(ncol(df)-1)]){
      df_2 <- cbind(df_2, median_mad(df, i))
      names(df_2)[(ncol(df_2)-3):ncol(df_2)] <- c("Date", i, paste0(strtrim(i,3),".med"), paste0(strtrim(i,3),".mad"))
    }
  }

  for(i in 1:length(names(df_2)[grep(".Adjusted", names(df_2))])){
  
    subset_d <- df_2[,grep(strtrim(names(df_2)[grep(".Adjusted", names(df_2))][i],3), names(df_2))]
    subset_d$Date <- df_2$Date
  
    title <- paste("Median and MAD for", names(df_2)[grep(".Adjusted", names(df_2))][i], lubridate::month(Sys.Date(), label = TRUE, abbr = FALSE), year(Sys.Date()))
  
    a <- ggplot(subset_d, aes_string(y=names(df_2)[grep(".Adjusted", names(df_2))][i], x="Date"))+
      geom_line()+
      geom_line(aes_string(y=names(df_2)[grep(".med", names(df_2))][i], x="Date"), linetype = "dashed")+
      geom_line(aes(x=Date, y=eval(as.name(paste(names(df_2)[grep("med", names(df_2))][i])))+eval(as.name(paste(names(df_2)[grep("mad", names(df_2))][i])))), col = "green", linetype = "dashed")+
      geom_line(aes(x=Date, y=eval(as.name(paste(names(df_2)[grep("med", names(df_2))][i])))-eval(as.name(paste(names(df_2)[grep("mad", names(df_2))][i])))), col = "green", linetype = "dashed")+
      geom_line(aes(x=Date, y=eval(as.name(paste(names(df_2)[grep("med", names(df_2))][i])))+2*eval(as.name(paste(names(df_2)[grep("mad", names(df_2))][i])))), col = "red", linetype = "dashed")+
      geom_line(aes(x=Date, y=eval(as.name(paste(names(df_2)[grep("med", names(df_2))][i])))-2*eval(as.name(paste(names(df_2)[grep("mad", names(df_2))][i])))), col = "blue", linetype = "dashed")+
      geom_line(aes(x=Date, y=eval(as.name(paste(names(df_2)[grep("med", names(df_2))][i])))+3*eval(as.name(paste(names(df_2)[grep("mad", names(df_2))][i])))), col = "red", linetype = "dashed")+
      geom_line(aes(x=Date, y=eval(as.name(paste(names(df_2)[grep("med", names(df_2))][i])))-3*eval(as.name(paste(names(df_2)[grep("mad", names(df_2))][i])))), col = "blue", linetype = "dashed")+
      xlab("")+
      ylab("")+
      ggtitle(title)+
      scale_x_date(date_breaks = "1 years", date_labels = "%Y")+
      theme_bw()+
      theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
    plot(a)
  }
}

vwap_stdev <- function(data, price, volume){
  Px <- data[,price]
  Vol <- data[,volume]
  PxVol <- Px*Vol
  SPxVol <- cumsum(PxVol)
  SVol <- cumsum(Vol)
  VWAP <- SPxVol/SVol
  
  stdev <- cumsum(rollapplyr(VWAP, 2, sd, fill = 0))
  return(data.frame(Date = data[,"Date"], Price = data[,price], VWAP, stdev))
}

bull_vwap <- function(data){
  my_data <- data.frame(data)
  my_data$Date <- as.Date(row.names(my_data))

  x <- c("Adjusted", "Volume")
  df <- my_data[,grep(paste(x, collapse = "|"), names(my_data))]
  df$Date <- my_data$Date
  df_2 <- vwap_stdev(df, names(my_data)[grep(".Adjusted", names(my_data))][1], names(my_data)[grep(".Volume", names(my_data))][1])
  names(df_2) <- c("Date", names(my_data)[grep(".Adjusted", names(my_data))][1], paste0(strtrim(names(my_data)[grep(".Adjusted", names(my_data))][1],3),".vwap"), paste0(strtrim(names(my_data)[grep(".Adjusted", names(my_data))][1],3), ".stdev"))

  vwap_stdev <- function(data, price, volume){
    Px <- data[,price]
    Vol <- data[,volume]
    PxVol <- Px*Vol
    SPxVol <- cumsum(PxVol)
    SVol <- cumsum(Vol)
    VWAP <- SPxVol/SVol
  
    stdev <- cumsum(rollapplyr(VWAP, 2, sd, fill = 0))
    return(data.frame(Date = data[,"Date"], Price = data[,price], VWAP, stdev))
  }

  if(length(names(df)[grep(".Adjusted", names(df))]) > 1){
    for(i in 2:length(names(df)[grep(".Adjusted", names(df))])){
      subset_d <- df[,names(df)[grep(strtrim(names(df)[grep(".Adjusted", names(df))][i],3), names(df))]]
      subset_d$Date <- df$Date
    
      df_2 <- cbind(df_2, vwap_stdev(subset_d, names(subset_d)[grep(".Adjusted", names(subset_d))], names(subset_d)[grep(".Volume", names(subset_d))]))
    
      names(df_2)[(ncol(df_2)-3):ncol(df_2)] <- c("Date", names(subset_d)[grep(".Adjusted", names(subset_d))][1], paste0(strtrim(names(subset_d)[grep(".Adjusted", names(subset_d))][1],3),".vwap"), paste0(strtrim(names(subset_d)[grep(".Adjusted", names(subset_d))][1],3),".stdev"))
    }
  }

  for(i in 1:length(names(df_2)[grep(".Adjusted", names(df_2))])){
  
    subset_d <- df_2[,grep(strtrim(names(df_2)[grep(".Adjusted", names(df_2))][i],3), names(df_2))]
    subset_d$Date <- df_2$Date
  
    title <- paste("VWAP for", names(subset_d)[grep(".Adjusted", names(subset_d))], lubridate::month(Sys.Date(), label = TRUE, abbr = FALSE), year(Sys.Date()))
  
    a <- ggplot(subset_d, aes_string(y=names(subset_d)[grep(".Adjusted", names(subset_d))], x="Date"))+
      geom_line()+
      geom_line(aes_string(y=names(subset_d)[grep("vwap", names(subset_d))], x="Date"), linetype = "dashed")+
      geom_line(aes(x=Date, y=eval(as.name(paste(names(subset_d)[grep("vwap", names(subset_d))])))+eval(as.name(paste(names(subset_d)[grep("stdev", names(subset_d))])))), col = "green", linetype = "dashed")+
      geom_line(aes(x=Date, y=eval(as.name(paste(names(subset_d)[grep("vwap", names(subset_d))])))-eval(as.name(paste(names(subset_d)[grep("stdev", names(subset_d))])))), col = "green", linetype = "dashed")+
      geom_line(aes(x=Date, y=eval(as.name(paste(names(subset_d)[grep("vwap", names(subset_d))])))+2*eval(as.name(paste(names(subset_d)[grep("stdev", names(subset_d))])))), col = "red", linetype = "dashed")+
      geom_line(aes(x=Date, y=eval(as.name(paste(names(subset_d)[grep("vwap", names(subset_d))])))-2*eval(as.name(paste(names(subset_d)[grep("stdev", names(subset_d))])))), col = "blue", linetype = "dashed")+
      geom_line(aes(x=Date, y=eval(as.name(paste(names(subset_d)[grep("vwap", names(subset_d))])))+3*eval(as.name(paste(names(subset_d)[grep("stdev", names(subset_d))])))), col = "red", linetype = "dashed")+
      geom_line(aes(x=Date, y=eval(as.name(paste(names(subset_d)[grep("vwap", names(subset_d))])))-3*eval(as.name(paste(names(subset_d)[grep("stdev", names(subset_d))])))), col = "blue", linetype = "dashed")+
      xlab("")+
      ylab("")+
      ggtitle(title)+
      scale_x_date(date_breaks = "1 years", date_labels = "%Y")+
      theme_bw()+
      theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
    plot(a)
  }
}

getSymbols("SPY", from = "2000-01-01")
getSymbols("IWM", from = "2000-01-01")
getSymbols("QQQ", from = "2000-01-01")

bull(SPY)#only for SPY
date <- drawdowns_func_min(SPY)
SPY <- SPY[index(SPY)>=date]
bull_mean_mad(SPY)
bull_vwap(SPY)

QQQ <- QQQ[index(QQQ)>=date]
bull_mean_mad(QQQ)
bull_vwap(QQQ)

IWM <- IWM[index(IWM)>=date]
bull_mean_mad(IWM)
bull_vwap(IWM)

#Bear market: a 20% market decline from the previous all time high - J.P. Morgan AM
#Bull: From the lowest close reached after the market has fallen 20% or more, to the next market high. 
#Bear: From when the index closes at least 20% down from its previous high close, through the lowest close reached after it has fallen 20% or more.
#https://www.ftportfolios.com/Common/ContentFileLoader.aspx?ContentGUID=4ecfa978-d0bb-4924-92c8-628ff9bfe12d