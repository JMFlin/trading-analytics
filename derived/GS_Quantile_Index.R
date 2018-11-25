library(quantmod)
library(ggplot2)
library(scales)
library(reshape)
library(lubridate)
library(gridExtra)
library(Quandl)
Sys.setlocale("LC_TIME", "C")
Quandl.api_key("Ykn-RwEHL2VBzptNdD6x")

bear_market_risk <- function(data, from = "1976-06-01"){
  df <- data[index(data) >= from]
  df_df <- data.frame(df)
  
  numRows <- 12*5
  
  tmp <- data.frame(df_df[1:numRows,1])
  names(tmp) <- names(df_df)[1]
  tmp$percentile <- ecdf(tmp[,1])(tmp[,1])
  other <- tmp
  
  for(i in 2:ncol(df_df)){
    tmp <- data.frame(df_df[1:numRows,i])
    names(tmp) <- names(df_df)[i]
    tmp$percentile <- ecdf(tmp[,1])(tmp[,1])
    other <- cbind(other, tmp)
  }
  names(other)[seq(2,ncol(other), 2)] <- paste0(names(other)[seq(1,ncol(other), 2)], ".percentile")
  other <- other[,names(other)[seq(2,ncol(other), 2)]]
  
  for(j in (numRows+1):nrow(df_df)){
    tmp <- data.frame(df_df[1:j,])
    names(tmp) <- names(df_df)
    a <- ecdf(tmp[,1])(tmp[,1])
    b <- ecdf(tmp[,2])(tmp[,2])
    c <- ecdf(tmp[,3])(tmp[,3])
    d <- ecdf(tmp[,4])(tmp[,4])
    e <- ecdf(tmp[,5])(tmp[,5])
    f <- data.frame(a,b,c,d,e)
    names(f) <- names(other)
    other <- rbind(other, f[nrow(f),])
  }
  
  row.names(other) <- row.names(df_df)
  
  other$Mean <- rowMeans(other, na.rm = TRUE)
  other$Date <- as.Date(row.names(other))
  
  a <- ggplot(other, aes(x=Date, y=Mean))+
    geom_line()+
    geom_point(data=other[nrow(other),], aes(x=Date, y=Mean), col = "blue", alpha = 0.7)+
    geom_hline(yintercept = 0.7, linetype = "longdash", alpha = 0.7)+
    geom_hline(yintercept = 0.3, linetype = "longdash", alpha = 0.7)+
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    ylab("") +
    xlab("")+
    ggtitle(paste("Bear Market Risk Indicator as of", Sys.Date(), "for", other$Date[nrow(other)]))+
    theme_bw()+
    theme(axis.line = element_line(),
          axis.text=element_text(color='black'),
          axis.title = element_text(colour = 'black'),
          legend.text=element_text(),
          legend.title=element_text(),
          legend.position='none')
  
  df <- merge(UNEMPLOY,T10Y2Y, CPILFESL, SP, ism.comp)
  df <- df[index(df) >= from]
  df_df <- data.frame(df)
  other <- df_df
  
  for(i in 1:ncol(df_df)){
    other[,i] <- ecdf(other[,i])(other[,i])
  }
  other$Mean <- rowMeans(other, na.rm = TRUE)
  other$Date <- as.Date(row.names(df_df))
  
  b <- ggplot(other, aes(x=Date, y=Mean))+
    geom_line()+
    geom_point(data=other[nrow(other),], aes(x=Date, y=Mean), col = "blue", alpha = 0.7)+
    geom_hline(yintercept = 0.7, linetype = "longdash", alpha = 0.7)+
    geom_hline(yintercept = 0.3, linetype = "longdash", alpha = 0.7)+
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    ylab("") +
    xlab("")+
    ggtitle("Bear Market Risk Indicator Second Variant")+
    theme_bw()+
    theme(axis.line = element_line(),
          axis.text=element_text(color='black'),
          axis.title = element_text(colour = 'black'),
          legend.text=element_text(),
          legend.title=element_text(),
          legend.position='none')
  
  grid.arrange(a, b, ncol = 1)
  tmp <- data.frame(apply(other, 2, function(x) length(which(!is.na(x)))))
  print(paste("Latest observation uses", sum(tmp[,1] == nrow(other))-2, "of the possible", ncol(other)-2,"time series"))
}

getSymbols(c("UNEMPLOY", "CPILFESL", "T10Y2Y"), src = "FRED", from = "1950-01-01")
ism.comp <- Quandl("ISM/MAN_PMI", collapse = "monthly", type = "xts", trim_start = "1950-01-01")
SPCOMP <- Quandl("YALE/SPCOMP", collapse = "monthly" , type = "xts", trim_start = "1950-01-01")
SP <- SPCOMP[,"Cyclically Adjusted PE Ratio"]

T10Y2Y <- to.monthly(T10Y2Y)
T10Y2Y <- T10Y2Y[,"T10Y2Y.Close"]
T10Y2Y <- T10Y2Y*-1
CPILFESL <- Delt(CPILFESL, k = 12)
UNEMPLOY <- UNEMPLOY*-1

df <- merge(UNEMPLOY,T10Y2Y, CPILFESL, SP, ism.comp)

bear_market_risk(df, from = "1976-06-01") #1976-06-01
#https://heisenbergreport.com/2017/09/14/one-banks-fabulous-5-bear-market-indicators-are-flashing-yellow/