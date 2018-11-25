library(quantmod)   
library(lubridate)
library(ggplot2)
library(gridExtra)
Sys.setlocale("LC_TIME", "C")

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

vwap_func <- function(data, YTD = TRUE){

  my_data <- data.frame(data)
  my_data$Date <- as.Date(row.names(my_data))
  
  if(YTD == FALSE){
    index <- which(quarter(my_data$Date) == quarter(Sys.Date()) & year(my_data$Date) == year(Sys.Date()))
    data <- my_data[index,]
    indi <- "5 days"
  }
  if(YTD == TRUE){
    index <- which(year(my_data$Date) == year(Sys.Date()))
    data <- my_data[index,]
    indi <- "1 month"
  }
  x <- c("Adjusted", "Volume")
  df <- data[,grep(paste(x, collapse = "|"), names(data))]
  df$Date <- data$Date
  df_2 <- vwap_stdev(df, names(data)[grep(".Adjusted", names(data))][1], names(data)[grep(".Volume", names(data))][1])
  names(df_2) <- c("Date", names(data)[grep(".Adjusted", names(data))][1], paste0(strtrim(names(data)[grep(".Adjusted", names(data))][1],3),".vwap"), paste0(strtrim(names(data)[grep(".Adjusted", names(data))][1],3), ".stdev"))
  
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
      scale_x_date(date_breaks = indi, date_labels = "%b %d")+
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

getSymbols(c("GLD","SPY", "TLT", "JNK", "IWM", "QQQ"), from="2016-01-01")

my_data <- Reduce(function(x,y) merge(x, y), list(SPY, TLT, GLD, JNK, IWM, QQQ))
my_data <- Reduce(function(x,y) merge(x, y), list(SPY, TLT, GLD))
vwap_func(my_data, YTD = TRUE)
vwap_func(my_data, YTD = FALSE)
