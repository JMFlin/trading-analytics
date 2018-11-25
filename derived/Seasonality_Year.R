library(quantmod)   
library(lubridate)
library(ggplot2)
library(reshape2)
library(gridExtra)
Sys.setlocale("LC_TIME", "C")

year_seasonality <- function(data, since = "1950-01-01"){
  data <- data[index(data) >= since,]
  data_years <- unique(year(index(data)))
  data_years <- data_years[data_years != year(Sys.Date())]
  temp <- data.frame(data[,grep(".Adjusted", names(data))])
  temp$Date <- as.Date(row.names(temp))
  
  l <- list()
  
  for(i in 1:length(data_years)){
    l[[i]] <- temp[year(temp$Date) == data_years[i],]
  }
  
  max.len <- c()
  for(i in 1:length(l)){
    max.len <- append(max.len, nrow(l[[i]]), length(max.len))
  }
  max.len <- max(max.len)
  
  for(i in 1:length(l)){
    names(l[[i]])[1] <- "GSPC.Adjusted"
  }
  
  for(i in 1:length(l)){
    l[[i]] <- rbind(l[[i]], data.frame("GSPC.Adjusted" = c(rep(NA, max.len-nrow(l[[i]]))), "Date" = c(rep(NA, max.len-nrow(l[[i]])))))
  }
  
  df <- l[[1]]
  for(i in 2:length(l)){
    df <- cbind(df, l[[i]])
  }
  
  
  df <- na.omit(df)
  df$Mean <- rowMeans(df[,names(df) %in% names(df[,grep("GSPC.Adjusted", names(df))])])
  df$Index <- seq(1,nrow(df),1)
  
  
  data_years <- unique(year(index(data)))
  data_years <- data_years[data_years == year(Sys.Date())]
  temp <- data.frame(data[,grep(".Adjusted", names(data))])
  temp$Date <- as.Date(row.names(temp))
  temp <- temp[year(temp$Date) == data_years,]
  temp$Index <- seq(1,nrow(temp),1)

  ggplot(df, aes(x=Index, y=Mean))+
    geom_line()+
    geom_point(data=df[df$Index == temp$Index[nrow(temp)],], aes(x=Index, y=Mean), col = "red")+
    #geom_line(aes(x=Index, y=Median), col = "green")+
    #geom_point(data=df[df$Index == temp$Index[nrow(temp)],], aes(x=Index, y=Median), col = "red")+
    theme_bw()+
    ggtitle(paste(names(data[,grep(".Adjusted", names(data))]), "Seasonality", paste0(year(df$Date[1]), "-", year(df[nrow(df), (ncol(df)-2)]))))+ 
    ylab("")+
    xlab("")+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          text = element_text(size=10),
          legend.position = "none")
}

getSymbols("TLT", from = "1990-01-01")
getSymbols("^GSPC", from = "1950-01-01")
getSymbols("^VIX", from = "1950-01-01")

year_seasonality(GSPC, since = "1950-01-01")
year_seasonality(GSPC, since = "1990-01-01")
year_seasonality(VIX, since = "1990-01-01")
year_seasonality(TLT, since = "2003-01-01") # this has to be the first of jan

