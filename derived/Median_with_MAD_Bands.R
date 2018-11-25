library(reshape2)
library(quantmod)   
library(lubridate)
library(ggplot2)
library(gridExtra)
Sys.setlocale("LC_TIME", "C")

Med_Mad <- function(x, YTD = FALSE, spread = FALSE){
  
  my_data <- data.frame(x)
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
  
  median_mad <- function(data, price){
    cum_med <- runMedian(data[,price], n = 1, non.unique = "mean", cumulative = TRUE)
    cum_mad <- runMAD(data[,price], n = 1, center = NULL, stat = "median", constant = 1.4826, non.unique = "mean", cumulative = TRUE)
    return(data.frame(Date = data[,"Date"], Price = data[,price], cum_med, cum_mad))
  }
  
  if(spread == FALSE){
    df <- data[,grep(".Adjusted", names(data))]
    df_2 <- data.frame(median_mad(data, grep(".Adjusted", names(data))[1]))
    names(df_2) <- c("Date", names(data)[grep(".Adjusted", names(data))][1], paste0(strtrim(names(data)[grep(".Adjusted", names(data))][1],3),".med"), paste0(strtrim(names(data)[grep(".Adjusted", names(data))][1],3), ".mad"))
  
    if(length(names(data)[grep(".Adjusted", names(data))]) != 1){
      df$Date <- data$Date
      for(i in names(df)[2:(ncol(df)-1)]){
        df_2 <- cbind(df_2, median_mad(df, i))
        names(df_2)[(ncol(df_2)-3):ncol(df_2)] <- c("Date", i, paste0(strtrim(i,3),".med"), paste0(strtrim(i,3),".mad"))
      }
    }
    
    for(i in 1:length(names(df_2)[grep(".Adjusted", names(df_2))])){
  
      subset_d <- df_2[,grep(strtrim(names(df_2)[grep(".Adjusted", names(df_2))][i],3), names(df_2))]
      subset_d$Date <- df_2$Date
    
      title <- paste("Median and MAD for", names(df_2)[grep(".Adjusted", names(df_2))][i], paste0("Q",lubridate::quarter(Sys.Date())), year(Sys.Date()))

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
  if(spread == TRUE){
    df <- data
    df_2 <- data.frame(median_mad(data, 1))
    names(df_2) <- c("Date", names(data)[1], paste0(names(data)[1],".med"), paste0(names(data)[1], ".mad"))
    
    if(length(names(df)) != 2){ #[2:(ncol(df)-1)]
      for(i in names(df)[2:(ncol(df)-1)]){
        df_2 <- cbind(df_2, median_mad(df, i))
        names(df_2)[(ncol(df_2)-3):ncol(df_2)] <- c("Date", i, paste0(i,".med"), paste0(i,".mad"))
      }
    }
    for(i in names(df)[1:(ncol(df)-1)]){
      
      subset_d <- df_2[,grep(i, names(df_2))]
      subset_d$Date <- df_2$Date

      title <- paste("Median and MAD for", i, paste0("Q",lubridate::quarter(Sys.Date())), year(Sys.Date()))
      title <- sub("\\.","/", title)
      a <- ggplot(subset_d, aes_string(y=names(subset_d)[1], x="Date"))+
        geom_line()+
        geom_line(aes_string(y=names(subset_d)[grep(".med", names(subset_d))], x="Date"), linetype = "dashed")+
        geom_line(aes(x=Date, y=eval(as.name(paste(names(subset_d)[grep("med", names(subset_d))])))+eval(as.name(paste(names(subset_d)[grep("mad", names(subset_d))])))), col = "green", linetype = "dashed")+
        geom_line(aes(x=Date, y=eval(as.name(paste(names(subset_d)[grep("med", names(subset_d))])))-eval(as.name(paste(names(subset_d)[grep("mad", names(subset_d))])))), col = "green", linetype = "dashed")+
        geom_line(aes(x=Date, y=eval(as.name(paste(names(subset_d)[grep("med", names(subset_d))])))+2*eval(as.name(paste(names(subset_d)[grep("mad", names(subset_d))])))), col = "red", linetype = "dashed")+
        geom_line(aes(x=Date, y=eval(as.name(paste(names(subset_d)[grep("med", names(subset_d))])))-2*eval(as.name(paste(names(subset_d)[grep("mad", names(subset_d))])))), col = "blue", linetype = "dashed")+
        geom_line(aes(x=Date, y=eval(as.name(paste(names(subset_d)[grep("med", names(subset_d))])))+3*eval(as.name(paste(names(subset_d)[grep("mad", names(subset_d))])))), col = "red", linetype = "dashed")+
        geom_line(aes(x=Date, y=eval(as.name(paste(names(subset_d)[grep("med", names(subset_d))])))-3*eval(as.name(paste(names(subset_d)[grep("mad", names(subset_d))])))), col = "blue", linetype = "dashed")+
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
}

spread_df <- function(my_data){
  tt <- my_data[,grep(".Adjusted", names(my_data))]
  p <- list()
  for(i in names(tt)){
    u <- which(colnames(tt)==i)
    if(u != length(names(tt))){
      nam <- names(tt)[(u+1):length(names(tt))]
      for(j in nam){
        a <- my_data[,i]/my_data[,j]
        p <- c(p, list(a))
      }
    }
  }
  df <- do.call(cbind.data.frame, p)
  
  vect <- c()
  for(i in names(tt)){
    u <- which(colnames(tt)==i)
    if(u != length(names(tt))){
      nam <- names(tt)[(u+1):length(names(tt))]
      for(j in nam){
        vect <- append(vect, paste0(strtrim(i,3),"/",strtrim(j,3)), length(vect))
      }
    }
  }
  names(df) <- vect
  return(df)
}
#"FXY", "HYG", "IEF", "IEI"
#c("SPY", "XLY", "XLP", "XLE","XLF", "XLV", "XLI", "XLB", "XLK", "XLU", "IYZ", "TLT")
getSymbols(c("GLD", "SPY", "TLT", "HYG"), from="2016-01-01")

my_data <- Reduce(function(x,y) merge(x, y), list(GLD, SPY, TLT, HYG))
Med_Mad(my_data, YTD = TRUE, spread = FALSE)
Med_Mad(my_data, YTD = FALSE, spread = FALSE)

my_data <- Reduce(function(x,y) merge(x, y), list(GLD, TLT, SPY, HYG))
my_data <- spread_df(my_data)
Med_Mad(my_data, YTD = TRUE, spread = TRUE)
Med_Mad(my_data, YTD = FALSE, spread = TRUE)
