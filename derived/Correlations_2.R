library(quantmod)   
library(lubridate)
library(ggplot2)
library(gridExtra)
Sys.setlocale("LC_TIME", "C")

corr_to_spy <- function(data, width = 120){
  
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
  longtemp <- melt(correlation, measure.vars = names(correlation)[3:(ncol(correlation)-1)])
  
  ggplot(longtemp, aes(y=value, x=date))+
    geom_line()+
    geom_point(size = 0.1)+
    ylab("")+
    xlab("")+
    geom_hline(yintercept = 0)+
    scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
    facet_wrap(~ variable, ncol = 2, scales = "free")+
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


corr_all <- function(data, width = 120){
  
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
  rollingcorr.1m.df$Date <- as.Date(row.names(rollingcorr.1m.df))
  plots_list <- list()
  
  for(i in names(rollingcorr.1m.df)[1:(ncol(rollingcorr.1m.df)-1)]) local({
    i <- i
    a <- ggplot(data = rollingcorr.1m.df, aes_string(x = "Date", y = i)) +
      geom_ribbon(aes(ymin = 0, ymax = rollingcorr.1m.df[,i])) +
      ggtitle(i)+ 
      ylab("")+
      xlab("")+
      theme_bw()+
      theme(axis.line = element_line(), 
            axis.text=element_text(color='black'), 
            axis.title = element_text(colour = 'black'), 
            legend.text=element_text(), 
            legend.title=element_text(),
            text = element_text(size=10),
            legend.position = "none")
    plots_list <<- c(plots_list, list(a)) 
  })
  plots_list
}

ticker <- c("TLT", "FXY", "GLD", "USO", "UUP","SPY")  
sector <- c("Bonds", "Yen","Gold", "Crude", "Dollar","SPY")
etf_ticker_sector <- data.frame(ticker, sector)
symbols <- getSymbols(ticker, from = "2008-01-01")

corr_to_spy(symbols, width = 120)
corr_to_spy(symbols, width = 365)

plots_list <- corr_all(symbols, width = 120)
grid.arrange(plots_list[[1]],plots_list[[2]],plots_list[[3]],plots_list[[4]], ncol = 2, nrow = 2)
grid.arrange(plots_list[[5]],plots_list[[6]],plots_list[[7]],plots_list[[8]], ncol = 2, nrow = 2)
grid.arrange(plots_list[[9]],plots_list[[10]], plots_list[[11]], plots_list[[12]],ncol = 2, nrow = 2)
grid.arrange(plots_list[[13]], plots_list[[14]], plots_list[[15]],ncol = 2, nrow = 2)
#grid.arrange(plots_list[[17]], plots_list[[18]], plots_list[[19]],plots_list[[20]],ncol = 2, nrow = 2)
#grid.arrange(plots_list[[21]], ncol = 2, nrow = 2)
