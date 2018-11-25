library(ggplot2)
library(reshape2)
library(MASS)
library(gridExtra)
library(quantmod)
library(lubridate)
library(quantreg)
library(robustbase)
library(PerformanceAnalytics)
Sys.setlocale("LC_TIME", "C")

drawdowns_func_min <- function(data){
  
  clean.rtn.xts <- function(univ.rtn.xts.obj,na.replace=0){
    univ.rtn.xts.obj[is.na(univ.rtn.xts.obj)] <- na.replace
    univ.rtn.xts.obj 
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

reg_func <- function(data, period = "weekly", se = TRUE, delt = FALSE, lag = -1){
  data <- na.omit(data)
  len <- function(x){
    length(x)
  }
  
  mlag <- function(m, nlag = lag){
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
  
  ticker <- c()
  for(i in 1:length(names(data))){
    ticker <- append(ticker, gsub("\\..*$","", names(data)[i]))
    #ticker <- append(ticker ,strtrim(names(data)[i],3),length(ticker))
  }
  
  if(delt == FALSE){
    etf_returns <- do.call(merge, lapply(data, 
                                       function(x) periodReturn(x, period = period, type = 'arithmetic')))
    etf_ticker_sector <- data.frame(ticker)
    colnames(etf_returns) <- etf_ticker_sector$ticker
    z.logret <- data.frame(etf_returns)
  }else{
    etf_returns <- data
    etf_ticker_sector <- data.frame(ticker)
    colnames(etf_returns) <- etf_ticker_sector$ticker
    z.logret <- data.frame(etf_returns)
  }
  
  z.logret$SPY_lead <- mlag(z.logret[,1], nlag = lag)
  z.logret <- na.omit(z.logret)
  tmp <- 1:length(z.logret)
  z.logret <- z.logret[,c(tmp[length(tmp)], 1:(length(tmp)-1))]
  
  lm_eqn <- function(m){
    
    l <- list(a = format(coef(m)[1], digits = 2),
              b = format(abs(coef(m)[2]), digits = 2),
              r2 = format(summary(m)$r.squared, digits = 3)); 
    
    if (coef(m)[2] >= 0)  {
      eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
    } else {
      eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)   
    }
    
    as.character(as.expression(eq));                
  }
  
  plots_list <- list()
  z.logret <- na.omit(z.logret)
  for(i in names(z.logret)){
    u <- which(colnames(z.logret)==i)
    
    if(u != length(names(z.logret))){
      nam <- names(z.logret)[(u+1):length(names(z.logret))]
      for(j in nam){ 
        k <- z.logret[,i]
        p <- z.logret[,j]
        #rlm.fit <- rlm(k ~ p)
        res1b <- rq(k ~ p)
        resid1 <- round(res1b$residuals, 7) 
        resid2 <- resid1[resid1 != 0]
        sigma <- (1/0.675)*median(abs(resid2))
        rlm.fit <- lmrob(k ~ p, method = "MM",
                         init = list(coefficients = res1b$coef, scale = sigma),
                         setting = "KS2014")
        lm.fit <- lm(k ~ p) 
        a <- ggplot(data = z.logret, aes_string(x = j, y = i)) +
          geom_smooth(method = function(formula,data,weights=weight) lmrob(formula,
                                                                           data,
                                                                           weights=weight,
                                                                           method="MM",
                                                                           init = list(coefficients = res1b$coef, scale = sigma),
                                                                           setting = "KS2014"),
                      colour = "red", size = 0.3, formula = y ~ x, se = se) +
          geom_smooth(method = "lm", colour = "black", size = 0.3, formula = y ~ x, se = se) +
          geom_point(alpha = 0.3) +
          geom_point(data=subset(z.logret, row.names(z.logret)[nrow(z.logret)] == row.names(z.logret)), colour="red")+
          geom_hline(yintercept = 0, col = "black", alpha = 0.5, linetype = "longdash")+
          geom_vline(xintercept = 0, col = "black", alpha = 0.5, linetype = "longdash")+
          ggtitle("")+ #paste(as.Date(row.names(z.logret)[1]), "to", as.Date(row.names(z.logret)[nrow(z.logret)]))
          ylab(i)+
          xlab(j)+
          annotate("text", x=mean(z.logret[,j], na.rm = T), y=Inf, label=lm_eqn(lm.fit), colour="black", size=3, parse=TRUE, vjust=1)+
          theme_bw()+
          theme(axis.line = element_line(), 
                axis.text=element_text(color='black'), 
                axis.title = element_text(colour = 'black'), 
                legend.text=element_text(), 
                legend.title=element_text(),
                text = element_text(size=10),
                legend.position = "none")
        plots_list <- c(plots_list, list(a)) 
      }
    }
    break
  }
  plots_list
}
ticker <- c("SPY", "QQQ", "IWM", "HYG", "MTUM", "SPHB","TLT", "IEF", "IEI", 
            "FXY","GLD", "XLU","USO", "DBC", "UUP", "IYR", "JJC") 
symbols <- getSymbols(ticker, from = "1990-01-01")

#Things to take into account:
#weekly, monthly and quarterly time frames, from what date, how many lags to predict and what % change.

symbols_1 <- symbols
#symbols_1 <- symbols[! symbols %in% c("HYG", "IEI", "FXY")]
#symbols_1 <- symbols[symbols %in% c("SPY", "TLT")]
DATE <- drawdowns_func_min(SPY)

etf_prices <- do.call(merge, lapply(symbols_1, function(x) to.weekly(get(x))))
colnames(etf_prices) <- colnames(do.call(merge, lapply(symbols_1, function(x) get(x))))
etf_prices <- Ad(etf_prices)
etf_prices <- do.call(merge, lapply(etf_prices, function(x) Delt(x, k = 4)))
names(etf_prices) <- symbols_1
plots_list <- reg_func(etf_prices[as.Date(index(etf_prices)) >= DATE,], 
                       se = FALSE, delt = TRUE, lag = -1)
grid.arrange(plots_list[[1]],plots_list[[2]],plots_list[[3]],plots_list[[4]], plots_list[[5]], plots_list[[6]], ncol = 2)
grid.arrange(plots_list[[7]],plots_list[[8]],plots_list[[9]],plots_list[[10]], plots_list[[11]], plots_list[[12]],ncol = 2)
grid.arrange(plots_list[[13]], plots_list[[14]], plots_list[[15]], plots_list[[16]], plots_list[[17]],ncol = 2)
grid.arrange(plots_list[[18]], plots_list[[19]],ncol = 2)


etf_prices <- do.call(merge, lapply(symbols_1, function(x) Ad(get(x))))
plots_list <- reg_func(etf_prices[index(etf_prices) >= DATE,], period = "weekly", 
                       se = FALSE, delt = FALSE, lag = -1)
grid.arrange(plots_list[[1]],plots_list[[2]],plots_list[[3]],plots_list[[4]], plots_list[[5]], plots_list[[6]], ncol = 2)
grid.arrange(plots_list[[7]],plots_list[[8]],plots_list[[9]],plots_list[[10]], plots_list[[11]], plots_list[[12]],ncol = 2)
grid.arrange(plots_list[[13]], plots_list[[14]], plots_list[[15]], plots_list[[16]], plots_list[[17]],ncol = 2)
