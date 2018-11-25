library(quantmod)   
library(lubridate)
library(ggplot2)
library(gridExtra)
library(MASS)
library(robustbase)
library(quantreg)
Sys.setlocale("LC_TIME", "C")

annual_ret <- function(data, se = TRUE){
  df <- data.frame(periodReturn(data, period = 'quarterly', type = 'arithmetic'))
  df$Date <- as.Date(row.names(df))
  df$Quarter <- quarter(df$Date)
  
  vect <- c()
  for(i in 1:(nrow(df)-1)){
    if(df$Quarter[i] == 1 & df$Quarter[i+1] == 2){
      vect <- append(vect, df$quarterly.returns[i] + df$quarterly.returns[i+1], length(vect))
      vect <- append(vect, df$quarterly.returns[i] + df$quarterly.returns[i+1], length(vect))
    }
    if(df$Quarter[i] == 3 & df$Quarter[i+1] == 4){
      vect <- append(vect, df$quarterly.returns[i] + df$quarterly.returns[i+1], length(vect))
      vect <- append(vect, df$quarterly.returns[i] + df$quarterly.returns[i+1], length(vect))
    }
  }
  
  if(length(vect) != nrow(df)){
    vect <- append(vect, df$quarterly.returns[nrow(df)], length(vect))
  }
  
  df$t <- vect
  tmp <- df[seq(1,nrow(df),2),]
  tmp$Quarter <- factor(ifelse(tmp$Quarter == 1, "Q1+Q2", "Q3+Q4"))
  
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
  
  c <- ggplot(tmp, aes(x=year(Date),y=t, group = Quarter, fill = Quarter))+
    geom_col(width=0.7, position = "stack", col = "black")+
    ggtitle("")+ 
    ylab("")+
    xlab("")+
    scale_x_continuous(breaks = seq(year(tmp$Date)[1],year(tmp$Date)[length(year(tmp$Date))],5))+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          text = element_text(size=10),
          legend.position = "top")+
    labs(fill="")
  
  names(tmp)[length(names(tmp))] <- "quarterly.returns"
  if(length(tmp[tmp$Quarter=="Q1+Q2","quarterly.returns"]) != length(tmp[tmp$Quarter=="Q3+Q4","quarterly.returns"])){
    tmp <- tmp[1:(nrow(tmp)-1),]
  }
  
  a <- data.frame(tmp[tmp$Quarter=="Q1+Q2","quarterly.returns"], tmp[tmp$Quarter=="Q3+Q4","quarterly.returns"])
  names(a) <- c("Q1Q2", "Q3Q4")
  k <- a[,1]
  p <- a[,2]
  lm.fit <- lm(k ~ p) 
  res1b <- rq(k ~ p)
  resid1 <- round(res1b$residuals, 7)
  resid2 <- resid1[resid1 != 0]
  sigma <- (1/0.675)*median(abs(resid2))
  res1c <- lmrob(k ~ p, method = "MM",
                 init = list(coefficients = res1b$coef, scale = sigma),
                 setting = "KS2014")
  
  b <- ggplot(a, aes(x=Q1Q2,y=Q3Q4)) +
    geom_point() +
    geom_smooth(method = "lm", colour = "black", size = 0.3, formula = y ~ x, se = se) +
    geom_smooth(method = function(formula,data,weights=weight) lmrob(formula,
                                                                     data,
                                                                     weights=weight,
                                                                     method="MM",
                                                                     init = list(coefficients = res1b$coef, scale = sigma),
                                                                     setting = "KS2014"),colour = "red", size = 0.3, formula = y ~ x, se = se) +
    geom_point(data=subset(a, row.names(a)[nrow(a)] == row.names(a)), colour="red")+
    geom_hline(yintercept = 0, col = "black", alpha = 0.5, linetype = "longdash")+
    geom_vline(xintercept = 0, col = "black", alpha = 0.5, linetype = "longdash")+
    ggtitle("")+ 
    ylab(paste("Q3+Q4"))+
    xlab(paste("Q1+Q2"))+
    annotate("text", x=mean(a$Q1Q2, na.rm = T), y=Inf, label=lm_eqn(lm.fit), colour="black", size=3, parse=TRUE, vjust=1)+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          text = element_text(size=10),
          legend.position = "none")
  
  plots_list <- list()
  plots_list <- c(plots_list, list(c))
  plots_list <- c(plots_list, list(b))
  plots_list
}

yearly_ret <- function(data, se = TRUE, lag = -1){
  
  etf_prices <- Ad(data)
  
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
  etf_returns <- periodReturn(etf_prices, period = "yearly", type = 'arithmetic')
  #etf_ticker_sector <- data.frame(ticker)
  #colnames(etf_returns) <- etf_ticker_sector$ticker
  z.logret <- data.frame(etf_returns)
  
  z.logret$SPY_lead <- mlag(z.logret[,1], nlag = lag)
  z.logret <- na.omit(z.logret)
  tmp <- 1:length(z.logret)
  z.logret <- z.logret[,c(tmp[length(tmp)], 1:(length(tmp)-1))]
  names(z.logret) <- c("lead", "lag")
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
  
  k <- z.logret[,1]
  p <- z.logret[,2]
  lm.fit <- lm(k ~ p) 
  res1b <- rq(k ~ p)
  resid1 <- round(res1b$residuals, 7)
  resid2 <- resid1[resid1 != 0]
  sigma <- (1/0.675)*median(abs(resid2))
  res1c <- lmrob(k ~ p, method = "MM",
                 init = list(coefficients = res1b$coef, scale = sigma),
                 setting = "KS2014")
  
  ggplot(z.logret, aes(x=lag,y=lead)) +
    geom_point() +
    geom_smooth(method = "lm", colour = "black", size = 0.3, formula = y ~ x, se = se) +
    geom_smooth(method = function(formula,data,weights=weight) lmrob(formula,
                                                                     data,
                                                                     weights=weight,
                                                                     method="MM",
                                                                     init = list(coefficients = res1b$coef, scale = sigma),
                                                                     setting = "KS2014"),colour = "red", size = 0.3, formula = y ~ x, se = se) +
    geom_point(data=subset(z.logret, row.names(z.logret)[nrow(z.logret)] == row.names(z.logret)), colour="red")+
    geom_hline(yintercept = 0, col = "black", alpha = 0.5, linetype = "longdash")+
    geom_vline(xintercept = 0, col = "black", alpha = 0.5, linetype = "longdash")+
    ggtitle("")+ 
    ylab(paste0(gsub("\\..*$","", names(data)[1]), "_lead"))+
    xlab(gsub("\\..*$","", names(data)[1]))+
    annotate("text", x=mean(z.logret[,2], na.rm = T), y=Inf, label=lm_eqn(lm.fit), colour="black", size=3, parse=TRUE, vjust=1)+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          text = element_text(size=10),
          legend.position = "none")
}


getSymbols("^GSPC", from = "1900-01-01")
annual_ret(GSPC, se = FALSE)
yearly_ret(GSPC, se = FALSE, lag = -2)

getSymbols("QQQ", from = "1900-01-01")
annual_ret(QQQ, se = FALSE)
yearly_ret(QQQ, se = FALSE, lag = -2)

getSymbols("IWM", from = "1900-01-01")
annual_ret(IWM, se = FALSE)
yearly_ret(IWM, se = FALSE, lag = -2)
#Up and down, that's how you trade.
#You buy after a down year, sell after a gain.

#Investors should err on the side of optimism.
#They should buy stocks at every reasonable oopportunity to gain wealth.
