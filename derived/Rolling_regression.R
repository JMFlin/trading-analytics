library(quantmod)
library(dplyr)
library(ggplot2)
library(gridExtra)
getSymbols("^GSPC", from="2013-01-01")
getSymbols("^VIX", from="2013-01-01")

# Rolling regression (unweighted), with prediction intervals

RollingRegression <- function(GSPC){
  
  CreateRollingRegression <- function(GSPC){
    
    rolling.regression <- rollapplyr( 
      as.xts(Ad(GSPC)), 
      width=300, by.column = FALSE, 
      FUN = function(x) {
        r <- lm( x ~ index(x) )
        tail(predict(r, interval="prediction"),1)
      } 
    )
    
    return(rolling.regression)
  }
  
  returns <- diff(GSPC[,ncol(GSPC)], log=TRUE, na.pad=FALSE) 
  
  returns.reg <- CreateRollingRegression(rollmean(returns, k=100))
  returns.reg <- data.frame(returns.reg, Date = index(returns.reg))
  
  mean.returns <- data.frame(rollmean(returns, k=100))
  mean.returns$Date <- as.Date(row.names(mean.returns))
  
  mean.returns <- inner_join(mean.returns, returns.reg, by = "Date")
  names(mean.returns)[(ncol(mean.returns)-2):ncol(mean.returns)] <- c("fit", "lwd", "hwd")
  
  rolling.regression <- CreateRollingRegression(GSPC)
  
  GSPC <- data.frame(GSPC)
  names(GSPC[,ncol(GSPC)]) <- "Adj"
  GSPC$Date <- as.Date(row.names(GSPC))
  
  rolling.regression <- data.frame(rolling.regression, Date = index(rolling.regression))
  
  GSPC <- inner_join(GSPC, rolling.regression, by = "Date")
  
  names(GSPC)[(ncol(GSPC)-2):ncol(GSPC)] <- c("fit", "lwd", "hwd")
  names(GSPC)[6] <- "Adj"
  price.plot <- ggplot(GSPC, aes(x=Date, y=Adj)) +
    geom_line()+
    geom_line(aes(x = Date, y = fit)) +
    geom_line(aes(x = Date, y = lwd)) +
    geom_line(aes(x = Date, y = hwd)) +
    theme_light()
  names(mean.returns)[1] <- "Adj"
  return.plot <- ggplot(mean.returns, aes(x = Date, y=Adj)) +
    geom_line()+
    geom_hline(yintercept = 0)+
    geom_line(aes(x = Date, y = fit)) +
    geom_line(aes(x = Date, y = lwd)) +
    geom_line(aes(x = Date, y = hwd)) +
    theme_bw()
  
  grid.arrange(price.plot, return.plot, ncol = 1)
  
}

RollingRegression(GSPC)

RollingRegression(VIX)


