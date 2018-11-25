library(quantmod)   
library(lubridate)
library(ggplot2)
library(gridExtra)
Sys.setlocale("LC_TIME", "C")

Seasonality_3 <- function(data){
  
  etf_prices <- do.call(merge, lapply(data, function(x) Ad(get(x))))
  
  etf_returns <- do.call(merge, lapply(etf_prices, 
                                       function(x) periodReturn(x, period = 'monthly', type = 'arithmetic')))
  spy_s <- data.frame(etf_returns)
  spy_s$Date <- as.Date(row.names(spy_s))
  spy_s$Month <- month(spy_s$Date, label = TRUE, abbr = TRUE)
  names(spy_s) <- c("ret", "Date", "Month")
  
  alpha <- 0.3
  if(as.Date(row.names(spy_s))[1] <= "1990-01-01"){
    alpha <- 0.2
  }
  
  M <- ggplot(spy_s, aes(x=Month, y=ret)) + 
    ylab("") + 
    xlab("") + 
    #paste(strtrim(names(etf_prices)[1],3))
    ggtitle(paste("Returns by month for", gsub("\\..*$","", names(etf_prices)[1]),"since", as.Date(row.names(spy_s))[1])) + 
    stat_boxplot(geom ='errorbar')+ 
    geom_boxplot(outlier.shape = NA)+
    geom_hline(yintercept = 0, linetype = "dashed")+
    geom_jitter(colour="black", position = position_jitter(width = 0.2), alpha = alpha) +
    geom_point(data = spy_s[nrow(spy_s),], col = "red")+
    #geom_jitter(data = spy_s[nrow(spy_s),], 
    #            position = position_jitter(width = 0.2), alpha = 0.7, col = "red") +
    theme_bw()+
    #scale_colour_gradient2(low = "red", mid = "blue",high = "black", midpoint = mean(spy_s$ret))+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(), 
          legend.key = element_rect(colour = "black"),
          legend.position = "none")
  
  etf_returns <- do.call(merge, lapply(etf_prices, 
                                       function(x) periodReturn(x, period = 'quarterly', type = 'arithmetic')))
  spy_q <- data.frame(etf_returns)
  spy_q$Date <- as.Date(row.names(spy_q))
  spy_q$Q <- quarter(spy_q$Date)
  
  names(spy_q) <- c("ret", "Date", "Q")
  Q <- ggplot(spy_q, aes(x=factor(Q), y=ret)) + 
    ylab("") + 
    xlab("") + 
    theme_bw()+
    ggtitle(paste("Returns by quarter for", gsub("\\..*$","", names(etf_prices)[1]),"since", as.Date(row.names(spy_q))[1])) + 
    stat_boxplot(geom ='errorbar')+ 
    geom_boxplot(outlier.shape = NA)+
    geom_hline(yintercept = 0, linetype = "dashed")+
    geom_jitter(colour="black", position = position_jitter(width = 0.2), alpha = 0.3) +
    geom_point(data = spy_q[nrow(spy_q),], col = "red")+
    #scale_colour_gradient2(low = "red", mid = "blue",high = "black", midpoint = mean(spy_q$ret))+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(), 
          legend.key = element_rect(colour = "black"),
          legend.position = "none")
  
  grid.arrange(M, Q)
}

Seasonality_3_day_month <- function(x){
 
  t <- gsub("\\..*$","", names(x)[1])
  SPY_1 <- do.call(merge, lapply(x, function(x) periodReturn(x, period = 'daily', type = 'arithmetic')))
  
  SPY_1 <- data.frame(SPY_1)
  SPY_1$Date <- as.Date(row.names(SPY_1))
  names(SPY_1) <- c("SPY.Open", "SPY.High","SPY.Low","SPY","SPY.Volume",t,"Date" )
  SPY_1$SPY.Volume <- NULL
  SPY_1 <- na.omit(SPY_1)
  
  tmp <- aggregate(SPY_1$Date, list(month = substr(SPY_1$Date, 1, 7)), max)
  
  vect <- rep(NA, 1)
  for(k in 1:nrow(tmp)){
    vect <- append(vect, seq(1,nrow(SPY_1[substr(SPY_1$Date, 1, 7) %in% tmp$month[k],]),1), length(vect))
  }
  
  SPY_1$realDays <- na.omit(vect)
  
  MonthEndSeasonality <- aggregate(SPY_1[,t]~realDays, data = SPY_1, sum)
  a <- aggregate(SPY_1[,t]~realDays, data = SPY_1, mean)
  MonthEndSeasonality$mean <- a[,2]
  a <- aggregate(SPY_1[,t]~realDays, data = SPY_1, median)
  MonthEndSeasonality$med <- a[,2]
  
  Dis <- SPY_1
  tmp <- aggregate(Dis$Date, list(month = substr(Dis$Date, 1, 7)), max)
  vect <- rep(NA, 1)
  for(k in 1:nrow(tmp)){
    vect <- append(vect, seq(1,nrow(Dis[substr(Dis$Date, 1, 7) %in% tmp$month[k],]),1), length(vect))
  }
  
  Dis$realDays <- na.omit(vect)
  
  df <- merge(Dis[,c("Date", "realDays")], MonthEndSeasonality, by = "realDays")
  names(MonthEndSeasonality)[2] <- t
  
  ggplot(MonthEndSeasonality, aes(x=realDays, y=mean))+
    geom_col(col = "black")+
    geom_point(aes(y=med, x=realDays))+
    #geom_point(data = subset(df, df$realDays == df[df$Date >= (Sys.Date()-5),"realDays"][length(df[df$Date >= (Sys.Date()-5),"realDays"])]), aes(y=med, x=realDays), col = "red")+
    geom_point(data = subset(df, df$realDays == df[df$Date == (Sys.Date()-1),"realDays"]+1), aes(y=med, x=realDays), col = "red")+
    theme_bw()+
    #geom_point(aes(y=scale(cumsum(SPY), center = FALSE), x=realDays), shape = 2, col = "blue")+
    ggtitle(paste("Daily Mean and Median Returns for", t,"since", SPY_1$Date[1]))+ 
    ylab("")+
    xlab("")+
    scale_x_continuous(breaks = 1:nrow(MonthEndSeasonality))+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          text = element_text(size=10),
          legend.position = "none")
}



#strtrim(names(x)[grep(".Adjusted",names(x))],3)
Seasonality_3_day_quarter <- function(x){
  t <- gsub("\\..*$","", names(x)[1])
  
  SPY_1 <- do.call(merge, lapply(x, function(x) periodReturn(x, period = 'daily', type = 'arithmetic')))
  
  SPY_1 <- data.frame(SPY_1)
  SPY_1$Date <- as.Date(row.names(SPY_1))
  names(SPY_1) <- c("SPY.Open", "SPY.High","SPY.Low","SPY","SPY.Volume","SPY","Date" )
  
  SPY_1 <- na.omit(SPY_1)
  SPY_1$Date <- as.yearqtr(SPY_1$Date, format = "%Y-%m-%d")
  tmp <- aggregate(SPY_1$Date, list(month = substr(SPY_1$Date, 1, 7)), max)
  
  vect <- rep(NA, 1)
  for(k in 1:nrow(tmp)){
    vect <- append(vect, seq(1,nrow(SPY_1[substr(SPY_1$Date, 1, 7) %in% tmp$month[k],]),1), length(vect))
  }
  
  SPY_1$realDays_Q <- na.omit(vect)
  
  QEndSeasonality <- aggregate(SPY~realDays_Q, data = SPY_1, sum)
  a <- aggregate(SPY~realDays_Q, data = SPY_1, mean)
  QEndSeasonality$mean <- a$SPY
  a <- aggregate(SPY~realDays_Q, data = SPY_1, median)
  QEndSeasonality$med <- a$SPY
  temp <- SPY_1
  temp$Date_Q <- as.yearqtr(temp$Date, format = "%Y-%m-%d")
  tmp <- aggregate(temp$Date_Q , list(month = substr(temp$Date_Q , 1, 7)), max)
  
  vect <- c()
  for(k in 1:nrow(tmp)){
    vect <- append(vect, seq(1,nrow(temp[substr(temp$Date_Q , 1, 7) %in% tmp$month[k],]),1), length(vect))
  }
  
  temp$realDays_Q <- vect
  
  df <- merge(temp[,c("Date", "realDays_Q")], QEndSeasonality, by = "realDays_Q")
  
  ggplot(QEndSeasonality, aes(x=realDays_Q, y=mean))+
    geom_col(col = "black")+
    geom_point(aes(y=med, x=realDays_Q))+
    #geom_point(data = subset(df, df$realDays_Q == df[df$Date >= as.yearqtr(Sys.Date()-5),"realDays_Q"][length(df[df$Date >= as.yearqtr(Sys.Date()-5),"realDays_Q"])]), aes(y=med, x=realDays_Q), col = "red")+
    ggtitle(paste("Daily Mean and Median Returns for", t,"since", SPY_1$Date[1]))+ 
    ylab("")+
    xlab("")+
    scale_x_continuous(breaks = seq(1,nrow(QEndSeasonality), 3))+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          text = element_text(size=10),
          legend.position = "none")
}

getSymbols(c("SPY","^GSPC","TLT", "^VIX"), src = 'yahoo', from = '1950-01-01', auto.assign = T)
Seasonality_3('TLT')
Seasonality_3('SPY')
Seasonality_3('GSPC')
Seasonality_3("VIX")

a <- Seasonality_3_day_month(SPY)
b <- Seasonality_3_day_month(GSPC)
grid.arrange(a,b)
#red is the latest date return


#Seasonality_3_day_quarter(GSPC) #not edited for red dot on current date
#After a normal July bounce, stocks have now entered the seasonally weak period between August and September. 
#In fact, they're usually the two weakest months of the year.
#It looks like August is living up to its reputation as one of year's most dangerous months. And September still lies ahead.