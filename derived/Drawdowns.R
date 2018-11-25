library(quantmod)
library(ggplot2)
library(scales)
library(reshape)
library(PerformanceAnalytics)
library(lubridate)
library(gridExtra)
Sys.setlocale("LC_TIME", "C")

drawdowns_func <- function(data, dd = 0.05, since = "1990-01-01", date_breaks = "2 years"){
  
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
    tmp.df$Date <- as.POSIXct(index(tmp))
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
  
  vect_min <- c()
  p <- 0
  vect <- c()
  for(i in 1:nrow(df)){
    if(df$ind[i] == 1){
      p <- p+1
      vect <- append(vect,p,length(vect))
    }else{
      
      vect_min <- append(vect_min,0,length(vect_min))
      
      if(sum(vect) != 0){
        vect_min <- append(vect_min,rep(0,(length(vect)-2)),length(vect_min))
        vect_min <- append(vect_min,max(vect),length(vect_min))
      }
      vect <- 0
      p <- 0
    }
  }
  
  vect_min <- append(vect_min,
                     c(rep(0,(length(0:length(vect_min[(length(vect_min)-(nrow(df)-length(vect_min))):(length(vect_min)-1)])
                     )-2)), 0)
                     ,length(vect_min))
  df$test <- vect_min[1:(length(vect_min))]
  
  df$test <- c(df$test[2:nrow(df)], df$ind2[nrow(df)])
  
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

  df$Date <- as.Date(df$Date)
  g <- paste("Mean:", round(mean(vect_ret[vect_ret <= -dd]),3)*100, "%", "\n",
             "Median:",round(median(vect_ret[vect_ret <= -dd]),3)*100,"%")
  
  a <- ggplot(df[df$Date >= since,], aes(x = Date, y = value, group = variable)) +
    geom_line() +
    geom_point(data = df[df$value %in% vect_ret[vect_ret <= -dd] & df$Date >= since,], col ="red", alpha = 0.5) +
    geom_point(data = df[nrow(df),], col ="blue", alpha = 0.5) +
    geom_hline(yintercept = 0, colour = "black") +
    geom_hline(yintercept = -0.2, colour = "black", linetype = "longdash", alpha = 0.5) +
    geom_hline(yintercept = median(vect_ret[vect_ret <= -dd]), col = "blue", linetype = "longdash", alpha = 0.2)+
    geom_hline(yintercept = mean(vect_ret[vect_ret <= -dd]), col = "red", linetype = "longdash", alpha = 0.2)+
    ggtitle(paste("Drawdowns"), paste("Latest drawdown:", round(df$value[nrow(df)],3)*100, "%")) +
    theme(axis.text.x = element_text(angle = 0)) +
    scale_x_date(date_breaks = date_breaks, date_labels = "%Y") +
    #annotate("text", x=as.POSIXct("2016-12-01"), y=-0.4, label=g, colour="black", size=3)+
    ylab("") +
    xlab("")+
    theme_bw()+
    theme(axis.line = element_line(),
        axis.text=element_text(color='black'),
        axis.title = element_text(colour = 'black'),
        legend.text=element_text(),
        legend.title=element_text(),
        legend.position='none')
  
  g <- paste("Mean:", round(mean(df[df$test %in% vect_min[vect_min > 20],"test"]),0), "days", "\n",
             "Median:",round(median(df[df$test %in% vect_min[vect_min > 20], "test"]),0),"days")
  
  b <- ggplot(df[df$Date >= since,], aes(x = Date, y = ind2, group = variable)) +
    geom_line() +
    geom_point(data = df[df$test %in% vect_min[vect_min > 20] & df$Date >= since,], col ="red", alpha = 0.5) +
    geom_point(data = df[nrow(df),], col ="blue", alpha = 0.5) +
    geom_hline(yintercept = 0, colour = "black") +
    geom_hline(yintercept = median(df[df$test %in% vect_min[vect_min > 20], "test"]), col = "blue", linetype = "longdash", alpha = 0.2)+
    geom_hline(yintercept = mean(df[df$test %in% vect_min[vect_min > 20],"test"]), col = "red", linetype = "longdash", alpha = 0.2)+
    ggtitle(paste("Days to new all time high"), paste("Days under all-time high:", df$ind2[nrow(df)])) +
    theme(axis.text.x = element_text(angle = 0)) +
    scale_x_date(date_breaks = date_breaks, date_labels = "%Y") +
    #annotate("text", x=as.POSIXct("2016-08-01"), y=1500, label=g, colour="black", size=3)+
    ylab("") +
    xlab("")+
    theme_bw()+
    theme(axis.line = element_line(),
          axis.text=element_text(color='black'),
          axis.title = element_text(colour = 'black'),
          legend.text=element_text(),
          legend.title=element_text(),
          legend.position='none')
  grid.arrange(a,b)
}

getSymbols("^GSPC", from = "1950-01-01")
drawdowns_func(GSPC, dd = 0.03, since = "2013-01-01", date_breaks = "1 year")
#This chart shows historical bear markets (a 20% market decline from the previous all time high) - J.P. Morgan AM
