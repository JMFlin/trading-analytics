library(quantmod)
library(ggplot2)
library(scales)
library(reshape)
library(PerformanceAnalytics)
library(lubridate)
library(gridExtra)
Sys.setlocale("LC_TIME", "C")

drawdowns_func_year <- function(data, from = "1950"){
  
  clean.rtn.xts <- function(univ.rtn.xts.obj,na.replace=0){
    univ.rtn.xts.obj[is.na(univ.rtn.xts.obj)] <- na.replace
    univ.rtn.xts.obj 
  }

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
  
  data <- data[year(index(data)) >= from,]
  data_years <- unique(year(index(data)))
  temp <- periodReturn(data[year(index(data)) == data_years[1],grep(".Adjusted", names(data))], period='daily')
  for(i in data_years[2:length(data_years)]){
    temp <- merge(temp,periodReturn(data[year(index(data)) == i,grep(".Adjusted", names(data))], period='daily'))
  }
  names(temp) <- data_years
  
  temp_2 <- cps.df(na.omit(temp[,names(temp)[1]]), geometric = TRUE)
  for(i in names(temp)[2:length(names(temp))]){
    temp_2 <- rbind(temp_2, cps.df(na.omit(temp[,i]), geometric = TRUE))
  }
  #temp_2$Date <- as.Date(temp_2$Date)
  df <- temp_2
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
  
  if(year(df$Date[1]) == 1950){
    indi <- "5 years"
    ind <- "%Y"
  }else if(year(df$Date[1]) == 2010){
    indi <- "1 years"
    ind <- "%Y"
  }else{
    indi <- "3 years"
    ind <- "%Y"
  }
  subset_d <- df[year(df$Date) == year(Sys.Date()),]
  df$Date <- as.Date(df$Date)
  g <- paste( 
    "Mean:",paste0(round(mean(vect_ret[vect_ret <= -0.05])*100,2),"%"),"\n",
    "Median:", paste0(round(median(vect_ret[vect_ret <= -0.05])*100,2),"%"))
  a <- ggplot(df, aes(x = Date, y = value, group = variable)) +
    geom_line() +
    geom_point(data = df[df$value %in% vect_ret[vect_ret <= -0.05],], col ="red", alpha = 0.5) +
    geom_point(data = df[nrow(df),], col ="blue", alpha = 0.5) +
    geom_hline(yintercept = 0, colour = "black") +
    geom_hline(yintercept = -0.2, colour = "black", linetype = "longdash", alpha = 0.5 ) +
    geom_hline(yintercept = median(vect_ret[vect_ret <= -0.05]), col = "blue", linetype = "longdash", alpha = 0.2)+
    geom_hline(yintercept = mean(vect_ret[vect_ret <= -0.05]), col = "red", linetype = "longdash", alpha = 0.2)+
    ggtitle(paste("Drawdowns since", year(df$Date)), paste(year(Sys.Date()),"maximum market decline:", 
                                      paste0(round(min(subset_d$value)*100,1),"%"))) +
    #annotate("text", x=as.POSIXct("2016", format = "%Y"), y=-0.15, label=g, colour="black", size=3)+
    scale_x_date(date_breaks = indi, date_labels = ind) +
    ylab("") +
    xlab("")+
    theme_bw()+
    theme(axis.line = element_line(),
          axis.text=element_text(color='black'),
          axis.title = element_text(colour = 'black'),
          legend.text=element_text(),
          legend.title=element_text(),
          legend.position='none')
  
  g <- paste( 
    "Mean:",paste(round(mean(df[df$test %in% vect_min[vect_min > 5],"test"]),0),"days"),"\n",
    "Median:", paste(round(median(df[df$test %in% vect_min[vect_min > 5], "test"]),0),"days"))
  b <- ggplot(df, aes(x = Date, y = ind2, group = variable)) +
    geom_line() +
    geom_point(data = df[df$test %in% vect_min[vect_min > 5],], col ="red", alpha = 0.5) +
    geom_point(data = df[nrow(df),], col ="blue", alpha = 0.5) +
    geom_hline(yintercept = 0, colour = "black") +
    geom_hline(yintercept = median(df[df$test %in% vect_min[vect_min > 5], "test"]), col = "blue", linetype = "longdash", alpha = 0.2)+
    geom_hline(yintercept = mean(df[df$test %in% vect_min[vect_min > 5],"test"]), col = "red", linetype = "longdash", alpha = 0.2)+
    ggtitle(paste("Days to new all time high"), paste(year(Sys.Date()), "maximum market decline:", max(subset_d$ind2), "days")) +
    #annotate("text", x=as.POSIXct("2016-08-01"), y=150, label=g, colour="black", size=3)+
    scale_x_date(date_breaks = indi, date_labels = ind) +
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


drawdowns_and_ret <- function(data, from = "1950"){
  
  clean.rtn.xts <- function(univ.rtn.xts.obj,na.replace=0){
    univ.rtn.xts.obj[is.na(univ.rtn.xts.obj)] <- na.replace
    univ.rtn.xts.obj 
  }

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
  
  data <- data[year(index(data)) >= from,]
  data_years <- unique(year(index(data)))
  temp <- periodReturn(data[year(index(data)) == data_years[1],grep(".Adjusted", names(data))], period='daily')
  for(i in data_years[2:length(data_years)]){
    temp <- merge(temp,periodReturn(data[year(index(data)) == i,grep(".Adjusted", names(data))], period='daily'))
  }
  names(temp) <- data_years
  
  temp_2 <- cps.df(na.omit(temp[,names(temp)[1]]), geometric = TRUE)
  for(i in names(temp)[2:length(names(temp))]){
    temp_2 <- rbind(temp_2, cps.df(na.omit(temp[,i]), geometric = TRUE))
  }
  #temp_2$Date <- as.Date(temp_2$Date)
  df <- temp_2
 
  
  if(year(df$Date[1]) == 1950){
    indi <- "5 years"
    ind <- "%Y"
  }else if(year(df$Date[1]) == 2010){
    indi <- "1 years"
    ind <- "%Y"
  }else{
    indi <- "3 years"
    ind <- "%Y"
  }
  
  vect <- c()
  for(i in unique(df$asset)){
    tmp <- df[df$asset == i,]
    vect <- append(vect, min(tmp$value),length(vect))
  }
  
  data <- data[year(index(data)) >= from,]
  data_years <- unique(year(index(data)))
  temp_ann <- periodReturn(data[year(index(data)) == data_years[1],grep(".Adjusted", names(data))], period='yearly')
  
  for(i in data_years[2:length(data_years)]){
    temp_ann <- rbind(temp_ann,periodReturn(data[year(index(data)) == i,grep(".Adjusted", names(data))], period='yearly'))
  }
  
  temp <- data.frame(as.POSIXct(unique(df$asset), format = "%Y"),vect, temp_ann)
  names(temp) <- c("Date", "vect", "yearly.returns")
  temp$Date <- as.Date(temp$Date)
  ggplot(temp[year(temp$Date) >= from,], aes(x = Date, y=vect))+
    geom_point(col = "red")+
    geom_text(aes(label=round(vect,3)*100),hjust=0.5, vjust=1.6, size = 2.5)+
    geom_col(aes(x = Date, y=yearly.returns), col = "black")+
    geom_text(aes(label=round(yearly.returns,3)*100, y=ifelse(yearly.returns>0, yearly.returns+0.01, yearly.returns-0.03)),
              vjust=0, size = 2.5, position=position_dodge(0.9))+
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    ylab("") +
    xlab("")+
    ggtitle("Annual returns and intra-year declines")+
    theme_bw()+
    theme(axis.line = element_line(),
          axis.text=element_text(color='black'),
          axis.title = element_text(colour = 'black'),
          legend.text=element_text(),
          legend.title=element_text(),
          legend.position='none')
}

getSymbols("^GSPC", from = "1950-01-01")
drawdowns_func_year(GSPC, from = "2010")
drawdowns_and_ret(GSPC, from = "1990")
