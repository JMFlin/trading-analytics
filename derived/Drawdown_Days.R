library(quantmod)
library(ggplot2)
library(scales)
library(reshape)
library(PerformanceAnalytics)
library(lubridate)
library(gridExtra)
Sys.setlocale("LC_TIME", "C")

dd_correction_func_one <- function(data, dd = 0.05, since = "1950-01-01", data_breaks = "5 years"){
  
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
  #data <- GSPC
  data <- data[index(data) >= since,]
  name <- names(data[,grep(".Adjusted", names(data))])
  data <- periodReturn(data[,grep(".Adjusted", names(data))], period='daily')
  
  df <- cps.df(data, geometric = TRUE)
  df$day_counts <- 0
  df_1 <- c()
  for(i in 1:nrow(df)){
    if(df$value[i]*-1 <=dd){
      df_1 <- append(df_1,df$value[i],length(df_1))
    }else{
      break
    }
  }
  
  df$day_counts[length(df_1)+1] <- length(df_1)+1
  count <- length(df_1)+1
  j <- 1
  
  while(j <= 55){
  
    data[sum(count)+1] <- 0
    dfdf <- cps.df(data[(sum(count)+1):length(data),], geometric = TRUE)
  
    df_1 <- c()
    i <- 1
    for(i in 1:nrow(dfdf)){
      if(dfdf$value[i]*-1 <=dd){
        df_1 <- append(df_1,dfdf$value[i],length(df_1))
      }else{
        break
      }
    }
    
    count <- append(count,length(df_1)+1,length(count))
    
    if(length(df$day_counts) >= sum(count)){
      df$day_counts[sum(count)] <- count[length(count)]
    }else{
      break
    }
    j+1
  }

  df$day_counts_ind <- df$day_counts
  j <- 0
  for(i in 1:nrow(df)){
    j <- j+1
    if(df$day_counts_ind[i] == 0){
      df$day_counts_ind[i] <- j
    }else{
      j <- 0
    }
  }
  df$Date <- as.Date(df$Date)
  
  if(dd >=0.1){
    ylim <- c(0, 2000)
  }
  if(dd >= 0.05 & dd < 0.1){
    ylim <- c(0, 450)
  }
  if(dd >= 0.03& dd < 0.05){
    ylim <- c(0, 340)
  }
  
  ggplot(df, aes(x=Date, y=day_counts_ind))+
    geom_line()+
    geom_point(data = df[df$day_counts > 0 & df$day_counts > df$day_counts_ind[nrow(df)],], col ="red", alpha = 0.5)+
    geom_point(data = df[nrow(df),], col ="blue", alpha = 0.5)+
    geom_text(
      data=df[df$day_counts > 0 & df$day_counts > df$day_counts_ind[nrow(df)],],
        aes(label=day_counts,
              x=Date, y=day_counts), vjust = -0.5,size = 3.0)+
    geom_text(
      data=df[nrow(df),],
      aes(label=day_counts_ind,
          x=Date, y=day_counts_ind), vjust = -0.5,size = 3.0)+
    #geom_text(aes(label=round(vect,3)*100),hjust=0.5, vjust=1.6, size = 2.5)+
    theme(axis.text.x = element_text(angle = 0)) +
    scale_x_date(date_breaks = data_breaks, date_labels = "%Y") +
    ggtitle(paste("Trading days since", paste0(dd*100,"%"), name,"correction from", year(since)))+
    ylab("") +
    xlab("")+
    coord_cartesian(ylim = ylim)+
    theme_bw()+
    theme(axis.line = element_line(),
          axis.text=element_text(color='black'),
          axis.title = element_text(colour = 'black'),
          legend.text=element_text(),
          legend.title=element_text(),
          legend.position='none',
          text = element_text(size=10))
}


dd_correction_func_two <- function(data, dd = 0.05, since = "1950-01-01", data_breaks = "5 years"){
  
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
  #data <- GSPC
  data <- data[index(data) >= since,]
  name <- names(data[,grep(".Adjusted", names(data))])
  data <- periodReturn(data[,grep(".Adjusted", names(data))], period='daily')
  
  df <- cps.df(data, geometric = TRUE)
  df$day_counts <- 0
  df_1 <- c()
  
  df$tmp_ind <- 0
  
  df$tmp_ind <- ifelse(df$value*-1 >= dd, 1,df$tmp_ind)
  
  #df$tmp_ind2 <- 0
  
  #for(i in 1:nrow(df)){
  #  if(df$tmp_ind[i] == 1){
  #    if(df$tmp_ind[i+1] == 0){
  #      df$tmp_ind[i] <- 2
  #    }
  #  }
  #}
  
  
  for(i in 1:nrow(df)){
    if(df$value[i]*-1 <=dd){
      df_1 <- append(df_1,df$value[i],length(df_1))
    }else{
      break
    }
  }
  
  df$day_counts[length(df_1)+1] <- length(df_1)+1
  
  p <- 0
  for(i in 1:nrow(df)){
    if(df$tmp_ind[i] == 0){
      df$tmp_ind2[i] <- p+1
      p <- p+1
    }
    if(df$tmp_ind[i] == 1 | df$tmp_ind[i] == 2){
      df$tmp_ind2[i] <- 0
      p <- 0
    }
  }
  
  for(i in 2:nrow(df)){
    if(df$tmp_ind[i] == 1){
      df$labels[i-1] <- df$tmp_ind2[i-1]
    }else{
      df$labels[i-1] <- 0
    }
  }
  
  df$labels[nrow(df)] <- df$tmp_ind2[nrow(df)]
  
  df$Date <- as.Date(df$Date)
  
  if(dd >=0.1){
    ylim <- c(0, 1800)
  }
  if(dd >= 0.05 & dd < 0.1){
    ylim <- c(0, 450)
  }
  if(dd >= 0.03& dd < 0.05){
    ylim <- c(0, 300)
  }
  
  ggplot(df, aes(x=Date, y=tmp_ind2))+
    geom_line()+
    geom_point(data = df[df$labels > 0 & df$labels > df$labels[nrow(df)],], col ="red", alpha = 0.5)+
    geom_point(data = df[nrow(df),], col ="blue", alpha = 0.5)+
    geom_text(data = df[df$labels > 0 & df$labels >= df$labels[nrow(df)],],
              aes(label=labels, x=Date, y=tmp_ind2), vjust = -0.5,size = 3.0)+
    geom_text(data = df[nrow(df),],
              aes(label=labels, x=Date, y=tmp_ind2), vjust = -0.5,size = 3.0)+
    
    #geom_text(
    #  data=df[nrow(df),],
    #  aes(label=day_counts_ind,
    #      x=Date, y=day_counts_ind), vjust = -0.5,size = 3.0)+
    #geom_text(aes(label=round(vect,3)*100),hjust=0.5, vjust=1.6, size = 2.5)+
    theme(axis.text.x = element_text(angle = 0)) +
    scale_x_date(date_breaks = data_breaks, date_labels = "%Y") +
    #ggtitle(paste("Trading days since", paste0(dd*100,"%"), name,"correction from", year(since)))+
    ylab("") +
    xlab("")+
    coord_cartesian(ylim = ylim)+
    theme_bw()+
    theme(axis.line = element_line(),
          axis.text=element_text(color='black'),
          axis.title = element_text(colour = 'black'),
          legend.text=element_text(),
          legend.title=element_text(),
          legend.position='none',
          text = element_text(size=10))
}


getSymbols("^GSPC", from = "1950-01-01")
a <- dd_correction_func_one(GSPC, dd = 0.1, since = "1950-01-01", data_breaks = "5 years")
c <- dd_correction_func_one(GSPC, dd = 0.05, since = "1980-01-01", data_breaks = "3 years")
e <- dd_correction_func_one(GSPC, dd = 0.03, since = "1980-01-01", data_breaks = "3 years")

b <- dd_correction_func_two(GSPC, dd = 0.1, since = "1950-01-01", data_breaks = "5 years")
d <- dd_correction_func_two(GSPC, dd = 0.05, since = "1980-01-01", data_breaks = "3 years")
f <- dd_correction_func_two(GSPC, dd = 0.03, since = "1980-01-01", data_breaks = "3 years")

grid.arrange(a,b)
grid.arrange(c,d)
grid.arrange(e,f)
