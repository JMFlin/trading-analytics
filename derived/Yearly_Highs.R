library(Quandl)
library(quantmod)
library(gridExtra)
library(ggplot2)
library(lubridate)
library(reshape)
Sys.setlocale("LC_TIME", "C")
Quandl.api_key("Ykn-RwEHL2VBzptNdD6x")

year_high_close <- function(d, from = "1950"){
  
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
    tmp.df$Date <- as.Date(index(tmp))
    tmp.df.long <- melt(tmp.df,id.var="Date")
    tmp.df.long$asset <- rep(series.name,nrow(tmp.df.long))
    tmp.df.long
  }
  
  data <- d[,grep(".Adjusted", names(d))]
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
  
  df <- temp_2
  
  df$ind <- 0
  
  temp <- df[df$asset == "1950",]
  for(i in 1:nrow(temp)){
    if(temp$value[i] == 0){
      temp$ind[i] <- 1
    }
  }
  df_merged <- temp
  
  
  for(y in unique(df$asset)[2:length(unique(df$asset))]){
    temp <- df[df$asset == y,]
    for(i in 2:nrow(temp)){
      if(temp$value[i] == 0){
        temp$ind[i] <- 1
      }
    }
    df_merged <- rbind(df_merged, temp)
  }
  
  df_merged$ind <- as.numeric(df_merged$ind)
  
  df_agg <- aggregate(df_merged$ind, by = list(df_merged$asset), FUN = sum)
  df_agg$Group.1 <- as.Date(df_agg$Group.1, format = "%Y")
  ggplot(df_agg, aes(x=Group.1, y=x))+
    geom_col(col = "black")+
    ylab("") +
    xlab("")+
    ggtitle(paste("Yearly high close for", names(d)[length(names(d))]))+
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    geom_text(
      data=df_agg[df_agg$x > 0 & df_agg$x > df_agg$x[nrow(df_agg)],],
      aes(label=x,
          x=Group.1, y=x), vjust = -0.5,size = 3.0)+
    geom_text(
      data=df_agg[nrow(df_agg),],
      aes(label=x,
          x=Group.1, y=x), vjust = -0.5,size = 3.0)+
    theme_bw()+
    theme(axis.line = element_line(),
          axis.text=element_text(color='black'),
          axis.title = element_text(colour = 'black'),
          legend.text=element_text(),
          legend.title=element_text(),
          legend.position='none')
}

year_all_time_high_close <- function(d, from = "1950"){
  
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
  
  data <- periodReturn(d[,grep(".Adjusted", names(d))], period='daily')
  data <- data[year(index(data)) >= from,]
  
  df <- cps.df(data, geometric = TRUE)
  
  df$ind <- 0
  
  for(i in 1:nrow(df)){
    if(df$value[i] == 0){
      df$ind[i] <- 1
    }
  }
  df$Date <- year(df$Date)
  df_agg <- aggregate(df$ind, by = list(df$Date), FUN = sum)
  
  df_agg$Group.1 <- as.character(df_agg$Group.1)
  df_agg$Group.1 <- as.Date(df_agg$Group.1, format = "%Y")
  
  ggplot(df_agg, aes(x=Group.1, y=x))+
    geom_col(col = "black")+
    ylab("") +
    xlab("")+
    ggtitle(paste("All-time high close by year for", names(d)[grep(".Adjusted", names(d))]))+
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    geom_text(
      data=df_agg[df_agg$x > 0 & df_agg$x > df_agg$x[nrow(df_agg)],],
      aes(label=x,
          x=Group.1, y=x), vjust = -0.5,size = 3.0)+
    geom_text(
      data=df_agg[nrow(df_agg),],
      aes(label=x,
          x=Group.1, y=x), vjust = -0.5,size = 3.0)+
    theme_bw()+
    theme(axis.line = element_line(),
          axis.text=element_text(color='black'),
          axis.title = element_text(colour = 'black'),
          legend.text=element_text(),
          legend.title=element_text(),
          legend.position='none')
}


getSymbols("^GSPC", from = "1950-01-01")
year_high_close(GSPC) 
year_all_time_high_close(GSPC)

SPCOMP <- Quandl("YALE/SPCOMP", type = "xts", order = "asc")
names(SPCOMP)[grep("Real Price", names(SPCOMP))] <- "Real.Adjusted"
names(SPCOMP)[grep("Cyclically Adjusted", names(SPCOMP))] <- "Real Price"
year_high_close(SPCOMP) #monthly data
year_all_time_high_close(SPCOMP) #monthly data
