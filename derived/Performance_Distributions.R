library(quantmod)   
library(lubridate)
library(ggplot2)
library(reshape2)
Sys.setlocale("LC_TIME", "C")

distributions <- function(data){
  drawdowns_func_min <- function(my_data){
    
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
    
    data <- periodReturn(my_data[,grep(".Adjusted", names(my_data))], period='daily')
    
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
  
  DATE <- drawdowns_func_min(data)
  
  df <- data[index(data) >= DATE,]
  data <- df
  data_years <- unique(year(index(data)))
  temp <- periodReturn(data[year(index(data)) == data_years[1],grep(".Adjusted", names(data))], period='daily')
  for(i in data_years[2:length(data_years)]){
    temp <- rbind(temp,periodReturn(data[year(index(data)) == i,grep(".Adjusted", names(data))], period='daily'))
  }
  
  temp_2 <- data.frame(temp)
  temp_2$Date <- as.Date(row.names(temp_2))
  temp_2$asset <- year(temp_2$Date)
  names(temp_2)[1] <- "value"
  names(temp_2)
  vline.dat.mean <- aggregate(temp_2$value, list(temp_2$asset), mean)
  vline.dat.median <- aggregate(temp_2$value, list(temp_2$asset), median)
  names(vline.dat.mean)[1] <- "asset"
  names(vline.dat.median)[1] <- "asset"
  
  temp_sd <- data.frame(temp)
  temp_sd$Date <- as.Date(row.names(temp_sd))
  temp_sd$asset <- year(temp_sd$Date)
  names(temp_sd)[1] <- "value"
  vline.dat.sd <- data.frame(sd(temp_sd$value))
  names(vline.dat.sd)[1] <- "x"
  
  a <- ggplot(temp_2, aes(x=value, fill = as.factor(asset)))+
    geom_histogram(bins = 30)+
    geom_density(alpha = 0.5)+
    facet_wrap(~ asset, ncol = 3, scales = "fixed")+
    ylab("")+
    xlab("")+
    ggtitle("Return distributions for all years in bull market")+
    geom_vline(aes(xintercept = x), data = vline.dat.mean, linetype = "longdash", color = "blue")+
    geom_vline(aes(xintercept = x), data = vline.dat.median, linetype = "longdash", color = "red")+
    geom_vline(aes(xintercept = 1*x), data = vline.dat.sd, linetype = "longdash", color = "black", alpha = 0.3)+
    geom_vline(aes(xintercept = -1*x), data = vline.dat.sd, linetype = "longdash", color = "black", alpha = 0.3)+
    geom_vline(aes(xintercept = 3*x), data = vline.dat.sd, linetype = "longdash", color = "black", alpha = 0.1)+
    geom_vline(aes(xintercept = -3*x), data = vline.dat.sd, linetype = "longdash", color = "black", alpha = 0.1)+
    geom_vline(aes(xintercept = 2*x), data = vline.dat.sd, linetype = "longdash", color = "black", alpha = 0.2)+
    geom_vline(aes(xintercept = -2*x), data = vline.dat.sd, linetype = "longdash", color = "black", alpha = 0.2)+
    geom_vline(aes(xintercept = 0))+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
  
  subset_d <- temp_2[month(temp_2$Date) == month(Sys.Date()),]
  vline.dat.mean <- aggregate(subset_d$value, list(subset_d$asset), mean)
  vline.dat.median <- aggregate(subset_d$value, list(subset_d$asset), median)
  names(vline.dat.mean)[1] <- "asset"
  names(vline.dat.median)[1] <- "asset" 
  
  b <- ggplot(subset_d, aes(x=value, fill = as.factor(asset)))+
    geom_histogram(bins = 10)+
    geom_density(alpha = 0.5)+
    facet_wrap(~ asset, ncol = 3, scales = "fixed")+
    ylab("")+
    xlab("")+
    ggtitle(paste("Return distributions for", month(Sys.Date(), label = TRUE, abbr = FALSE)))+
    geom_vline(aes(xintercept = x), data = vline.dat.mean, linetype = "longdash", color = "blue")+
    geom_vline(aes(xintercept = x), data = vline.dat.median, linetype = "longdash", color = "red")+
    geom_vline(aes(xintercept = 0))+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
  
  temp_5 <- temp_2[month(temp_2$Date) < month(Sys.Date()),]
  d <- ggplot(temp_5, aes(x=value, fill = as.factor(asset)))+
    geom_histogram(bins = 30)+
    geom_density(alpha = 0.5)+
    facet_wrap(~ asset, ncol = 3, scales = "fixed")+
    ylab("")+
    xlab("")+
    ggtitle(paste("Return distributions for all years to", month(Sys.Date(), abbr = FALSE, label = TRUE)))+
    geom_vline(aes(xintercept = x), data = vline.dat.mean, linetype = "longdash", color = "blue")+
    geom_vline(aes(xintercept = x), data = vline.dat.median, linetype = "longdash", color = "red")+
    geom_vline(aes(xintercept = 0))+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
  
  temp_2$ind <- ifelse(month(temp_2$Date) == month(Sys.Date()), "Current", "Other")
  temp_2$ind_2 <- ifelse(temp_2$ind == "Current" & year(temp_2$Date) == year(Sys.Date()), "Current", 
                         ifelse(temp_2$ind == "Current" & year(temp_2$Date) != year(Sys.Date()),"Current_Other","Other"))
  temp_3 <- temp_2[temp_2$ind == "Current" | temp_2$ind == "Current_Other",]
  temp_3$ind_2 <- ifelse(temp_3$ind_2 == "Current", "Current", "Previous")
  
  vline.dat.mean <- aggregate(temp_3$value, list(temp_3$ind_2), mean)
  vline.dat.median <- aggregate(temp_3$value, list(temp_3$ind_2), median)
  names(vline.dat.mean)[1] <- "ind_2"
  names(vline.dat.median)[1] <- "ind_2" 
  
  c <- ggplot(temp_3, aes(x=value, fill = as.factor(ind_2)))+
    geom_histogram(bins = 30)+
    geom_density(alpha = 0.5)+
    facet_wrap(~ ind_2, ncol = 2, scales = "fixed")+
    ylab("")+
    xlab("")+
    ggtitle(paste("Return distributions for", month(Sys.Date(), label = TRUE, abbr = FALSE), year(Sys.Date()), "vs aggregated returns for", month(Sys.Date(), label = TRUE, abbr = FALSE), "of previous years"))+
    geom_vline(aes(xintercept = x), data = vline.dat.mean, linetype = "longdash", color = "blue")+
    geom_vline(aes(xintercept = x), data = vline.dat.median, linetype = "longdash", color = "red")+
    geom_vline(aes(xintercept = 0))+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
  
  p <- list(a,d,b,c)
  p
}

getSymbols("SPY", from = "2008-01-01")
distributions(SPY)
getSymbols("^VIX", from = "2008-01-01")
distributions(VIX)

