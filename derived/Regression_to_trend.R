library(quantmod)   
library(lubridate)
library(ggplot2)
library(gridExtra)
Sys.setlocale("LC_TIME", "C")

Reg_trend <- function(data, spread = FALSE){

  if(spread == FALSE){
    df <- data.frame(data[,grep(".Adjusted", names(data))])
    df$Date <- as.Date(rownames(df))
    Dis <- df[df$Date >= ymd(df$Date[nrow(df)])-years(1),]
  
    spy_df <- data.frame(Date = Dis$Date[Dis$Date >= ymd(Dis$Date[nrow(Dis)])-years(1)], 
                     SPY = Dis[,grep(".Adjusted", names(Dis))][Dis$Date >= ymd(Dis$Date[nrow(Dis)])-years(1)], 
                     reg = lm(Dis[,grep(".Adjusted", names(Dis))][Dis$Date >= ymd(Dis$Date[nrow(Dis)])-years(1)] ~ Dis$Date[Dis$Date >= ymd(Dis$Date[nrow(Dis)])-years(1)])$residuals)
   
    names(spy_df)[c(seq(2,ncol(spy_df),3))] <- names(df)[1:(ncol(df)-1)]
    names(spy_df)[c(seq(3,ncol(spy_df),3))] <- paste0(names(spy_df)[c(seq(3,ncol(spy_df),3))],".",names(df)[1:(ncol(df)-1)])
    
    spy_df2 <- data.frame(Date = Dis$Date[Dis$Date >= as.character(cut(Sys.Date(), "year"))], 
                      SPY = Dis[,grep(".Adjusted", names(Dis))][Dis$Date >= as.character(cut(Sys.Date(), "year"))], 
                      reg = lm(Dis[,grep(".Adjusted", names(Dis))][Dis$Date >= as.character(cut(Sys.Date(), "year"))] ~ Dis$Date[Dis$Date >= as.character(cut(Sys.Date(), "year"))])$residuals)
   
    names(spy_df2)[c(seq(2,ncol(spy_df2),3))] <- names(df)[1:(ncol(df)-1)]
    names(spy_df2)[c(seq(3,ncol(spy_df2),3))] <- paste0(names(spy_df2)[c(seq(3,ncol(spy_df2),3))],".",names(df)[1:(ncol(df)-1)])
   
  }else{
    df <- data
    df$Date <- as.Date(rownames(df))
    Dis <- df[df$Date >= ymd(df$Date[nrow(df)])-years(1),]
    
    spy_df <- data.frame(Date = Dis$Date[Dis$Date >= ymd(Dis$Date[nrow(Dis)])-years(1)], 
                         a = Dis[,1][Dis$Date >= ymd(Dis$Date[nrow(Dis)])-years(1)], 
                         reg = lm(Dis[,1][Dis$Date >= ymd(Dis$Date[nrow(Dis)])-years(1)] ~ Dis$Date[Dis$Date >= ymd(Dis$Date[nrow(Dis)])-years(1)])$residuals)
    if(length(names(df)) > 2){
      for(i in 2:(length(names(Dis))-1)){
        spy_df_tmp <- data.frame(Date = Dis$Date[Dis$Date >= ymd(Dis$Date[nrow(Dis)])-years(1)], 
                           b = Dis[,i][Dis$Date >= ymd(Dis$Date[nrow(Dis)])-years(1)], 
                           reg = lm(Dis[,i][Dis$Date >= ymd(Dis$Date[nrow(Dis)])-years(1)] ~ Dis$Date[Dis$Date >= ymd(Dis$Date[nrow(Dis)])-years(1)])$residuals)
        spy_df <- cbind(spy_df, spy_df_tmp)
      }
    }
    names(spy_df)[c(seq(2,ncol(spy_df),3))] <- names(df)[1:(ncol(df)-1)]
    names(spy_df)[c(seq(3,ncol(spy_df),3))] <- paste0(names(spy_df)[c(seq(3,ncol(spy_df),3))],".",names(df)[1:(ncol(df)-1)])
    names(spy_df) <- sub("/","\\.", names(spy_df))
    
    spy_df2 <- data.frame(Date = Dis$Date[Dis$Date >= as.character(cut(Sys.Date(), "year"))], 
                          SPY = Dis[,1][Dis$Date >=as.character(cut(Sys.Date(), "year"))], 
                          reg = lm(Dis[,1][Dis$Date >= as.character(cut(Sys.Date(), "year"))] ~ Dis$Date[Dis$Date >= as.character(cut(Sys.Date(), "year"))])$residuals)
  
    if(length(names(df)) > 2){
      for(i in 2:(length(names(Dis))-1)){
        spy_df2_tmp <- data.frame(Date = Dis$Date[Dis$Date >= as.character(cut(Sys.Date(), "year"))], 
                                SPY = Dis[,i][Dis$Date >=as.character(cut(Sys.Date(), "year"))], 
                                reg = lm(Dis[,i][Dis$Date >= as.character(cut(Sys.Date(), "year"))] ~ Dis$Date[Dis$Date >=  as.character(cut(Sys.Date(), "year"))])$residuals)
        spy_df2  <- cbind(spy_df2 , spy_df2_tmp)
      }
    }
     names(spy_df2)[c(seq(2,ncol(spy_df2),3))] <- names(df)[1:(ncol(df)-1)]
     names(spy_df2)[c(seq(3,ncol(spy_df2),3))] <- paste0(names(spy_df2)[c(seq(3,ncol(spy_df2),3))],".",names(df)[1:(ncol(df)-1)])
     names(spy_df2) <- sub("/","\\.", names(spy_df2))
  }
  
  for(i in names(spy_df)[c(names(spy_df) != "Date" & !grepl('reg', colnames(spy_df)))]){

    a <- ggplot(spy_df2, aes_string(x="Date", y=names(spy_df2)[(which(names(spy_df2) %in% i)+1)]), group = 1)+
      geom_line()+
      geom_point(size = 0.5)+
      ylab("")+
      xlab("")+
      geom_hline(yintercept = 0)+
      geom_hline(yintercept = 0-2*sd(spy_df2[,names(spy_df2)[(which(names(spy_df2) %in% i)+1)]]), col = "blue", linetype = "dashed")+
      geom_hline(yintercept = 0+2*sd(spy_df2[,names(spy_df2)[(which(names(spy_df2) %in% i)+1)]]), col = "red", linetype = "dashed")+
      ggtitle(paste("YTD Residuals"))+
      scale_x_date(date_breaks = "1 month", date_labels = "%b")+
      theme_bw()+
      theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

    b <- ggplot(spy_df, aes_string(x="Date", y=names(spy_df)[(which(names(spy_df) %in% i)+1)]), group = 1)+
      geom_line()+
      geom_point(size = 0.5)+
      ylab("")+
      xlab("")+
      geom_hline(yintercept = 0)+
      geom_hline(yintercept = 0-2*sd(spy_df[,names(spy_df)[(which(names(spy_df) %in% i)+1)]]), col = "blue", linetype = "dashed")+
      geom_hline(yintercept = 0+2*sd(spy_df[,names(spy_df)[(which(names(spy_df) %in% i)+1)]]), col = "red", linetype = "dashed")+
      ggtitle(paste("One Year Residuals"))+
      scale_x_date(date_breaks = "2 month", date_labels = "%b")+
      theme_bw()+
      theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))
    if(spread == TRUE){
      c <- ggplot(spy_df, aes_string(x="Date", y=i, group = 1))+
        geom_line()+
        ylab("")+
        xlab("")+
        ggtitle(sub("\\.","/", i))+
        scale_x_date(date_breaks = "2 month")+
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
    if(spread == FALSE){
      c <- ggplot(spy_df, aes_string(x="Date", y=i, group = 1))+
        geom_line()+
        ylab("")+
        xlab("")+
        ggtitle(i)+
        scale_x_date(date_breaks = "2 month")+
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
    grid.arrange(c, ncol = 1, arrangeGrob(a,b, ncol = 2))
  }
}

spread_df <- function(my_data){
  tt <- my_data[,grep(".Adjusted", names(my_data))]
  p <- list()
  for(i in names(tt)){
    u <- which(colnames(tt)==i)
    if(u != length(names(tt))){
      nam <- names(tt)[(u+1):length(names(tt))]
      for(j in nam){
        a <- my_data[,i]/my_data[,j]
        p <- c(p, list(a))
      }
    }
  }
  df <- do.call(cbind.data.frame, p)
  
  vect <- c()
  for(i in names(tt)){
    u <- which(colnames(tt)==i)
    if(u != length(names(tt))){
      nam <- names(tt)[(u+1):length(names(tt))]
      for(j in nam){
        vect <- append(vect, paste0(strtrim(i,3),"/",strtrim(j,3)), length(vect))
      }
    }
  }
  names(df) <- vect
  return(df)
}

getSymbols(c("SPY", "TLT", "GLD"), from="1990-01-01")
Reg_trend(SPY)
Reg_trend(TLT)
Reg_trend(GLD)

my_data <- Reduce(function(x,y) merge(x, y), list(TLT, SPY, GLD))
my_data <- spread_df(my_data)
Reg_trend(my_data, spread = TRUE)
