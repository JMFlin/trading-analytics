#https://www.advisorperspectives.com/dshort/updates/2017/05/31/moving-averages-may-month-end-update

Ivy_Index_Mean <- function(data){
  
  df_month <- data.frame(to.monthly(data))
  df_month$Date <- as.Date(as.yearmon(row.names(df_month)))
  
  a <- ggplot(df_month, aes(x=Date, y = df_month[,grep(".Close", names(df_month))]))+
    geom_line()+
    geom_point(aes(col = df_month[,grep(".Close", names(df_month))] > SMA(df_month[,grep(".Close", names(df_month))], n = 10)))+
    geom_line(aes(x=Date, y = SMA(df_month[,grep(".Close", names(df_month))], n = 10)))+
    ylab("")+
    xlab("")+
    ggtitle("SPY and 10 Month SMA")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
  
  df <- cbind(df_month, SMA(df_month[,grep(".Close", names(df_month))], n=10))
  names(df)[ncol(df)] <- "SMA10"
  
  df$SMA10Value <- ((df[,grep(".Close", names(df))]-df$SMA10) / (df$SMA10))*100
  
  b <- ggplot(df, aes(x=Date, y=SMA10Value, group = 1))+
    geom_line()+
    geom_point()+
    ylab("")+
    xlab("")+
    ggtitle("% Above/Below SMA")+
    geom_hline(yintercept = 0)+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
  
  grid.arrange(a,b)
}

Ivy_Index_Med_Mad <- function(data){
  
  df_month <- data.frame(to.monthly(data))
  df_month$Date <- as.Date(as.yearmon(row.names(df_month)))
  
  x <- c(".Close", "Date")
  df <- df_month[,grep(paste(x, collapse = "|"), names(df_month))]
  
  median_mad <- function(data, price){
    cum_med <- runMedian(data[,price], n = 12, non.unique = "mean", cumulative = FALSE)
    cum_mad <- runMAD(data[,price], n = 12, center = NULL, stat = "median", constant = 1.4826, non.unique = "mean", cumulative = FALSE)
    return(data.frame(Date = data[,"Date"], Price = data[,price], cum_med, cum_mad))
  }
  
  df_month <- median_mad(df, names(df)[grep(".Close", names(df))])

  a <- ggplot(df_month, aes(x=Date, y = df_month[,grep("Price", names(df_month))]))+
    geom_line()+
    geom_point(aes(col = df_month[,grep("Price", names(df_month))] > df_month[,grep("cum_med", names(df_month))]))+
    geom_line(aes(x=Date, y = cum_med))+
    #geom_line(aes(x=Date, y = cum_med+1*cum_mad), col = "green", linetype = "dashed")+
    #geom_line(aes(x=Date, y = cum_med-1*cum_mad), col = "green", linetype = "dashed")+
    #geom_line(aes(x=Date, y = cum_med+2*cum_mad), col = "red", linetype = "dashed")+
    #geom_line(aes(x=Date, y = cum_med-2*cum_mad), col = "blue", linetype = "dashed")+
    #geom_line(aes(x=Date, y = cum_med+3*cum_mad), col = "red", linetype = "dashed")+
    #geom_line(aes(x=Date, y = cum_med-3*cum_mad), col = "blue", linetype = "dashed")+
    ylab("")+
    xlab("")+
    ggtitle("SPY and 12 Median")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
  
  df_month$Val <- ((df_month[,grep("Price", names(df_month))]-df_month$cum_med) / (df_month$cum_med))*100
  
  b <- ggplot(df_month, aes(x=Date, y=Val, group = 1))+
    geom_line()+
    geom_point()+
    ylab("")+
    xlab("")+
    ggtitle("% Above/Below Median")+
    geom_hline(yintercept = 0)+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
  
  grid.arrange(a,b)
}

getSymbols('SPY', from = "1990-01-01")
Ivy_Index_Mean(SPY)

Ivy_Index_Med_Mad(SPY)


