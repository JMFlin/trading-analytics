library(quantmod)
library(gridExtra)
library(ggplot2)
library(lubridate)

Disparity_Index <- function(data){
  SPY_1 <- data.frame(data)
  SPY_1$Date <- as.Date(row.names(SPY_1))
  names(SPY_1) <- c("SPY.Open", "SPY.High","SPY.Low","SPY","SPY.Volume","SPY.Adjusted","Date" )
  
  Dis <- cbind(SPY_1,EMA(SPY_1$SPY, n=20), EMA(SPY_1$SPY, n=50), EMA(SPY_1$SPY, n=200))#,  ratio=2/(5+1)
  names(Dis)[8:10] <- c("EMA20", "EMA50", "EMA200")
  Dis <- na.omit(Dis)
  
  Dis$Ema20Value <- ((Dis$SPY-Dis$EMA20) / ((Dis$SPY+Dis$EMA20)/2))*100
  Dis$Ema20sd1.5 <- 1.5*sd(Dis$Ema20Value) 
  Dis$Ema20sdneg2 <- -1.5*sd(Dis$Ema20Value)
  Dis$Date <- as.Date(Dis$Date)
  
  Dis$Ema50Value <- ((Dis$SPY-Dis$EMA50) / ((Dis$SPY+Dis$EMA50)/2))*100
  Dis$Ema50sd1.5 <- 1.5*sd(Dis$Ema50Value) 
  Dis$Ema50sdneg2 <- -1.5*sd(Dis$Ema50Value)
  
  Dis$Ema200Value <- ((Dis$SPY-Dis$EMA200) / ((Dis$SPY+Dis$EMA200)/2))*100
  Dis$Ema200sd1.5 <- 1.5*sd(Dis$Ema200Value) 
  Dis$Ema200sdneg2 <- -1.5*sd(Dis$Ema200Value)
  Dis$Ema200sd2 <- 2*sd(Dis$Ema200Value)
  
  dis20.plot <- ggplot(Dis[Dis$Date >= "2017-01-01",], aes(x=Date, y=Ema20Value, group = 1))+
    geom_line(aes(x=Date, y=Ema20Value))+
    geom_point(aes(x=Date, y=Ema20Value))+
    ylab("")+
    xlab("")+
    ggtitle("Disparity Index 20")+
    geom_hline(yintercept = 0)+
    geom_hline(aes(yintercept = Ema20sd1.5), linetype = "dashed")+
    geom_hline(aes(yintercept = Ema20sdneg2), linetype = "dashed")+
    scale_x_date(date_breaks = "1 month", date_labels = "%Y-%b")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
  
  dis50.plot <- ggplot(Dis[Dis$Date >= "2017-01-01",], aes(x=Date, y=Ema50Value, group = 1))+
    geom_line(aes(x=Date, y=Ema50Value))+
    geom_point(aes(x=Date, y=Ema50Value))+
    ylab("")+
    xlab("")+
    ggtitle("Disparity Index 50")+
    geom_hline(yintercept = 0)+
    geom_hline(yintercept = Dis$Ema50sd1.5, linetype = "dashed")+
    geom_hline(yintercept = Dis$Ema50sdneg2, linetype = "dashed")+
    scale_x_date(date_breaks = "1 month", date_labels = "%Y-%b")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
  
  dis200.plot <- ggplot(Dis[Dis$Date >= "2017-01-01",], aes(x=Date, y=Ema200Value, group = 1))+
    geom_line(aes(x=Date, y=Ema200Value))+
    geom_point(aes(x=Date, y=Ema200Value))+
    ylab("")+
    xlab("")+
    ggtitle("Disparity Index 200")+
    geom_hline(yintercept = 0)+
    #geom_hline(yintercept = Dis$Ema200sd1.5, linetype = "dashed")+
    geom_hline(yintercept = Dis$Ema200sdneg2, linetype = "dashed")+
    geom_hline(yintercept = Dis$Ema200sd2, linetype = "dashed")+
    scale_x_date(date_breaks = "1 month", date_labels = "%Y-%b")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))
  
  grid.arrange(dis20.plot,dis50.plot,dis200.plot, ncol = 1)
}

getSymbols("SPY", from="2010-01-01")
getSymbols("TLT", from = "2010-01-01")
getSymbols("GLD", from = "2010-01-01")
getSymbols("UUP", from = "2010-01-01")
getSymbols("IWM", from = "2010-01-01")
getSymbols("QQQ", from = "2010-01-01")

Disparity_Index(SPY)
Disparity_Index(IWM)
Disparity_Index(QQQ)
Disparity_Index(TLT)
Disparity_Index(GLD)
Disparity_Index(UUP)

