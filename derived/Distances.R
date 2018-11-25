library(quantmod)   
library(lubridate)
library(ggplot2)
library(gridExtra)
Sys.setlocale("LC_TIME", "C")

Distances <- function(data){
  GSPC <- data.frame(data)
  
  GSPC$Date <- as.Date(row.names(GSPC))
  
  GSPC <- data.frame(Price = GSPC[,grep(".Adjusted", names(data))], Date = as.Date(row.names(GSPC)))
  
  GSPC$Ema20 <- EMA(GSPC$Price, n=20)
  GSPC$Ema50 <- EMA(GSPC$Price, n=50)
  GSPC$Ema200 <- EMA(GSPC$Price, n=200)
  
  GSPC <- na.omit(GSPC)
  
  GSPC$Ind20 <- ifelse(GSPC$Price > GSPC$Ema20,1,0)
  GSPC$Ind50 <- ifelse(GSPC$Price > GSPC$Ema50,1,0)
  GSPC$Ind200 <- ifelse(GSPC$Price > GSPC$Ema200,1,0)
  
  
  vect <- c()
  p <- 0
  j <- 0
  for(i in 1:nrow(GSPC)){
    if(GSPC$Ind20[i] == 1){
      j <- 0
      p <- p+1
      vect <- append(vect, p, length(vect))
    }else{
      p <- 0
      j <- j-1
      vect <- append(vect, j, length(vect))
    }
  }
  GSPC$vect20 <- vect
  GSPC$Deltind20 <- Delt(GSPC$vect20, k=1, type = c("arithmetic"))
  
  for(i in 2:nrow(GSPC)){
    if(GSPC$Deltind20[i] < 0){
      GSPC$Deltind20[i-1] <- 1
      GSPC$Deltind20[i] <- 0
    }else{
      GSPC$Deltind20[i] <- 0
    }
  }
  GSPC$Deltind20 <- ifelse(GSPC$vect20 <0,0,GSPC$Deltind20)
  GSPC$mean20 <- mean(GSPC[GSPC$Deltind20==1,"vect20"], na.rm = T)
  
  GSPC$negDeltind20 <- Delt(GSPC$vect20, k=1, type = c("arithmetic"))
  for(i in 2:nrow(GSPC)){
    if(GSPC$negDeltind20[i] < 0){
      GSPC$negDeltind20[i-1] <- 1
      GSPC$negDeltind20[i] <- 0
    }else{
      GSPC$negDeltind20[i] <- 0
    }
  }
  GSPC$negDeltind20 <- ifelse(GSPC$vect20 >0,0,GSPC$negDeltind20)
  GSPC$negmean20 <- mean(GSPC[GSPC$negDeltind20==1,"vect20"], na.rm = T)
  
  ma20 <- ggplot(GSPC[GSPC$Date >="2015-01-01",], aes(x=Date,y=vect20))+
    geom_line()+
    geom_hline(yintercept = 0)+
    geom_hline(yintercept = GSPC$mean20, linetype = "dashed")+
    geom_hline(yintercept = GSPC$mean20+2*sd(GSPC[GSPC$Deltind20==1,"vect20"], na.rm = T), linetype = "dashed", col = "red")+
    geom_hline(yintercept = GSPC$negmean20, linetype = "dashed")+
    geom_hline(yintercept = GSPC$negmean20-2*sd(GSPC[GSPC$negDeltind20==1,"vect20"], na.rm = T), linetype = "dashed", col = "blue")+
    ylab("")+
    xlab("")+
    ggtitle(paste("Days above 20 DEMA for", gsub("\\..*$","", names(data)[1])))+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0 ),
          legend.position='none',
          text = element_text(size=10))
  
  dist20 <- ggplot(GSPC, aes(x=vect20))+
    geom_histogram(aes(x=vect20), col = "black", bins = 50)+
    geom_vline(xintercept = GSPC$vect20[nrow(GSPC)], col = "red")+
    geom_vline(xintercept = sd(GSPC[,"vect20"], na.rm = T), linetype = "dashed", col ="red")+
    geom_vline(xintercept = -sd(GSPC[,"vect20"], na.rm = T), linetype = "dashed", col = "blue")+
    geom_vline(xintercept = 2*sd(GSPC[,"vect20"], na.rm = T), linetype = "dashed", col ="red")+
    geom_vline(xintercept = -2*sd(GSPC[,"vect20"], na.rm = T), linetype = "dashed", col = "blue")+
    geom_vline(xintercept = 3*sd(GSPC[,"vect20"], na.rm = T), linetype = "dashed", col ="red")+
    ylab("")+
    xlab("")+
    ggtitle("")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0 ),
          legend.position='none',
          text = element_text(size=10))
  
  
  vect <- c()
  p <- 0
  j <- 0
  for(i in 1:nrow(GSPC)){
    if(GSPC$Ind50[i] == 1){
      j <- 0
      p <- p+1
      vect <- append(vect, p, length(vect))
    }else{
      p <- 0
      j <- j-1
      vect <- append(vect, j, length(vect))
    }
  }
  GSPC$vect50 <- vect
  GSPC$Deltind50 <- Delt(GSPC$vect50, k=1, type = c("arithmetic"))
  
  for(i in 2:nrow(GSPC)){
    if(GSPC$Deltind50[i] < 0){
      GSPC$Deltind50[i-1] <- 1
      GSPC$Deltind50[i] <- 0
    }else{
      GSPC$Deltind50[i] <- 0
    }
  }
  GSPC$Deltind50 <- ifelse(GSPC$vect50 <0,0,GSPC$Deltind50)
  GSPC$mean50 <- mean(GSPC[GSPC$Deltind50==1,"vect50"], na.rm = T)
  
  GSPC$negDeltind50 <- Delt(GSPC$vect50, k=1, type = c("arithmetic"))
  for(i in 2:nrow(GSPC)){
    if(GSPC$negDeltind50[i] < 0){
      GSPC$negDeltind50[i-1] <- 1
      GSPC$negDeltind50[i] <- 0
    }else{
      GSPC$negDeltind50[i] <- 0
    }
  }
  GSPC$negDeltind50 <- ifelse(GSPC$vect50 >0,0,GSPC$negDeltind50)
  GSPC$negmean50 <- mean(GSPC[GSPC$negDeltind50==1,"vect50"], na.rm = T)
  
  ma50 <- ggplot(GSPC[GSPC$Date >="2010-01-01",], aes(x=Date,y=vect50))+
    geom_line()+
    geom_hline(yintercept = 0)+
    geom_hline(yintercept = GSPC$mean50, linetype = "dashed")+
    geom_hline(yintercept = GSPC$mean50+2*sd(GSPC[GSPC$Deltind50==1,"vect50"], na.rm = T), linetype = "dashed", col = "red")+
    geom_hline(yintercept = GSPC$negmean50, linetype = "dashed")+
    geom_hline(yintercept = GSPC$negmean50-2*sd(GSPC[GSPC$negDeltind50==1,"vect50"], na.rm = T), linetype = "dashed", col = "blue")+
    ylab("")+
    xlab("")+
    ggtitle("Days above 50 DEMA")+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0 ),
          legend.position='none',
          text = element_text(size=10))
  
  dist50 <- ggplot(GSPC, aes(x=vect50))+
    geom_histogram(aes(x=vect50), col = "black", bins = 70)+
    geom_vline(xintercept = GSPC$vect50[nrow(GSPC)], col = "red")+
    geom_vline(xintercept = sd(GSPC[,"vect50"], na.rm = T), linetype = "dashed", col ="red")+
    geom_vline(xintercept = -sd(GSPC[,"vect50"], na.rm = T), linetype = "dashed", col = "blue")+
    geom_vline(xintercept = 2*sd(GSPC[,"vect50"], na.rm = T), linetype = "dashed", col ="red")+
    geom_vline(xintercept = -2*sd(GSPC[,"vect50"], na.rm = T), linetype = "dashed", col = "blue")+
    geom_vline(xintercept = 3*sd(GSPC[,"vect50"], na.rm = T), linetype = "dashed", col ="red")+
    ylab("")+
    xlab("")+
    ggtitle("")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0 ),
          legend.position='none',
          text = element_text(size=10))
  
  
  vect <- c()
  p <- 0
  j <- 0
  for(i in 1:nrow(GSPC)){
    if(GSPC$Ind200[i] == 1){
      j <- 0
      p <- p+1
      vect <- append(vect, p, length(vect))
    }else{
      p <- 0
      j <- j-1
      vect <- append(vect, j, length(vect))
    }
  }
  GSPC$vect200 <- vect
  GSPC$Deltind200 <- Delt(GSPC$vect200, k=1, type = c("arithmetic"))
  
  for(i in 2:nrow(GSPC)){
    if(GSPC$Deltind200[i] < 0){
      GSPC$Deltind200[i-1] <- 1
      GSPC$Deltind200[i] <- 0
    }else{
      GSPC$Deltind200[i] <- 0
    }
  }
  GSPC$Deltind200 <- ifelse(GSPC$vect200 <0,0,GSPC$Deltind200)
  GSPC$mean200 <- mean(GSPC[GSPC$Deltind200==1,"vect200"], na.rm = T)
  
  GSPC$negDeltind200 <- Delt(GSPC$vect200, k=1, type = c("arithmetic"))
  for(i in 2:nrow(GSPC)){
    if(GSPC$negDeltind200[i] < 0){
      GSPC$negDeltind200[i-1] <- 1
      GSPC$negDeltind200[i] <- 0
    }else{
      GSPC$negDeltind200[i] <- 0
    }
  }
  GSPC$negDeltind200 <- ifelse(GSPC$vect200 >0,0,GSPC$negDeltind200)
  GSPC$negmean200 <- mean(GSPC[GSPC$negDeltind200==1,"vect200"], na.rm = T)
  
  ma200 <- ggplot(GSPC[GSPC$Date >="1990-01-01",], aes(x=Date,y=vect200))+
    geom_line()+
    geom_hline(yintercept = 0)+
    geom_hline(yintercept = GSPC$mean200, linetype = "dashed")+
    geom_hline(yintercept = GSPC$mean200+2*sd(GSPC[GSPC$Deltind200==1,"vect200"], na.rm = T), linetype = "dashed", col ="red")+
    geom_hline(yintercept = GSPC$negmean200, linetype = "dashed")+
    geom_hline(yintercept = GSPC$negmean200-2*sd(GSPC[GSPC$negDeltind200==1,"vect200"], na.rm = T), linetype = "dashed", col = "blue")+
    ylab("")+
    xlab("")+
    ggtitle("Days above 200 DEMA")+
    scale_x_date(date_breaks = "3 year", date_labels = "%Y")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0 ),
          legend.position='none',
          text = element_text(size=10))
  
  dist200 <- ggplot(GSPC, aes(x=vect200))+
    geom_histogram(aes(x=vect200), col = "black", bins = 100)+
    geom_vline(xintercept = GSPC$vect200[nrow(GSPC)], col = "red")+
    geom_vline(xintercept = sd(GSPC[,"vect200"], na.rm = T), linetype = "dashed", col ="red")+
    geom_vline(xintercept = -sd(GSPC[,"vect200"], na.rm = T), linetype = "dashed", col = "blue")+
    geom_vline(xintercept = 2*sd(GSPC[,"vect200"], na.rm = T), linetype = "dashed", col ="red")+
    geom_vline(xintercept = -2*sd(GSPC[,"vect200"], na.rm = T), linetype = "dashed", col = "blue")+
    geom_vline(xintercept = 3*sd(GSPC[,"vect200"], na.rm = T), linetype = "dashed", col ="red")+
    ylab("")+
    xlab("")+
    ggtitle("")+
    theme_bw()+
    theme(axis.line = element_line(), 
          axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), 
          legend.text=element_text(), 
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0 ),
          legend.position='none',
          text = element_text(size=10))
  
  grid.arrange(ma20, dist20, ma50, dist50, ma200, dist200, ncol = 2)
}
getSymbols("SPY", from = "1990-01-01")
getSymbols("QQQ", from = "1990-01-01")
getSymbols("IWM", from = "1990-01-01")
Distances(SPY)
Distances(QQQ)
Distances(IWM)

getSymbols("TLT", from = "1990-01-01")
getSymbols("GLD", from = "1990-01-01")
Distances(TLT)
Distances(GLD)

getSymbols("^VIX", from = "1990-01-01")
Distances(VIX)
#During the past 7 years, drops of more than 4% in NDX have always coincided with falls of at least 3% in SPY. 
#That doesn't sound like much, but it would be the largest drop so far in 2017.