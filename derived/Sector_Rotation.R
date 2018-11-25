library(quantmod)   
library(lubridate)
library(ggplot2)
library(gridExtra)
Sys.setlocale("LC_TIME", "C")

symbols <- c("SPY", "XLY", "XLP", "XLE","XLF", "XLV",   
             "XLI", "XLB", "XLK", "XLU", "IYZ", "XSD", "IYT")
sectors <- getSymbols(symbols, from="2015-01-01")

etf_ticker_sector <- data.frame(sectors)
etf_prices <- do.call(merge, lapply(sectors, function(x) Ad(get(x))))

etf_returns <- etf_prices
names(etf_returns) <- symbols
etf_returns[is.na(etf_returns)] <- 0

for(i in 1:nrow(etf_returns)){
  for(j in 1:ncol(etf_returns)){
    if(etf_returns[i,j] == 0){
      etf_returns[i,j] <- etf_returns[i-1,j]
    }
  }
}

SectorModel <- data.frame(etf_returns[,"XLF"], etf_returns[,"XLE"], etf_returns[,"XLU"], etf_returns[,"XLP"], etf_returns[,"XLY"])

XLF.ROC <- 100*Delt(SectorModel$XLF, k = 20)
XLU.ROC <- 100*Delt(SectorModel$XLU, k = 20)
XLE.ROC <- 100*Delt(SectorModel$XLE, k = 20)
XLP.ROC <- 100*Delt(SectorModel$XLP, k = 20)
XLY.ROC <- 100*Delt(SectorModel$XLY, k = 20)

Sec.ROC <- data.frame(XLF = XLF.ROC, XLU = XLU.ROC, XLE = XLE.ROC, XLP = XLP.ROC, XLY = XLY.ROC, 
                      Date = row.names(SectorModel))

Sec.ROC <- na.omit(Sec.ROC)
names(Sec.ROC) <- c("XLF", "XLU", "XLE", "XLP", "XLY", "Date")

Sec.ROC$Model <- (((Sec.ROC$XLF+Sec.ROC$XLY)/2)-((Sec.ROC$XLE+Sec.ROC$XLU+Sec.ROC$XLP)/3))

SPY <- data.frame(SPY)
SPY$Date <- row.names(SPY)
Sec.ROC <- cbind(Sec.ROC, SPY[21:nrow(SPY), "SPY.Close"])
names(Sec.ROC) <- c("XLF", "XLU", "XLE", "XLP", "XLY", "Date", "Model", "SPY")
Sec.ROC$Date <- as.Date(Sec.ROC$Date)

sec.plot <- ggplot(Sec.ROC[Sec.ROC$Date >= "2017-01-01",], aes(x=Date, y=Model, group = 1))+
  geom_line(aes(x=Date, y=Model))+
  geom_point(aes(x=Date, y=Model, col = Sec.ROC$Model[Sec.ROC$Date >= "2017-01-01"] > 0))+
  ylab("")+
  xlab("")+
  ggtitle("Sector Rotation Model")+
  geom_hline(yintercept = 0)+
  theme_bw()+
  #geom_hline(yintercept = 2*sd(Sec.ROC$Model[Sec.ROC$Date >= "2017-01-01"]), linetype = "dashed", col = "blue")+
  #geom_hline(yintercept = -2*sd(Sec.ROC$Model[Sec.ROC$Date >= "2017-01-01"]), linetype = "dashed", col = "red")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

spy.plot <- ggplot(Sec.ROC[Sec.ROC$Date >= "2017-01-03",], aes(x=Date, y=SPY, group = 1))+
  geom_line(aes(x=Date, y=SPY))+
  geom_point(aes(x=Date, y=SPY, col = Sec.ROC$Model[Sec.ROC$Date >= "2017-01-01"] > 0))+
  ylab("")+
  xlab("")+
  ggtitle(paste("SPY", Sec.ROC$Date[nrow(Sec.ROC)]))+
  theme_bw()+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

grid.arrange(spy.plot, sec.plot, ncol = 1)

etf_returns$cyclicals <-  etf_returns[,"XLY"]+etf_returns[,"XLF"]+etf_returns[,"XLI"]+etf_returns[,"XLB"]+etf_returns[,"XLK"]
etf_returns$defensives <- etf_returns[,"XLP"]+etf_returns[,"XLV"]+etf_returns[,"XLU"]+etf_returns[,"IYZ"]
etf_returns <- data.frame(etf_returns)
etf_returns$Date <- as.Date(row.names(etf_returns))
plot_data <- etf_returns[etf_returns$Date >= "2016-01-01",]

sector <- sectors[sectors %in% c("XSD", "IYT", "XLY", "XLU","XLP", "IYZ")]

etf_returns <- etf_prices
names(etf_returns) <- symbols
etf_returns[is.na(etf_returns)] <- 0

etf_returns <- data.frame(etf_returns)

etf_returns$Date <- as.Date(row.names(etf_returns))
plot_data2 <- etf_returns[etf_returns$Date >= "2016-01-01",]

for(i in 2:nrow(plot_data2)){
  for(j in 1:(ncol(plot_data2)-1)){
    if(plot_data2[i,j] <= 1){
      plot_data2[i,j] <- plot_data2[i-1,j]
    }
  }
}

plot_data2$cyclicals <- plot_data2[,"XSD"]+plot_data2[,"IYT"]+plot_data2[,"XLY"]
plot_data2$defensives <- plot_data2[,"XLP"]+plot_data2[,"IYZ"]+plot_data2[,"XLU"]

plot_data2$ind2 <- plot_data2$cyclicals/plot_data2$defensives

plot_data <- merge(plot_data, plot_data2[,c("Date", "ind2")], by = "Date")

ggplot(plot_data, aes(y=scale(plot_data$cyclicals/plot_data$defensives), x = Date, group = 1, col = "red"))+
  geom_line()+
  geom_line(aes(y=scale(ind2), x = Date, group = 1, col = "blue"))+
  ylab("")+
  xlab("")+
  theme_bw()+
  ggtitle(paste("Cyclicals/Defensives Performance", plot_data$Date[nrow(plot_data)]))+
  scale_x_date(date_breaks = "3 month", date_labels = "%Y %b")+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))

#This correlates with profit expectations of fund managers

#these look like the dollar and spread between us and jpy, ger
#AND market implied rate hikes! 
#regressions
