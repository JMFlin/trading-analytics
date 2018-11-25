library(quantmod)   
library(lubridate)
library(ggplot2)
library(gridExtra)
Sys.setlocale("LC_TIME", "C")

getSymbols(c("GOOG", "AMZN", "AAPL", "NFLX", "FB", "MSFT"), env = .GlobalEnv, from="2017-01-01")
getSymbols("SPY", env = .GlobalEnv, from="2017-01-01")

FANG <- data.frame(GOOG[,"GOOG.Close"] + AMZN[,"AMZN.Close"] + NFLX[,"NFLX.Close"]+ FB[,"FB.Close"])
FANG$Date <- as.Date(row.names(FANG))
FANG$SPY <- SPY[,"SPY.Close"]
names(FANG)[3] <- "SPY"

FANG$FAANG <- data.frame(GOOG[,"GOOG.Close"] + AMZN[,"AMZN.Close"] + AAPL[,"AAPL.Close"] + NFLX[,"NFLX.Close"]+ FB[,"FB.Close"])
FANG$FAAMG <- data.frame(GOOG[,"GOOG.Close"] + AMZN[,"AMZN.Close"] + AAPL[,"AAPL.Close"] + MSFT[,"MSFT.Close"]+ FB[,"FB.Close"])


ggplot(FANG, aes(x=Date, y=scale(GOOG.Close), group = 1))+
  geom_line(col = "red")+
  geom_line(aes(x=Date, y=scale(SPY)))+
  geom_line(aes(x=Date, y=scale(FAANG)), col = "red", alpha = 0.2)+
  geom_line(aes(x=Date, y=scale(FAAMG)), col = "darkred", alpha = 0.2)+
  ylab("")+
  xlab("")+
  theme_bw()+
  ggtitle(paste("FANG/FAANG/FAAMG vs SPY", FANG$Date[nrow(FANG)]))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none',
        text = element_text(size=10))
