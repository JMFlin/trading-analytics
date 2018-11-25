library(quantmod)   
library(lubridate)
library(ggplot2)
library(reshape2)
library(gridExtra)
Sys.setlocale("LC_TIME", "C")

#Mean time below or at 20 day high!!

getSymbols("SPY", src = "yahoo", from = "2017-01-01")
SPY20 <- data.frame(SPY[,"SPY.Adjusted"], SPY[,"SPY.High"], SPY[,"SPY.Low"])
SPY20$Date <- as.Date(row.names(SPY20))

SPY20$indi <- 0
SPY20$indi2 <- 0
i <- 0
j <- i+20
for(i in 1:(nrow(SPY20)-20)){
  SPY20$indi[j] <- max(SPY20[i:j,"SPY.Adjusted"])
  j <- j+1
}
i <- 0
j <- i+20
for(i in 1:(nrow(SPY20)-20)){
  SPY20$indi2[j] <- max(SPY20[i:j,"SPY.High"])
  j <- j+1
}

SPY20$indi <- ifelse(SPY20$indi == 0, NA, SPY20$indi)
SPY20$indi2 <- ifelse(SPY20$indi2 == 0, NA, SPY20$indi2)

ggplot(SPY20, aes(x=Date, y=SPY.Adjusted))+
  geom_line()+
  geom_line(aes(x=Date, y=SPY20$indi), col = "red")+
  xlab("")+
  ylab("")+
  ggtitle("20 Day High Close")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none')

#yahoo data problems

ggplot(SPY20, aes(x=Date, y=SPY.High))+
  geom_line()+
  geom_line(aes(x=Date, y=SPY20$indi2),col = "red")+
  xlab("")+
  ylab("")+
  ggtitle("20 Day High")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none')

ggplot(SPY20, aes(x=Date, y=SPY.Adjusted))+
  geom_line()+
  geom_segment(aes(y=max(SPY20[month(SPY20$Date) == month(Sys.Date()) & year(SPY20$Date) == year(Sys.Date()),]$SPY.High), yend=  max(SPY20[month(SPY20$Date) == month(Sys.Date()) & year(SPY20$Date) == year(Sys.Date()),]$SPY.High), 
                   x=SPY20[SPY20$SPY.High == max(SPY20[month(SPY20$Date) == month(Sys.Date()) & year(SPY20$Date) == year(Sys.Date()), "SPY.High"]),]$Date, xend = as.Date(Sys.Date())), linetype = "dashed", size = 0.1)+
  geom_segment(aes(y=max(SPY20[month(SPY20$Date) == month(Sys.Date())-1 & year(SPY20$Date) == year(Sys.Date()),]$SPY.High), yend=  max(SPY20[month(SPY20$Date) == month(Sys.Date())-1 & year(SPY20$Date) == year(Sys.Date()),]$SPY.High), 
                   x=SPY20[SPY20$SPY.High == max(SPY20[month(SPY20$Date) == month(Sys.Date())-1 & year(SPY20$Date) == year(Sys.Date()), "SPY.High"]),]$Date, xend = as.Date(Sys.Date())), linetype = "dashed", size = 0.1)+
  geom_segment(aes(y=max(SPY20[month(SPY20$Date) == month(Sys.Date())-2 & year(SPY20$Date) == year(Sys.Date()),]$SPY.High), yend=  max(SPY20[month(SPY20$Date) == month(Sys.Date())-2 & year(SPY20$Date) == year(Sys.Date()),]$SPY.High), 
                   x=SPY20[SPY20$SPY.High == max(SPY20[month(SPY20$Date) == month(Sys.Date())-2 & year(SPY20$Date) == year(Sys.Date()), "SPY.High"]),]$Date, xend = as.Date(Sys.Date())), linetype = "dashed", size = 0.1)+
  geom_segment(aes(y=max(SPY20[month(SPY20$Date) == month(Sys.Date())-3 & year(SPY20$Date) == year(Sys.Date()),]$SPY.High), yend=  max(SPY20[month(SPY20$Date) == month(Sys.Date())-3 & year(SPY20$Date) == year(Sys.Date()),]$SPY.High), 
                   x=SPY20[SPY20$SPY.High == max(SPY20[month(SPY20$Date) == month(Sys.Date())-3 & year(SPY20$Date) == year(Sys.Date()), "SPY.High"]),]$Date, xend = as.Date(Sys.Date())), linetype = "dashed", size = 0.1)+
  geom_segment(aes(y=max(SPY20[month(SPY20$Date) == month(Sys.Date())-4 & year(SPY20$Date) == year(Sys.Date()),]$SPY.High), yend=  max(SPY20[month(SPY20$Date) == month(Sys.Date())-4 & year(SPY20$Date) == year(Sys.Date()),]$SPY.High), 
                   x=SPY20[SPY20$SPY.High == max(SPY20[month(SPY20$Date) == month(Sys.Date())-4 & year(SPY20$Date) == year(Sys.Date()), "SPY.High"]),]$Date, xend = as.Date(Sys.Date())), linetype = "dashed", size = 0.1)+
  
  xlab("")+
  ylab("")+
  ggtitle("Latest Months Highs")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none')


ggplot(SPY20, aes(x=Date, y=SPY.Adjusted))+
  geom_line()+
  geom_segment(aes(y=max(SPY20[month(SPY20$Date) == month(Sys.Date()) & year(SPY20$Date) == year(Sys.Date()),]$SPY.Adjusted), yend=  max(SPY20[month(SPY20$Date) == month(Sys.Date()) & year(SPY20$Date) == year(Sys.Date()),]$SPY.Adjusted), 
                   x=SPY20[SPY20$SPY.Adjusted == max(SPY20[month(SPY20$Date) == month(Sys.Date()) & year(SPY20$Date) == year(Sys.Date()), "SPY.Adjusted"]),]$Date, xend = as.Date(Sys.Date())), linetype = "dashed", size = 0.1)+
  geom_segment(aes(y=max(SPY20[month(SPY20$Date) == month(Sys.Date())-1 & year(SPY20$Date) == year(Sys.Date()),]$SPY.Adjusted), yend=  max(SPY20[month(SPY20$Date) == month(Sys.Date())-1 & year(SPY20$Date) == year(Sys.Date()),]$SPY.Adjusted), 
                   x=SPY20[SPY20$SPY.Adjusted == max(SPY20[month(SPY20$Date) == month(Sys.Date())-1 & year(SPY20$Date) == year(Sys.Date()), "SPY.Adjusted"]),]$Date, xend = as.Date(Sys.Date())), linetype = "dashed", size = 0.1)+
  geom_segment(aes(y=max(SPY20[month(SPY20$Date) == month(Sys.Date())-2 & year(SPY20$Date) == year(Sys.Date()),]$SPY.Adjusted), yend=  max(SPY20[month(SPY20$Date) == month(Sys.Date())-2 & year(SPY20$Date) == year(Sys.Date()),]$SPY.Adjusted), 
                   x=SPY20[SPY20$SPY.Adjusted == max(SPY20[month(SPY20$Date) == month(Sys.Date())-2 & year(SPY20$Date) == year(Sys.Date()), "SPY.Adjusted"]),]$Date, xend = as.Date(Sys.Date())), linetype = "dashed", size = 0.1)+
  geom_segment(aes(y=max(SPY20[month(SPY20$Date) == month(Sys.Date())-3 & year(SPY20$Date) == year(Sys.Date()),]$SPY.Adjusted), yend=  max(SPY20[month(SPY20$Date) == month(Sys.Date())-3 & year(SPY20$Date) == year(Sys.Date()),]$SPY.Adjusted), 
                   x=SPY20[SPY20$SPY.Adjusted == max(SPY20[month(SPY20$Date) == month(Sys.Date())-3 & year(SPY20$Date) == year(Sys.Date()), "SPY.Adjusted"]),]$Date, xend = as.Date(Sys.Date())), linetype = "dashed", size = 0.1)+
  geom_segment(aes(y=max(SPY20[month(SPY20$Date) == month(Sys.Date())-4 & year(SPY20$Date) == year(Sys.Date()),]$SPY.Adjusted), yend=  max(SPY20[month(SPY20$Date) == month(Sys.Date())-4 & year(SPY20$Date) == year(Sys.Date()),]$SPY.Adjusted), 
                   x=SPY20[SPY20$SPY.Adjusted == max(SPY20[month(SPY20$Date) == month(Sys.Date())-4 & year(SPY20$Date) == year(Sys.Date()), "SPY.Adjusted"]),]$Date, xend = as.Date(Sys.Date())), linetype = "dashed", size = 0.1)+
  
  xlab("")+
  ylab("")+
  ggtitle("Latest Months Closes")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none')

#yahoo data problems
ggplot(SPY20, aes(x=Date, y=SPY.Low))+
  geom_line()+
  geom_segment(aes(y=min(SPY20[month(SPY20$Date) == month(Sys.Date()) & year(SPY20$Date) == year(Sys.Date()),]$SPY.Low), yend=  min(SPY20[month(SPY20$Date) == month(Sys.Date()) & year(SPY20$Date) == year(Sys.Date()),]$SPY.Low), 
                   x=SPY20[SPY20$SPY.Low == min(SPY20[month(SPY20$Date) == month(Sys.Date()) & year(SPY20$Date) == year(Sys.Date()), "SPY.Low"]),]$Date, xend = as.Date(Sys.Date())), linetype = "dashed", size = 0.1)+
  geom_segment(aes(y=min(SPY20[month(SPY20$Date) == month(Sys.Date())-1 & year(SPY20$Date) == year(Sys.Date()),]$SPY.Low), yend=  min(SPY20[month(SPY20$Date) == month(Sys.Date())-1 & year(SPY20$Date) == year(Sys.Date()),]$SPY.Low), 
                   x=SPY20[SPY20$SPY.Low == min(SPY20[month(SPY20$Date) == month(Sys.Date())-1 & year(SPY20$Date) == year(Sys.Date()), "SPY.Low"]),]$Date, xend = as.Date(Sys.Date())), linetype = "dashed", size = 0.1)+
  geom_segment(aes(y=min(SPY20[month(SPY20$Date) == month(Sys.Date())-2 & year(SPY20$Date) == year(Sys.Date()),]$SPY.Low), yend=  min(SPY20[month(SPY20$Date) == month(Sys.Date())-2 & year(SPY20$Date) == year(Sys.Date()),]$SPY.Low), 
                   x=SPY20[SPY20$SPY.Low == min(SPY20[month(SPY20$Date) == month(Sys.Date())-2 & year(SPY20$Date) == year(Sys.Date()), "SPY.Low"]),]$Date, xend = as.Date(Sys.Date())), linetype = "dashed", size = 0.1)+
  geom_segment(aes(y=min(SPY20[month(SPY20$Date) == month(Sys.Date())-3 & year(SPY20$Date) == year(Sys.Date()),]$SPY.Low), yend=  min(SPY20[month(SPY20$Date) == month(Sys.Date())-3 & year(SPY20$Date) == year(Sys.Date()),]$SPY.Low), 
                   x=SPY20[SPY20$SPY.Low == min(SPY20[month(SPY20$Date) == month(Sys.Date())-3 & year(SPY20$Date) == year(Sys.Date()), "SPY.Low"]),]$Date, xend = as.Date(Sys.Date())), linetype = "dashed", size = 0.1)+
  geom_segment(aes(y=min(SPY20[month(SPY20$Date) == month(Sys.Date())-4 & year(SPY20$Date) == year(Sys.Date()),]$SPY.Low), yend=  min(SPY20[month(SPY20$Date) == month(Sys.Date())-4 & year(SPY20$Date) == year(Sys.Date()),]$SPY.Low), 
                   x=SPY20[SPY20$SPY.Low == min(SPY20[month(SPY20$Date) == month(Sys.Date())-4 & year(SPY20$Date) == year(Sys.Date()), "SPY.Low"]),]$Date, xend = as.Date(Sys.Date())), linetype = "dashed", size = 0.1)+
  
  xlab("")+
  ylab("")+
  ggtitle("Latest Months Lows")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(),
        axis.text.x = element_text(angle = 0),
        legend.position='none')



