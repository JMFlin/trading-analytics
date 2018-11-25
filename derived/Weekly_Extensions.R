library(quantmod)
library(ggplot2)
library(scales)
library(reshape)
library(PerformanceAnalytics)
library(lubridate)
library(gridExtra)
Sys.setlocale("LC_TIME", "C")

getSymbols("^GSPC", src = "yahoo")

GSPC <- to.weekly(GSPC)
GSPC <- data.frame(GSPC = GSPC[,"GSPC.Adjusted"], Date = time(GSPC))

GSPC$ind <- 0
GSPC$cum <- 0

for(i in 3:nrow(GSPC)){
  if(GSPC[,1][i] > GSPC[,1][i-1]){
    GSPC$ind[i] <- 1 
  }else{
    GSPC$ind[i] <- -1
  }
}


for(i in 3:nrow(GSPC)){
    
  if(GSPC$ind[i] > 0 & GSPC$ind[i-1] > 0){
    GSPC$cum[i] <- GSPC$ind[i] + GSPC$cum[i-1]
  }else if(GSPC$ind[i] < 0 & GSPC$ind[i-1] < 0){
    GSPC$cum[i] <- GSPC$ind[i] + GSPC$cum[i-1]
  }else if(GSPC$ind[i] == 1 & GSPC$ind[i-1] == -1){
    GSPC$cum[i] <- 1
  }else if(GSPC$ind[i] == -1 & GSPC$ind[i-1] == 1){
    GSPC$cum[i] <- -1
  }
}

GSPC$mean_pos <- mean(GSPC$cum[GSPC$ind > 0])
GSPC$mean_neg <- mean(GSPC$cum[GSPC$ind < 0])

ggplot(GSPC[GSPC$Date >= "2017-01-01",], aes(x = Date, y = cum))+
  geom_line()+
  geom_point(data = GSPC[nrow(GSPC),], col ="blue", alpha = 0.5)+
  geom_text(data = GSPC[nrow(GSPC),],
            aes(label=cum, x=Date, y=cum), vjust = -0.5,size = 3.0)+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = GSPC$mean_pos, col = "blue", linetype = "dashed")+
  geom_hline(yintercept = GSPC$mean_neg, col = "red", linetype = "dashed")+
  theme_bw()+
  ylab("Weeks")+
  xlab("")+
  ggtitle("Weekly Extensions")+
  theme(axis.line = element_line(),
        axis.text=element_text(color='black'),
        axis.title = element_text(colour = 'black'),
        legend.text=element_text(),
        legend.title=element_text(),
        legend.position='none',
        text = element_text(size=10))

#SPX has gained 3 weeks in a row; most often, these streaks are followed by a higher high without too much interim give back. 


getSymbols("^GSPC", src = "yahoo")

GSPC <- to.monthly(GSPC)
GSPC <- data.frame(GSPC = GSPC[,"GSPC.Adjusted"], Date = as.Date(time(GSPC)))

GSPC$ind <- 0
GSPC$cum <- 0

for(i in 3:nrow(GSPC)){
  if(GSPC[,1][i] > GSPC[,1][i-1]){
    GSPC$ind[i] <- 1 
  }else{
    GSPC$ind[i] <- -1
  }
}


for(i in 3:nrow(GSPC)){
  
  if(GSPC$ind[i] > 0 & GSPC$ind[i-1] > 0){
    GSPC$cum[i] <- GSPC$ind[i] + GSPC$cum[i-1]
  }else if(GSPC$ind[i] < 0 & GSPC$ind[i-1] < 0){
    GSPC$cum[i] <- GSPC$ind[i] + GSPC$cum[i-1]
  }else if(GSPC$ind[i] == 1 & GSPC$ind[i-1] == -1){
    GSPC$cum[i] <- 1
  }else if(GSPC$ind[i] == -1 & GSPC$ind[i-1] == 1){
    GSPC$cum[i] <- -1
  }
}

GSPC$mean_pos <- mean(GSPC$cum[GSPC$ind > 0])
GSPC$mean_neg <- mean(GSPC$cum[GSPC$ind < 0])

ggplot(GSPC[GSPC$Date >= "2014-01-01",], aes(x = Date, y = cum))+
  geom_line()+
  geom_point()+
  geom_point(data = GSPC[nrow(GSPC),], col ="blue", alpha = 0.5)+
  geom_text(data = GSPC[nrow(GSPC),],
            aes(label=cum, x=Date, y=cum), vjust = -0.5,size = 3.0)+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = GSPC$mean_pos, col = "blue", linetype = "dashed")+
  geom_hline(yintercept = GSPC$mean_neg, col = "red", linetype = "dashed")+
  theme_bw()+
  ylab("Weeks")+
  xlab("")+
  ggtitle("Monthly Extensions")+
  theme(axis.line = element_line(),
        axis.text=element_text(color='black'),
        axis.title = element_text(colour = 'black'),
        legend.text=element_text(),
        legend.title=element_text(),
        legend.position='none',
        text = element_text(size=10))

#US equities are up three months in a row and positive for the year. 
#Historically, equities have a very strong propensity to end the year higher under these circumstances. 
#That remains our long term view.

#SPX has risen 3 months in a row, every month of 2Q. Historically, 
#when stocks rise during this period, they have risen the next 4, 6 and 12 months in 14 of 15 instances (93%). 
#Risk/reward over the next 12 months has been highly favorable