library(quantmod)
library(ggplot2)
library(scales)
library(reshape)
library(PerformanceAnalytics)
library(lubridate)
library(gridExtra)
Sys.setlocale("LC_TIME", "C")

b <- c("SPY")
a <- c('XLE','XLV','XLI','XLU','XLP','IYZ','XLK','XLY','XLF','XLB',
       'EFA','EEM',"FXY","JJC","HYG","LQD","MDY",
       'QQQ','DIA', 'IYT','TLT',"IEI",'IWM','VEU','EWG', "IYR", "UUP", "GLD", "MTUM", "IVE", "IVW") #'SLY'

l <- c(b,a)

getSymbols(l, from="2017-01-01")
l[1] <- "SPY"

# Function to extract "adjusted prices" and build dataframe: Thanks to Zach Mayer of moderntoolmaking.blogspot.com
symbolFrame <- function(symbolList) {
    Data <- data.frame(NULL)
    for (S in symbolList) {
        Data <- cbind(Data,Ad(get(S)))
    }
    colnames(Data) <- symbolList
    return(Data)
    
}

Data <- symbolFrame(l[-1]) # build a dataframe without DAX istelf
Data <- cbind(Ad(SPY), Data) # add DAX
colnames(Data)[1] <- "SP500"
tail(Data,2) #just to check - often Yahoo is not up to date and there are NAs in the last row
#Data <- window(Data, start=start(Data), end=end(Data)-1) # code to delete last row...

Data.r <- Return.calculate(Data, method="simple") #calculates the returns (simple, log)
Data.r[is.na(Data.r)] <- 0 

#builds frames for the respective perfromances on short, mid and long term
short.perf <- as.data.frame(coredata(tail(cumsum(tail(Data.r,5)),1)))
mid.perf <- as.data.frame(coredata(tail(cumsum(tail(Data.r,20)),1)))
long.perf <- as.data.frame(coredata(tail(cumsum(tail(Data.r,60)),1)))

per.df <- data.frame(cbind(t(short.perf), t(mid.perf), t(long.perf)))
colnames(per.df) <- c("short", "mid", "long")
row.names(per.df)[1] <- "SPY"
 
ggplot(data=per.df, aes(short, mid, label=rownames(per.df))) + 
  geom_point(aes(color=long), size=3) +
  geom_text(hjust=0, vjust=0, size=3) + 
  geom_vline(xintercept=0) + 
  geom_hline(yintercept=0) +
  ggtitle(paste("Performance of Major Asset Classes",end(Data)))+
  scale_colour_gradient2(low="red", high="green", "60 day\nPerformance") +
  scale_y_continuous("20 day Performance") +
  scale_x_continuous("5 day Performance")+
  theme_bw()+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(),
        legend.position='right')
