library(ggplot2)
library(gdata)

getshiller <- function(source="http://www.econ.yale.edu/~shiller/data/ie_data.xls",cachefile="shiller.RData") {
  
  if (file.exists(cachefile)) {
    load(cachefile)
    return(x)
  }
  xraw <- read.xls(source, sheet = 1, verbose=FALSE, perl="perl")
  xraw[xraw=="#N/A"]  <- NA
  rawrows    <- dim(xraw)[1]
  rawcols    <- 11
  t.x  <- as.Date(sprintf("%4.2f.01",as.numeric(as.vector(xraw[,1]))),"%Y.%m.%d")
  ok   <- !is.na(t.x)
  x    <- zoo(as.numeric(as.vector(xraw[ok,2])),t.x[ok])
  for ( i in seq(3,rawcols) ) {
    z    <- zoo(as.numeric(as.vector(xraw[ok,i])),t.x[ok])
    x    <- merge.zoo(x,z)
  }
  colnames(x) <- c("P","D","E","CPI","DateFraction","LongRate","P.real","D.real","E.real","CAPE")
  save(x,file=cachefile)
  return(x)
}

shiller <- getshiller()
#spx <- shiller$P.real
shiller <- data.frame(shiller)
len <- function(x){
  length(x)
}

mlag <- function(m, nlag = lag){
  if(is.null(dim(m))){
    n = len(m)
    if(nlag > 0) {
      m[(nlag+1):n] = m[1:(n-nlag)]
      m[1:nlag] = NA
    }else if(nlag < 0){
      m[1:(n+nlag)] = m[(1-nlag):n]
      m[(n+nlag+1):n] = NA
    }
  } else {
    n = nrow(m)
    if(nlag > 0) {
      m[(nlag+1):n,] = m[1:(n-nlag),]
      m[1:nlag,] = NA
    } else if(nlag < 0) {
      m[1:(n+nlag),] = m[(1-nlag):n,]
      m[(n+nlag+1):n,] = NA
    }
  }
  return(m);
}
shiller$lagvalue <- mlag(shiller$P.real, nlag = -120)
shiller$spx.rtn.10y <- 100*(shiller$lagvalue/shiller$P.real -1)
#spx.rtn.10y <- 100*(lag(spx,120) / spx - 1)
df <- shiller
# merge 10 year ahead S&P 500 return with CAPE today
#decade.ahead.return.cape <- merge(spx.rtn.10y,shiller$CAPE)
#decade.ahead.return.cape <- decade.ahead.return.cape[!is.na(apply(decade.ahead.return.cape,1,sum)),]
names(df)
#df <- data.frame(decade.ahead.return.cape)
#n <- nrow(df)
cape.now <-as.numeric(shiller$CAPE[nrow(shiller)])
ggplot(df,aes(x=log(CAPE),y=spx.rtn.10y)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x, size = 1) +
  #coord_trans(x="log2") +
  theme_bw() +
  geom_vline(xintercept = log(as.numeric(shiller$CAPE[nrow(shiller)])),colour="red") +
  geom_hline(yintercept = 0, linetype = "dashed")+
  #annotate(geom = "text",x = cape.now,y=100,label=paste("CAPE Now",cape.now),angle=-90,vjust=-0.5) +
  ggtitle(paste("CAPE now",cape.now))+
  xlab("Shiller Cyclically Adjusted Price Earnings") +
  ylab("S&P 500 Return Over Following Decade %") +
  theme(legend.position="none")

getSymbols(c("GDP", "CP"), src = "FRED")
prof <- merge(GDP,CP)
prof$divisor <- prof$CP/prof$GDP

prof <- data.frame(prof)
prof$Date <- as.Date(row.names(prof))
df$Date <- as.Date(row.names(df))

prof_1 <- prof[rep(1:nrow(prof),each=3),] 

for(i in 2:nrow(prof_1)){
  if(prof_1$Date[i] == prof_1$Date[i-1]){
    prof_1$Date[i] <- prof_1$Date[i] %m+% months(1)
  }
}

for(i in 3:nrow(prof_1)){
  if(prof_1$Date[i] == prof_1$Date[i-2]){
    prof_1$Date[i] <- prof_1$Date[i] %m+% months(2)
  }
}


df <- merge(prof_1, df, by = "Date")
df$other <- df$CAPE/df$divisor

ggplot(df,aes(x=log(other),y=spx.rtn.10y)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x, size = 1) +
  #coord_trans(x="log2") +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed")+
  #geom_vline(xintercept = as.numeric(shiller$CAPE[nrow(shiller)]),colour="red") +
  #annotate(geom = "text",x = cape.now,y=100,label=paste("CAPE Now",cape.now),angle=-90,vjust=-0.5) +
  xlab("Shiller Cyclically Adjusted Price Earnings") +
  ylab("S&P 500 Return Over Following Decade %") +
  theme(legend.position="none")

