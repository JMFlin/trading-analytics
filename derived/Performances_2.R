library(quantmod)   
library(lubridate)
library(ggplot2)
library(gridExtra)
Sys.setlocale("LC_TIME", "C")

getSymbols("SPY", from = "2017-01-01")

SPY <- SPY[index(SPY) >= as.Date(cut(Sys.Date(), "year")),]

this_year <- c(1:nrow(SPY))[length(c(1:nrow(SPY)))]

browseURL("https://stockcharts.com/freecharts/candleglance.html?[US]")

browseURL(paste0("http://stockcharts.com/freecharts/perf.php?$SPX,$INDU,$NDX,$MID,$SML,$RUT,$DWMI&n=",this_year,"&O=111000"))
browseURL(paste0("http://stockcharts.com/freecharts/perf.php?QQEW,QQQ,OEF,SPY,RSP,DIA,IWM,MDY,IJR&n=",this_year,"&O=111000"))
browseURL(paste0("http://stockcharts.com/freecharts/perf.php?SPY,XLY,XLF,XLI,XLK,XLB,XLP,XLE,XLV,XLU,IYZ&n=",this_year,"&O=111000"))


#At the year's mid-point, SPX is up 2.5%, NDX is up 10% and small caps are up 7%. The Dow is down almost 2%. 
#Part of these results are explained by the upward bias in the dollar, 
#which favors domestic-focused small caps relative to internationally-weighted large caps. 

this_month <- c(1:nrow(SPY[month(index(SPY)) == month(Sys.Date()),]))[length(c(1:nrow(SPY[month(index(SPY)) == month(Sys.Date()),])))]

browseURL(paste0("http://stockcharts.com/freecharts/perf.php?$SPX,$INDU,$NDX,$MID,$SML,$RUT,$DWMI&n=",this_month,"&O=111000"))
browseURL(paste0("http://stockcharts.com/freecharts/perf.php?QQEW,QQQ,OEF,SPY,RSP,DIA,IWM,MDY,IJR&n=",this_month,"&O=111000"))
browseURL(paste0("http://stockcharts.com/freecharts/perf.php?SPY,XLY,XLF,XLI,XLK,XLB,XLP,XLE,XLV,XLU,IYZ&n=",this_month,"&O=111000"))


browseURL("http://stockcharts.com/freecharts/rrg/?s=$dwmi,$INDU,$MID,$NDX,$RUT,$SML,$UTIL&b=$SPX&p=d&y=1&t=6&f=tail,d")
browseURL("http://stockcharts.com/freecharts/rrg/?s=FXY,IEI,GLD,TLT&b=$SPX&p=d&y=1&t=6&f=tail,d")
browseURL("http://stockcharts.com/freecharts/rrg/?s=XLB,XLE,XLF,XLI,XLK,XLP,XLU,XLV,XLY,IYZ&b=$SPX&p=d&y=1&t=6&f=tail,d")
browseURL("http://stockcharts.com/freecharts/rrg/?s=HYG,MTUM,SPHB,SPLV,DVY&b=$SPX&p=d&y=1&t=6&f=tail,d")

browseURL("http://stockcharts.com/freecharts/yieldcurve.php")



#SP500
browseURL("http://www.indexindicators.com/charts/sp500-vs-sp500-stocks-at-20d-highs-params-8m-x-x-x-x/")

browseURL("http://www.indexindicators.com/charts/sp500-vs-sp500-stocks-above-200d-sma-params-8m-x-x-x-x/")
browseURL("http://www.indexindicators.com/charts/sp500-vs-sp500-stocks-above-50d-sma-params-8m-x-x-x-x/")
browseURL("http://www.indexindicators.com/charts/sp500-vs-sp500-stocks-above-20d-sma-params-8m-x-x-x-x/")


#NYSE
browseURL("http://www.indexindicators.com/charts/nyse-vs-nyse-stocks-above-200d-sma-params-8m-x-x-x-x/")
browseURL("http://www.indexindicators.com/charts/nyse-vs-nyse-stocks-above-50d-sma-params-8m-x-x-x-x/")
browseURL("http://www.indexindicators.com/charts/nyse-vs-nyse-stocks-above-20d-sma-params-8m-x-x-x-x/")

#SP600 smallcap
browseURL("http://www.indexindicators.com/charts/sp600-vs-sp600-stocks-at-20d-highs-params-8m-x-x-x-x/")

browseURL("http://www.indexindicators.com/charts/sp600-vs-sp600-stocks-above-200d-sma-params-8m-x-x-x-x/")
browseURL("http://www.indexindicators.com/charts/sp600-vs-sp600-stocks-above-50d-sma-params-8m-x-x-x-x/")
browseURL("http://www.indexindicators.com/charts/sp600-vs-sp600-stocks-above-20d-sma-params-8m-x-x-x-x/")

#SP400 midcap
browseURL("http://www.indexindicators.com/charts/sp400-vs-sp400-stocks-at-20d-highs-params-8m-x-x-x-x/")

browseURL("http://www.indexindicators.com/charts/sp400-vs-sp400-stocks-above-200d-sma-params-8m-x-x-x-x/")
browseURL("http://www.indexindicators.com/charts/sp400-vs-sp400-stocks-above-50d-sma-params-8m-x-x-x-x/")
browseURL("http://www.indexindicators.com/charts/sp400-vs-sp400-stocks-above-20d-sma-params-8m-x-x-x-x/")

#Nasdaq100
browseURL("http://www.indexindicators.com/charts/nasdaq100-vs-nasdaq100-stocks-at-20d-highs-params-8m-x-x-x-x/")

browseURL("http://www.indexindicators.com/charts/nasdaq100-vs-nasdaq100-stocks-above-200d-sma-params-8m-x-x-x-x/")
browseURL("http://www.indexindicators.com/charts/nasdaq100-vs-nasdaq100-stocks-above-50d-sma-params-8m-x-x-x-x/")
browseURL("http://www.indexindicators.com/charts/nasdaq100-vs-nasdaq100-stocks-above-20d-sma-params-8m-x-x-x-x/")

#SP100
browseURL("http://www.indexindicators.com/charts/sp100-vs-sp100-stocks-at-20d-highs-params-8m-x-x-x-x/")

browseURL("http://www.indexindicators.com/charts/sp100-vs-sp100-stocks-above-200d-sma-params-8m-x-x-x-x/")
browseURL("http://www.indexindicators.com/charts/sp100-vs-sp100-stocks-above-50d-sma-params-8m-x-x-x-x/")
browseURL("http://www.indexindicators.com/charts/sp100-vs-sp100-stocks-above-20d-sma-params-8m-x-x-x-x/")
