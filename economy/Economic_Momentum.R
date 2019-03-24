glibrary(quantmod)
library(ggplot2)
library(scales)
library(reshape)
library(PerformanceAnalytics)
library(lubridate)
library(gridExtra)
library(Quandl)
library(yaml)

keys <- read_yaml("C:/Users/janne/Documents/trading-analytics/API_keys.yaml")

Sys.setlocale("LC_TIME", "C")
Quandl.api_key(keys$Quandl)

economic_momentum <- function(data, n_ema = 12, from = "2007-03-01"){
  #data <- data_xts_2
  data_xts <- data.frame(data)
  data_xts <- data_xts[,grep("_delt", names(data_xts))]
  data_xts <- data_xts[as.Date(row.names(data_xts)) >= from,]
  data_xts <- data_xts[2:nrow(data_xts),]
  a <- apply(data_xts[index(data_xts) >= from,], 2, function(col) sum(is.na(col)))
  a <- data.frame(a)
  a$name <- row.names(a)
  b <- a[a$a <= 1,"name"]
  data_xts <- data_xts[,names(data_xts) %in% b]

  data_xts_clone <- data_xts

  for(i in 1:ncol(data_xts)){
    for(j in 4:nrow(data_xts)){

      if(!is.na(data_xts[j,i]) & !is.na(data_xts[nrow(data_xts),i])){
        if(data_xts[j,i] >= data_xts[j-3,i]){
          data_xts_clone[j,i] = 1
        }else{
          data_xts_clone[j,i] = 0
        }
      }
    }
  }

  for(i in 1:ncol(data_xts)){
    for(j in 4:(nrow(data_xts)-1)){

      if(!is.na(data_xts[j,i]) & is.na(data_xts[nrow(data_xts),i])){
        if(data_xts[j,i] >= data_xts[j-3,i]){
          data_xts_clone[j,i] = 1
        }else{
          data_xts_clone[j,i] = 0
        }
      }
    }
  }

  #sum(colSums(is.na(data_xts_clone)) > 0)
  data_xts_clone$percent <- rowSums(data_xts_clone, na.rm = TRUE)/ncol(data_xts_clone)
  #data_xts_clone$percent[nrow(data_xts_clone)] <- rowSums(data_xts_clone[nrow(data_xts_clone),1:(ncol(data_xts_clone)-1)], na.rm = TRUE)/sum(colSums(is.na(data_xts_clone)) > 0)
  data_xts_clone$percent <- data_xts_clone$percent*100
  data_xts_clone <- data_xts_clone[4:nrow(data_xts_clone),]
  data_xts_clone$Date <- as.Date(row.names(data_xts_clone))
  data_xts_clone$ma <- EMA(data_xts_clone[,"percent"], n=n_ema)

  a <- ggplot(data_xts_clone, aes(x=Date, y=percent))+
    geom_line()+
    geom_point(size = 1)+
    geom_line(aes(y=ma, x=Date), col = "red", alpha = 0.7) +
    geom_point(data=data_xts_clone[nrow(data_xts_clone),], aes(x=Date, y=percent), col = "blue", alpha = 0.7)+
    geom_hline(yintercept = 50, linetype = "longdash", alpha = 0.7)+
    ylab("")+
    xlab("")+
    theme_bw()+
    ggtitle("")+
    scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
    theme(axis.line = element_line(),
          axis.text=element_text(color='black'),
          axis.title = element_text(colour = 'black'),
          legend.text=element_text(),
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))

  data_xts_clone <- data_xts

  for(i in 1:ncol(data_xts)){
    for(j in 7:nrow(data_xts)){

      if(!is.na(data_xts[j,i]) & !is.na(data_xts[nrow(data_xts),i])){
        if(data_xts[j,i] >= data_xts[j-6,i]){
          data_xts_clone[j,i] = 1
        }else{
          data_xts_clone[j,i] = 0
        }
      }
    }
  }

  for(i in 1:ncol(data_xts)){
    for(j in 7:(nrow(data_xts)-1)){

      if(!is.na(data_xts[j,i]) & is.na(data_xts[nrow(data_xts),i])){
        if(data_xts[j,i] >= data_xts[j-6,i]){
          data_xts_clone[j,i] = 1
        }else{
          data_xts_clone[j,i] = 0
        }
      }
    }
  }

  data_xts_clone$percent <- rowSums(data_xts_clone, na.rm = TRUE)/ncol(data_xts_clone)
  #data_xts_clone$percent[nrow(data_xts_clone)] <- rowSums(data_xts_clone[nrow(data_xts_clone),1:(ncol(data_xts_clone)-1)], na.rm = TRUE)/sum(colSums(is.na(data_xts_clone)) > 0)
  data_xts_clone$percent <- data_xts_clone$percent*100
  data_xts_clone <- data_xts_clone[7:nrow(data_xts_clone),]
  data_xts_clone$Date <- as.Date(row.names(data_xts_clone))
  data_xts_clone$ma <- EMA(data_xts_clone[,"percent"], n=n_ema)

  b <- ggplot(data_xts_clone, aes(x=Date, y=percent))+
    geom_line()+
    geom_point(size = 1)+
    geom_line(aes(y=ma, x=Date), col = "red", alpha = 0.7) +
    geom_point(data=data_xts_clone[nrow(data_xts_clone),], aes(x=Date, y=percent), col = "blue", alpha = 0.7)+
    ylab("")+
    xlab("")+
    geom_hline(yintercept = 50, linetype = "longdash", alpha = 0.7)+
    theme_bw()+
    ggtitle("")+
    scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
    theme(axis.line = element_line(),
          axis.text=element_text(color='black'),
          axis.title = element_text(colour = 'black'),
          legend.text=element_text(),
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))

  data_xts_clone <- data_xts

  for(i in 1:ncol(data_xts)){
    for(j in 13:nrow(data_xts)){

      if(!is.na(data_xts[j,i]) & !is.na(data_xts[nrow(data_xts),i])){
        if(data_xts[j,i] >= data_xts[j-12,i]){
          data_xts_clone[j,i] = 1
        }else{
          data_xts_clone[j,i] = 0
        }
      }
    }
  }

  for(i in 1:ncol(data_xts)){
    for(j in 13:(nrow(data_xts)-1)){

      if(!is.na(data_xts[j,i]) & is.na(data_xts[nrow(data_xts),i])){
        if(data_xts[j,i] >= data_xts[j-12,i]){
          data_xts_clone[j,i] = 1
        }else{
          data_xts_clone[j,i] = 0
        }
      }
    }
  }

  data_xts_clone$percent <- rowSums(data_xts_clone, na.rm = TRUE)/ncol(data_xts_clone)
  #data_xts_clone$percent[nrow(data_xts_clone)] <- rowSums(data_xts_clone[nrow(data_xts_clone),1:(ncol(data_xts_clone)-1)], na.rm = TRUE)/sum(colSums(is.na(data_xts_clone)) > 0)
  data_xts_clone$percent <- data_xts_clone$percent*100
  data_xts_clone <- data_xts_clone[13:nrow(data_xts_clone),]
  data_xts_clone$Date <- as.Date(row.names(data_xts_clone))
  data_xts_clone$ma <- EMA(data_xts_clone[,"percent"], n=n_ema)

  c <- ggplot(data_xts_clone, aes(x=Date, y=percent))+
    geom_line()+
    geom_point()+
    geom_line(aes(y=ma, x=Date), col = "red", alpha = 0.7) +
    geom_point(data=data_xts_clone[nrow(data_xts_clone),], aes(x=Date, y=percent), col = "blue", alpha = 0.7)+
    geom_hline(yintercept = 50, linetype = "longdash", alpha = 0.7)+
    #geom_text(
    #  data=data_xts_clone[nrow(data_xts_clone),],
    #  aes(label=percent,
    #      x=Date, y=percent), vjust = -0.2,size = 3.0)+
    ylab("")+
    xlab("")+
    theme_bw()+
    ggtitle(paste("Percentage of Data Accelerating as of", Sys.Date(), "for", month(data_xts_clone$Date[nrow(data_xts_clone)], label = TRUE, abbr = FALSE)))+
    #ggtitle(paste("Percentage of Data Accelerating as of", Sys.Date(), paste0("(",data_xts_clone$Date[nrow(data_xts_clone)],")")))+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    theme(axis.line = element_line(),
          axis.text=element_text(color='black'),
          axis.title = element_text(colour = 'black'),
          legend.text=element_text(),
          legend.title=element_text(),
          axis.text.x = element_text(angle = 0),
          legend.position='none',
          text = element_text(size=10))

  grid.arrange(c, heights = c(2.5/4, 1.5/4), ncol = 1, arrangeGrob(a, b, ncol = 2))
  tmp <- data.frame(apply(data_xts, 2, function(x) length(which(!is.na(x)))))

  print(paste("Latest observation uses", sum(tmp[,1] == nrow(data_xts))-3, "of the possible", ncol(data_xts)-3,"time series"))
}

data_load_fed_monthly <- function(){
  getSymbols(c("PCEC96", # real PCE
               "DSPIC96", # real disposable income
               "CUUR0000SEHA", #rent inflation
               "CES0500000003",
               "PAYEMS", #nonfarm payroll
               "AWHAETP",
               "PI", #personal income
               "RRSFS", #real retail food
               "RSXFS", #total retail
               "CSUSHPINSA", #case shiller national
               "EMRATIO", #emp to pop to monthly
               "LNS12500000", #work fulltime
               "JTSHIL", #hires
               "JTSJOL", #job openings
               "DGORDER", #manu new orders
               "REALLN", #real estate loans
               "HOUST", #housing starts new total private
               "HOUST1F", # housing 1 unit
               "CES0500000003", #avg hourly earnings
               "CPILFESL", #core cpi
               "SVGCBNS", # savings deposits down is good
               "TCU", #capacity utilization
               "INDPRO", #industrial prod
               "IPMAN", #industrial prod manu
               "UNRATE", #unemp rate
               "AHETPI", #wages
               "ALTSALES", #light vehicles
               "WPSFD41312",
               "NEWORDER",
               "CFNAIMA3",
               "FRBLMCI",
               "PCEPILFE",
               "TOTALSA", #vehicle sales not delt
               "DAUTOSA", #domestic vehicles not delt
               "TTLCONS", #Total Construction Spending delt
               "TLRESCONS", #Total Construction Spending: Residential delt
               "PRMFGCONS", # Total Private Construction Spending: Manufacturing delt
               "PPIACO", #PPI all comm delt
               "WPSFD4131", # PPI finnished core
               "AAA",
               "BAA"),
               src = "FRED", from = "2000-01-01")
  fed_ts <- merge(PCEC96,
        DSPIC96,
        CUUR0000SEHA, #rent inflation
        CES0500000003,
        PAYEMS, #nonfarm payroll
        AWHAETP,
        PI, #personal income
        RRSFS, #real retail food
        RSXFS, #total retail
        CSUSHPINSA, #case shiller national
        EMRATIO, #emp to pop to monthly
        LNS12500000, #work fulltime
        JTSHIL, #hires
        JTSJOL, #job openings
        DGORDER, #manu new orders
        REALLN, #real estate loans
        HOUST, #housing starts new total private
        HOUST1F, # housing 1 unit
        CES0500000003, #avg hourly earnings
        CPILFESL, #core cpi
        SVGCBNS, # savings deposits down is good
        TCU, #capacity utilization
        INDPRO, #industrial prod
        IPMAN, #indpro manu
        UNRATE, #unemp rate
        AHETPI, #wages
        ALTSALES, #light vehicles
        NEWORDER/WPSFD41312,
        CFNAIMA3,
        FRBLMCI,
        PCEPILFE,
        TOTALSA, #vehicle sales not delt
        DAUTOSA, #domestic vehicles not delt
        TTLCONS, #Total Construction Spending delt
        TLRESCONS, #Total Construction Spending: Residential delt
        PRMFGCONS, # Total Private Construction Spending: Manufacturing delt
        PPIACO, #PPI all comm delt
        WPSFD4131, # PPI finnished core
        AAA,
        BAA)

  return(fed_ts)
}

data_load_fed_weekly <- function(){
  getSymbols(c("NFCI", #chicago financial stress to monthly
                           "ANFCI", #chicago conditions to monthly
                           "KCFSI", #kansas
                           "STLFSI", #st loius to monthly
                           "TEDRATE", #TED to monthly
                           "BAMLH0A0HYM2", #hy option spread higher is worse!
                           "BAMLHYH0A0HYM2TRIV", #hy junk index, high better
                           "CCSA", #4 week ma of claims
                           "TOTCI",
               "COMPAPER",
                "BAMLHE00EHYIEY",
               "DGS10"), src = "FRED", from = "2000-01-01")

  NFCI <- to.monthly(NFCI, OHLC = FALSE)
  ANFCI <- to.monthly(ANFCI, OHLC = FALSE)
  KCFSI <- to.monthly(KCFSI, OHLC = FALSE)
  STLFSI <- to.monthly(STLFSI, OHLC = FALSE)
  TEDRATE <- to.monthly(TEDRATE, OHLC = FALSE)
  BAMLH0A0HYM2 <- to.monthly(BAMLH0A0HYM2, OHLC = FALSE)
  BAMLHYH0A0HYM2TRIV <- to.monthly(BAMLHYH0A0HYM2TRIV, OHLC = FALSE)
  CCSA <- to.monthly(CCSA, OHLC = FALSE)
  TOTCI <- to.monthly(TOTCI, OHLC = FALSE)
  COMPAPER <- to.monthly(COMPAPER, OHLC = FALSE)
  BAMLHE00EHYIEY <- to.monthly(BAMLHE00EHYIEY, OHLC = FALSE)
  DGS10 <- to.monthly(DGS10, OHLC = FALSE)
  EUROJUNK <- BAMLHE00EHYIEY/DGS10
  names(EUROJUNK) <- "EUROJUNK"

  fed_ts_data <- merge(NFCI, #chicago financial stress to monthly
        ANFCI, #chicago conditions to monthly
        KCFSI, #kansas
        STLFSI, #st loius to monthly
        TEDRATE, #TED to monthly
        BAMLH0A0HYM2, #hy option spread higher is worse!
        BAMLHYH0A0HYM2TRIV, #hy junk index, high better
        CCSA,
        TOTCI,
        COMPAPER,
        EUROJUNK)

  return(fed_ts_data)
}

data_load_quandl <- function(){
  ism.comp <- Quandl("ISM/MAN_PMI", collapse = "monthly", trim_start = "2000-01-01", type = "xts")
  ism.comp_emp <- Quandl("ISM/MAN_EMPL", collapse = "monthly", trim_start = "2000-01-01", type = "xts")
  ism.comp <- ism.comp[,1]
  ism.comp_emp <- ism.comp_emp[,"Index"]

  USCOIN <- Quandl("ECRI/USCOIN", collapse = "monthly", trim_start = "2000-01-01", type = "xts")
  ECRI <- Quandl("ECRI/USLEADING", collapse = "monthly", trim_start = "2000-01-01", type = "xts")
  USCOIN <- USCOIN[,"Growth"]
  ECRI <- ECRI[,"Growth"]
  ECRI <- to.monthly(ECRI, OHLC = FALSE)

  SPCOMP <- Quandl("YALE/SPCOMP", trim_start = "2000-01-01", type = "xts")
  SP <- Delt(SPCOMP[,"Real Price"], k=(12*5))
  SP_1 <- Delt(SPCOMP[,"Real Price"], k=(12*2))
  names(SP) <- "SP"
  names(SP_1) <- "SP_1"
  data_quandl <- merge(ism.comp, ism.comp_emp, USCOIN, ECRI, SP, SP_1)
  names(data_quandl) <- c("ismp.comp", "ism.comp_emp", "USCOIN", "ECRI", "SP", "SP_1")
  return(data_quandl)
}

data_load_yahoo <- function(){
  getSymbols("^VIX", src = "yahoo", from = "1990-01-01")
  VIX <- to.monthly(VIX, OHLC = FALSE)
  VIX <- VIX*-1
  data_yahoo <- VIX$VIX.Adjusted
  return(data_yahoo)
}

data_prep <- function(data_fed_monthly, data_fed_weekly, data_quandl, data_yahoo, from = "2007-03-01"){

  data_xts <- merge(data_fed_monthly, data_fed_weekly, data_quandl, data_yahoo)
  data_xts <- data_xts[1:(nrow(data_xts)-1),]

  a <- apply(data_xts[index(data_xts) >= from,], 2, function(col) sum(is.na(col)))
  a <- data.frame(a)
  a$name <- row.names(a)
  b <- a[a$a <= 1,"name"]

  data_xts <- data_xts[, names(data_xts) %in% b]

  rem <- ncol(data_xts)

  data_xts$Agg_delt <- Delt(data_xts$CES0500000003*data_xts$PAYEMS*data_xts$AWHAETP, k = 12)
  data_xts$Agg_rent_delt <- data_xts$Agg_delt-Delt(data_xts$CUUR0000SEHA, k=12)

  data_xts$BAA_AAA_delt <- (data_xts$BAA-data_xts$AAA)*-1

  data_xts$NFCI_delt <- data_xts[,"NFCI"]*-1
  data_xts$ANFCI_delt <- data_xts[,"ANFCI"]*-1
  data_xts$KCFSI_delt <- data_xts[,"KCFSI"]*-1
  data_xts$STLFSI_delt <- data_xts[,"STLFSI"]*-1
  data_xts$TEDRATE_delt <- data_xts[,"TEDRATE"]*-1
  data_xts$EUROJUNK_delt  <- data_xts[,"EUROJUNK"]*-1

  data_xts$SVGCBNS_delt <- Delt(data_xts$SVGCBNS, k = 12)*-1
  data_xts$BAMLH0A0HYM2_delt <- Delt(data_xts$BAMLH0A0HYM2, k = 12)*-1
  data_xts$UNRATE_delt <- Delt(data_xts$UNRATE, k = 12)*-1
  data_xts$CCSA_delt <- Delt(data_xts$CCSA, k = 12)*-1

  data_xts$ism.comp_delt <- data_xts$ism.comp
  data_xts$ism.comp_emp_delt <- data_xts$ism.comp_emp
  data_xts$CFNAIMA3_delt <- data_xts$CFNAIMA3
  data_xts$ECRI_delt <- data_xts$ECRI
  data_xts$USCOIN_delt <- data_xts$USCOIN
  data_xts$FRBLMCI_delt <- data_xts$FRBLMCI
  data_xts$SP_delt <- data_xts$SP
  data_xts$SP_1_delt <- data_xts$SP_1
  data_xts$TCU_delt <- data_xts$TCU
  data_xts$VIX_delt <- data_xts$VIX

  data_xts$PPIACO_delt <- Delt(data_xts$PPIACO, k = 12) #PPI all comm delt
  data_xts$WPSFD4131_delt <- Delt(data_xts$WPSFD4131, k = 12) # PPI finnished core
  #data_xts$TTLCONS_delt <- Delt(data_xts$TTLCONS, k = 12) #Total Construction Spending delt
  #data_xts$TLRESCONS_delt <- Delt(data_xts$TLRESCONS, k = 12) #Total Construction Spending: Residential delt
  #data_xts$PRMFGCONS_delt <- Delt(data_xts$PRMFGCONS, k = 12) # Total Private Construction Spending: Manufacturing delt
  data_xts$TOTALSA_delt <- Delt(data_xts$TOTALSA, k = 12) #vehicle sales not delt
  data_xts$DAUTOSA_delt <- Delt(data_xts$DAUTOSA, k = 12) #domestic vehicles not delt
  data_xts$EMRATIO_delt <- Delt(data_xts$EMRATIO, k = 12) #emp to pop to monthly
  data_xts$LNS12500000_delt <- Delt(data_xts$LNS12500000, k = 12) #work fulltime
  data_xts$payems_delt <- Delt(data_xts$PAYEMS, k = 12)
  data_xts$BAMLHYH0A0HYM2TRIV_delt <- Delt(data_xts$BAMLHYH0A0HYM2TRIV, k = 12)
  data_xts$INDPRO_delt <- Delt(data_xts$INDPRO, k = 12)
  data_xts$IPMAN_delt <- Delt(data_xts$IPMAN, k = 12)
  data_xts$RRSFS_delt <- Delt(data_xts$RRSFS, k = 12)
  data_xts$RSXFS_delt <- Delt(data_xts$RSXFS, k = 12)
  data_xts$AHETPI_delt <- Delt(data_xts$AHETPI, k = 12)
  data_xts$ALTSALES_delt <- Delt(data_xts$ALTSALES, k = 12)
  data_xts$REALLN_delt <- Delt(data_xts$REALLN , k = 12)
  data_xts$e_delt <- Delt(data_xts$HOUST, k = 12) #housing starts new total private
  data_xts$f_delt <- Delt(data_xts$HOUST1F, k = 12) # housing 1 unit
  data_xts$h_delt <- Delt(data_xts$CES0500000003, k = 12) #avg hourly earnings
  data_xts$CPILFESL_delt <- Delt(data_xts$CPILFESL, k = 12)
  data_xts$TOTCI_delt <- Delt(data_xts$TOTCI, k = 12)
  data_xts$COMPAPER_delt <- Delt(data_xts$COMPAPER, k = 12)
  data_xts$AHETPI_delt_inf <- data_xts$AHETPI_delt-Delt(data_xts$CUUR0000SEHA, k=12)
  data_xts$h_delt_inf <- data_xts$h_delt-Delt(data_xts$CUUR0000SEHA, k=12)


  return(data_xts)
}

data_prep_2 <- function(data_fed_monthly, data_fed_weekly, data_quandl, data_yahoo, from = "2007-03-01"){

  data_xts <- merge(data_fed_monthly, data_fed_weekly, data_quandl, data_yahoo)
  data_xts <- data_xts[, names(data_xts) %in% c("NFCI","ANFCI","STLFSI","TEDRATE","BAMLH0A0HYM2","BAMLHYH0A0HYM2TRIV","CCSA","TOTCI","COMPAPER","EUROJUNK","ismp.comp","ism.comp_emp","ECRI","SP","SP_1","VIX.Adjusted")]

  data_xts <- data_xts[1:(nrow(data_xts)-1),]

  a <- apply(data_xts[index(data_xts) >= from,], 2, function(col) sum(is.na(col)))
  a <- data.frame(a)
  a$name <- row.names(a)
  b <- a[a$a <= 1,"name"]

  data_xts <- data_xts[, names(data_xts) %in% b]

  data_xts$NFCI_delt <- data_xts[,"NFCI"]*-1
  data_xts$ANFCI_delt <- data_xts[,"ANFCI"]*-1
  data_xts$KCFSI_delt <- data_xts[,"KCFSI"]*-1
  data_xts$STLFSI_delt <- data_xts[,"STLFSI"]*-1
  data_xts$TEDRATE_delt <- data_xts[,"TEDRATE"]*-1
  data_xts$EUROJUNK_delt  <- data_xts[,"EUROJUNK"]*-1


  data_xts$BAMLH0A0HYM2_delt <- Delt(data_xts$BAMLH0A0HYM2, k = 12)*-1

  data_xts$CCSA_delt <- Delt(data_xts$CCSA, k = 12)*-1

  data_xts$ism.comp_delt <- data_xts$ism.comp
  data_xts$ism.comp_emp_delt <- data_xts$ism.comp_emp
  data_xts$CFNAIMA3_delt <- data_xts$CFNAIMA3
  data_xts$ECRI_delt <- data_xts$ECRI
  data_xts$USCOIN_delt <- data_xts$USCOIN
  data_xts$FRBLMCI_delt <- data_xts$FRBLMCI
  data_xts$SP_delt <- data_xts$SP
  data_xts$SP_1_delt <- data_xts$SP_1
  data_xts$TCU_delt <- data_xts$TCU
  data_xts$VIX_delt <- data_xts$VIX

  data_xts$BAMLHYH0A0HYM2TRIV_delt <- Delt(data_xts$BAMLHYH0A0HYM2TRIV, k = 12)

  data_xts$TOTCI_delt <- Delt(data_xts$TOTCI, k = 12)
  data_xts$COMPAPER_delt <- Delt(data_xts$COMPAPER, k = 12)

  return(data_xts)
}
#Check periods of data loaded at end of month

##
#Do a correlation check!!!!
##

data_quandl <- data_load_quandl()
tail(data_quandl)
data_fed_monthly <- data_load_fed_monthly()
tail(data_fed_monthly)
data_fed_weekly <- data_load_fed_weekly()
tail(data_fed_weekly)
data_yahoo <- data_load_yahoo()
tail(data_yahoo)
data_xts <- data_prep(data_fed_monthly, data_fed_weekly, data_quandl, data_yahoo, from = "2007-03-01")
data_xts_2 <- data_prep_2(data_fed_monthly, data_fed_weekly, data_quandl, data_yahoo, from = "2007-03-01")

economic_momentum(data_xts, n_ema = 12, from = "2007-03-01") # "2007-03-01"
economic_momentum(data_xts, n_ema = 6, from = "2007-03-01")
economic_momentum(data_xts_2, n_ema = 12, from = "2007-03-01") # "2007-03-01"
economic_momentum(data_xts_2, n_ema = 6, from = "2007-03-01")
#https://seekingalpha.com/article/4105962-interest-rates-will-rise


#Update by end of month:
#  "NFCI"               "ANFCI"              "STLFSI"             "TEDRATE"            "BAMLH0A0HYM2"
#[6] "BAMLHYH0A0HYM2TRIV" "CCSA"               "TOTCI"              "COMPAPER"           "EUROJUNK"
#[11] "ismp.comp"          "ism.comp_emp"       "ECRI"               "SP"                 "SP_1"
#[16] "VIX.Adjusted"


#------

#All done for monthly
#wages and salaries - rent inflation
#total construction spending
#residential construction spending
#manufacturing construction spending
#total vehicle sales
#US auto sales
#Core factory orders
#construction payrolls
#producer price index
#core producer price index
#industrial production: manufactoring
#new home sales
#new home sales: median sales
#existing home sales
#existing home sales median
#durable goods orders
#core durable goods orders
#core capex orders


#Quarterly
#DRTSCILM loan tight
#DRTSCIS loan tight
#DRBLACBS delinq
#DRCLACBS delinq
#DRALACBS delinq
#CSUSHPINSA/CUSR0000SEHC
#PRFIC96 resid invest
#PNFIC96 nonresid invest
#A261RX1Q020SBEA real gross dom income
#BUSLOANS/GDP
#CP
#CPATAX
#A053RC1Q027SBEA
#CPROFIT
#A446RC1Q027SBEA
#A463RC1Q027SBEA
#A464RC1Q027SBEA

#"FINSLC1", #real final sales

#corp and household balancesheet:
#TABSNNCB
#TNWMVBSNNCB
#TNWBSHNO
#TFAABSHNO

#yield spreds
#DGS10 / GDP # low is good
#DAAA + DBAA / 2 - DGS10 # low is good
#FF-GDP # low is good
#BAA - AA low is good
#MORTGAGE30US-DGS30 # low is good

##data_xts$Agg_pce_delt <-  data_xts$Agg_delt-data_xts$PCEPILFE_delt
#data_xts$PCEPILFE_delt <- Delt(data_xts$PCEPILFE, k = 12)
#data_xts$pi_delt <- Delt(data_xts$PI, k = 12)
#data_xts$real_pce_delt <- Delt(data_xts$PCEC96, k = 12)
#data_xts$real_disp_delt <- Delt(data_xts$DSPIC96, k = 12)
#data_xts$CSUSHPINSA_delt <- Delt(data_xts$CSUSHPINSA, k = 12) #case shiller national
#data_xts$a_delt <- Delt(data_xts$JTSHIL, k = 12) #hires
#data_xts$b_delt <- Delt(data_xts$JTSJOL, k = 12) #job openings
#data_xts$c_delt <- Delt(data_xts$DGORDER, k = 12) #manu new orders
#data_xts$TOTCI_delt <- Delt(data_xts$TOTCI, k = 12)
#data_xts$NEWORDER_delt <- Delt(data_xts$NEWORDER, k = 12)
#emp to vacancy!
#YALE/SPCOMP
#NHFSEPC #new houses sold:completed

#PI
#RPI

#MSPNHSUS

#REALLN

#data_xts$TTLCONS_delt <- Delt(data_xts$TTLCONS, k = 12) #Total Construction Spending delt
#data_xts$TLRESCONS_delt <- Delt(data_xts$TLRESCONS, k = 12) #Total Construction Spending: Residential delt
#data_xts$PRMFGCONS_delt <- Delt(data_xts$PRMFGCONS, k = 12) # Total Private Construction Spending: Manufacturing delt


#data_xts$Agg_pce_delt <-  data_xts$Agg_delt-data_xts$PCEPILFE_delt
#data_xts$PCEPILFE_delt <- Delt(data_xts$PCEPILFE, k = 12)
#data_xts$pi_delt <- Delt(data_xts$PI, k = 12)
#data_xts$real_pce_delt <- Delt(data_xts$PCEC96, k = 12)
#data_xts$real_disp_delt <- Delt(data_xts$DSPIC96, k = 12)
#data_xts$NEWORDER_delt <- Delt(data_xts$NEWORDER, k = 12)
#data_xts$CSUSHPINSA_delt <- Delt(data_xts$CSUSHPINSA, k = 12) #case shiller national
#data_xts$a_delt <- Delt(data_xts$JTSHIL, k = 12) #hires
#data_xts$b_delt <- Delt(data_xts$JTSJOL, k = 12) #job openings
#data_xts$c_delt <- Delt(data_xts$DGORDER, k = 12) #manu new orders
