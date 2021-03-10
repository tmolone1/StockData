rm(list=ls())
symbols<-c("AAL", "FEYE", "FMC", "LOW", "NEE", "QQQ", "QRVO")
strategy<-c("long call spread", "back ratio call spread", "front_ratio_call_spread", "long call", "bull put spread", "short call spread", "diagonal call spread")
Active_Trades<-tibble(symbols, strategy)
Active_Trades[6,]
source("./scripts/ActiveTrades/QQQ_Short_Call_Spread.R")
