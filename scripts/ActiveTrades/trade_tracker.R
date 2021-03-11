rm(list=ls())
library(tidyverse)
i<-6
for (i in 2:10) {
symbols<-c("LOW", "FMC", "DOCU","XRT","AAL", "NEE", "QQQ", "QRVO", "SPY", "IWM")
strategy<-c("long_call", "front_spread_with_calls","bull_put_spread","short_call_spread","long_call_spread", "bull_put_spread", "short_call_spread", "diagonal_call_spread", "skip_strike_call_butterfly", "back_spread_with_calls")
Active_Trades<-tibble(symbols, strategy, basis=0, profit=0)
symb<-Active_Trades[i,]$symbols
strat<-Active_Trades[i,]$strategy
writeLines(c(symb,strat))
source(paste0("./scripts/ActiveTrades/",symb,"_",strat,".R"))
writeLines(" ")
}
