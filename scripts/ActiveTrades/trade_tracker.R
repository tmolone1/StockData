rm(list=ls())
library(tidyverse)
i<-6
for (i in 3:8) {
symbols<-c("FMC", "LOW", "XRT","AAL", "NEE", "QQQ", "QRVO", "SPY")
strategy<-c("front_ratio_call_spread", "long_call", "short_call_spread","long_call_spread", "bull_put_spread", "short_call_spread", "diagonal_call_spread", "skip_strike_call_butterfly")
Active_Trades<-tibble(symbols, strategy)
symb<-Active_Trades[i,]$symbols
strat<-Active_Trades[i,]$strategy
writeLines(c(symb,strat))
source(paste0("./scripts/ActiveTrades/",symb,"_",strat,".R"))
writeLines(" ")
}
