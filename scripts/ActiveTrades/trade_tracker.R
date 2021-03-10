rm(list=ls())
i<-6
for (i in 5:8) {
symbols<-c("AAL", "FEYE", "FMC", "LOW", "NEE", "QQQ", "QRVO", "SPY")
strategy<-c("long_call_spread", "back_ratio_call_spread", "front_ratio_call_spread", "long_call", "bull_put_spread", "short_call_spread", "diagonal_call_spread", "skip_strike_call_butterfly")
Active_Trades<-tibble(symbols, strategy)
symb<-Active_Trades[i,]$symbols
strat<-Active_Trades[i,]$strategy
writeLines(c(symb,strat))
source(paste0("./scripts/ActiveTrades/",symb,"_",strat,".R"))
}
