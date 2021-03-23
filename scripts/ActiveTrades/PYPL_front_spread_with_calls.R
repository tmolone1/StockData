rm(list=setdiff(ls(),c("basis","profit_pct","Active_Trades")))
library(quantmod)
library(tidyverse)
library(lubridate)
symb<-"PYPL"
exp<-"2021-04-16"
chain<-getOptionChain(symb, Exp = exp)
quote<-getQuote(symb)
A<-270
B<-280
posn_size<-100

strategy<-"Front Spread with Calls"
calls<-chain[["calls"]] 
A_price<-as.numeric(calls %>% filter(Strike == A) %>% select(Bid))
B_price<-as.numeric(calls %>% filter(Strike == B) %>% select(Ask))
A_price_bought<-4.13
B_price_sold<-2.55
basis<-(2*B_price_sold-A_price_bought)*-posn_size
net_quote<-2*B_price-A_price
cost_to_close<-net_quote*posn_size
profit_pct<-round((cost_to_close+basis)/basis,3)

profit_potential<-(B-A)*posn_size-basis
close_target_50pct_gains<-basis*-0.5
close_target_80pct_gains<-basis*-0.2
stop_loss<-((B*1.1-B)*2*posn_size-(B*1.1-A)*posn_size)+basis  # 10% upward move past B strike level

days_to_exp<-as.period(lubridate::interval(Sys.Date(),as.Date(exp)),unit = "day")
days_to_exp<-ceiling(days_to_exp@day*5/7)
ATM_IV<-as.numeric(head(arrange(calls,desc(Vol)) %>% select(IV),1))

prices<-c(A*.85,A,B,B*1.15)
profit_loss<-c(rep(-basis,2),(B-A)*posn_size-basis,(((B*1.15-B)*2*posn_size-(B*1.15-A)*posn_size)+basis)*-1)
plot(prices,profit_loss,type="o", main=c(symb,strategy))
lines(c(A*.85,B*1.15),c(0,0))
lines(rep(quote$Last,2), range(profit_loss), col="pink")
lm1<-lm(prices[3:4]~profit_loss[3:4])
BEP<-as.numeric(lm1[["coefficients"]][1])

writeLines(c(paste0("Symbol quote ", quote$Last),
             paste0("Current Profit ", profit_pct),
             paste0("Days to Expiry: ", days_to_exp),
             paste0("ATM IV: ", ATM_IV),
             paste0("Current Cost to Close: ", cost_to_close),
             paste0("Break-even Point: ", BEP),
             paste0("Suggested stop loss cost to close: ", round(stop_loss,2)),
             paste0("Suggested profit target as 80% maximum gain: ", (profit_potential)*.8),
             paste0("Suggested profit target net quote: ", ((profit_potential)*.8)/-posn_size)),
           con = stdout(), sep = "\n", useBytes = FALSE)




