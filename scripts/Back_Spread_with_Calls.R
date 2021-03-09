rm(list=ls())
library(quantmod)
library(tidyverse)
library(lubridate)
symb<-"IWM"
exp<-"2021-05-21"
chain<-getOptionChain(symb, Exp = exp)
quote<-getQuote(symb)
A<-220
B<-235
posn_size<-100

strategy<-"Back Spread with Calls"
calls<-chain[["calls"]] 
sell_price<-as.numeric(calls %>% filter(Strike == A) %>% select(Bid))
buy_price<-as.numeric(calls %>% filter(Strike == B) %>% select(Ask))
net_quote<-sell_price-2*buy_price
credit_debit<-ifelse(net_quote>=0, "credit", "debit")
net_credit<-net_quote*posn_size
max_risk<-(-(B-A)*posn_size)+net_credit
net_credit_risk_ratio<-net_credit/-max_risk
sold_prem_pct<-net_quote/quote$Last
spread_midpoint<-A+(B-A)/2
trade_value_at_midpoint<-(-(spread_midpoint-A)*posn_size)+net_credit

days_to_exp<-as.period(lubridate::interval(Sys.Date(),as.Date(exp)),unit = "day")
days_to_exp<-ceiling(days_to_exp@day*5/7)
ATM_IV<-as.numeric(head(arrange(calls,desc(Vol)) %>% select(IV),1))
stop_loss_est<-quote$Last-(2*net_quote)/-.3

prices<-c(A*.8,A,spread_midpoint,B,B*1.2)
profit_loss<-c(rep(net_credit,2),trade_value_at_midpoint,max_risk,-(B*1.2-A)*posn_size+(B*1.2-B)*posn_size*2+net_credit)
lm1<-lm(prices[2:4]~profit_loss[2:4])
BEP1<-as.numeric(lm1[["coefficients"]][1])
lm2<-lm(prices[4:5]~profit_loss[4:5])
BEP2<-as.numeric(lm2[["coefficients"]][1])
target<-(1+ATM_IV)*quote$Last 
upside_implied_profit_potential<-(target-A)*-posn_size+(target-B)*2*posn_size+net_credit
upside_reward_risk<-upside_implied_profit_potential/-max_risk
plot(prices,profit_loss,type="o")
lines(c(A*.8,B*1.2),c(0,0))


writeLines(c(paste0("Net Credit: ", net_credit),
             paste0("Days to Expiry: ", days_to_exp),
             paste0("Net Credit/Risk: ", round(net_credit_risk_ratio,2)),
             paste0("Upside Reward/Risk: ", round(upside_reward_risk,2)),
             paste0("Break-even Points: ", BEP1, " ", BEP2),
             paste0("Generally close these trades with around 25-30 days to expiry")),
           con = stdout(), sep = "\n", useBytes = FALSE)




