rm(list=ls())
library(quantmod)
library(tidyverse)
library(lubridate)
symb<-"SPY"
exp<-"2021-04-16"
chain<-getOptionChain(symb, Exp = exp)
quote<-getQuote(symb)
width<-4
A<-387
B<-A+width
C<-B+width
D<-C+width
posn_size<-200

strategy<-"Skip Strike Butterfly with Calls"
calls<-chain[["calls"]] 
A_price<-as.numeric(calls %>% filter(Strike == A) %>% select(Ask))
B_price<-as.numeric(calls %>% filter(Strike == B) %>% select(Bid))
C_price<-as.numeric(calls %>% filter(Strike == C) %>% select(Bid))
D_price<-as.numeric(calls %>% filter(Strike == D) %>% select(Ask))
net_quote<-2*B_price-A_price-D_price
credit_debit<-ifelse(net_quote>=0, "credit", "debit")
net_credit<-net_quote*posn_size
B_strike_profit_potential<-(B-A)*posn_size+net_credit
max_risk<-(-(D-C)*posn_size)+net_credit
reward_risk_ratio<-B_strike_profit_potential/-max_risk
sold_prem_pct<-net_quote/quote$Last

days_to_exp<-as.period(lubridate::interval(Sys.Date(),as.Date(exp)),unit = "day")
days_to_exp<-ceiling(days_to_exp@day*5/7)
ATM_IV<-as.numeric(head(arrange(calls,desc(Vol)) %>% select(IV),1))


prices<-c(A*.9,A,B,D,D*1.1)
profit_loss<-c(rep(net_credit,2),B_strike_profit_potential,rep(max_risk,2))
plot(prices,profit_loss,type="o")
lines(c(A*.8,D*1.2),c(0,0))
lm1<-lm(prices[2:3]~profit_loss[2:3])
BEP1<-as.numeric(lm1[["coefficients"]][1])
lm2<-lm(prices[3:4]~profit_loss[3:4])
BEP2<-as.numeric(lm2[["coefficients"]][1])

writeLines(c(paste0("Net Credit: ", net_credit),
             paste0("Days to Expiry: ", days_to_exp),
             paste0("Reward/Risk: ", round(reward_risk_ratio,2)),
             paste0("Break-even Points: ", BEP1, " ", BEP2)),
           con = stdout(), sep = "\n", useBytes = FALSE)




