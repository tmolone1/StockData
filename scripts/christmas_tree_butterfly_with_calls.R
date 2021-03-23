rm(list=ls())
library(quantmod)
library(tidyverse)
library(lubridate)
symb<-"AAPL"
exp<-"2021-04-16"
chain<-getOptionChain(symb, Exp = exp)
quote<-getQuote(symb)
width<-10
A<-115
B<-A+width
C<-B+width
D<-C+width
posn_size<-100

strategy<-"Christmas Tree Butterfly with Calls"
calls<-chain[["calls"]] 
A_price<-as.numeric(calls %>% filter(Strike == A) %>% select(Ask))
B_price<-as.numeric(calls %>% filter(Strike == B) %>% select(Bid))
C_price<-as.numeric(calls %>% filter(Strike == C) %>% select(Bid))
D_price<-as.numeric(calls %>% filter(Strike == D) %>% select(Ask))
net_quote<-3*C_price-A_price-2*D_price
credit_debit<-ifelse(net_quote>=0, "credit", "debit")
net_credit<-net_quote*posn_size
C_strike_profit_potential<-(C-A)*posn_size+net_credit
reward_risk_ratio<-C_strike_profit_potential/-net_credit
B_strike_reward_risk_ratio<-((B-A)*posn_size+net_credit)/-net_credit
sold_prem_pct<-C_price/quote$Last

days_to_exp<-as.period(lubridate::interval(Sys.Date(),as.Date(exp)),unit = "day")
days_to_exp<-ceiling(days_to_exp@day*5/7)
ATM_IV<-as.numeric(head(arrange(calls,desc(Vol)) %>% select(IV),1))


prices<-c(A*.8,A,C,D,D*1.2)
profit_loss<-c(rep(net_credit,2),C_strike_profit_potential,rep(net_credit,2))
plot(prices,profit_loss,type="o")
lines(c(A*.8,D*1.2),c(0,0))
lines(rep(quote$Last,2),range(profit_loss),col="pink")
lm1<-lm(prices[2:3]~profit_loss[2:3])
BEP1<-as.numeric(lm1[["coefficients"]][1])
lm2<-lm(prices[3:4]~profit_loss[3:4])
BEP2<-as.numeric(lm2[["coefficients"]][1])

writeLines(c(paste0("Symbol Quote: ", quote$Last),
             paste0("Net Debit: ", net_credit),
             paste0("Days to Expiry: ", days_to_exp),
             paste0("Reward/Risk @ B Strike: ", round(B_strike_reward_risk_ratio,2)),
             paste0("Profit Potential @ B Strike: ", round(((B-A)*posn_size+net_credit),2)),
             paste0("Break-even Points: ", BEP1, " ", BEP2),
             paste0("Sell Target: ", round(((B-A)*posn_size+net_credit)-net_credit,2))),
           con = stdout(), sep = "\n", useBytes = FALSE)




