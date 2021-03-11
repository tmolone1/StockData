rm(list=ls())
library(quantmod)
library(tidyverse)
library(lubridate)
symb<-"DOCU"
exp<-"2021-03-19"
chain<-getOptionChain(symb, Exp = exp)
quote<-getQuote(symb)
width<-20
A<-185
B<-200
posn_size<-100

strategy<-"Bull Put Spread"
puts<-chain[["puts"]] 
A_price<-as.numeric(puts %>% filter(Strike == A) %>% select(Ask))
B_price<-as.numeric(puts %>% filter(Strike == B) %>% select(Bid))
net_quote<-A_price-B_price
credit_debit<-ifelse(net_quote<=0, "credit", "debit")
net_credit<-net_quote*-posn_size
max_risk<-(B-A)*-posn_size+net_credit
reward_risk_ratio<-net_credit/-max_risk
sold_prem_pct<-net_quote/quote$Last

days_to_exp<-as.period(lubridate::interval(Sys.Date(),as.Date(exp)),unit = "day")
days_to_exp<-ceiling(days_to_exp@day*5/7)
ATM_IV<-as.numeric(head(arrange(puts,desc(Vol)) %>% select(IV),1))


prices<-c(A*.8,A,B,B*1.2)
profit_loss<-c(rep(max_risk,2),rep(net_credit,2))
plot(prices,profit_loss,type="o")
lines(c(A*.8,B*1.2),c(0,0))
lines(rep(quote$Last,2),range(profit_loss),col="pink")
lm1<-lm(prices[2:3]~profit_loss[2:3])
BEP1<-as.numeric(lm1[["coefficients"]][1])

writeLines(c(paste0("Net Credit: ", net_credit),
             paste0("Days to Expiry: ", days_to_exp),
             paste0("Reward/Risk: ", round(reward_risk_ratio,2)),
             paste0("Break-even Point: ", BEP1)),
           con = stdout(), sep = "\n", useBytes = FALSE)




