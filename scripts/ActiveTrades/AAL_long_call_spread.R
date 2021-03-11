rm(list=setdiff(ls(),c("basis","profit_pct","Active_Trades")))
library(quantmod)
library(tidyverse)
library(lubridate)
symb<-"AAL"
exp<-"2021-05-21"
chain<-getOptionChain(symb, Exp = exp)
quote<-getQuote(symb)
A<-20
B<-30
posn_size<-100

strategy<-"Long Call Spread"
calls<-chain[["calls"]] 
A_price<-as.numeric(calls %>% filter(Strike == A) %>% select(Bid))
B_price<-as.numeric(calls %>% filter(Strike == B) %>% select(Ask))
net_quote<-B_price-A_price
cost_to_close<-net_quote*posn_size
A_price_bought<-3.66
B_price_sold<-0.98
basis<-(A_price_bought-B_price_sold)*posn_size
profit_pct<-(cost_to_close+basis)/-basis

max_reward<-((B-A)*posn_size)-basis
reward_risk_ratio<-max_reward/basis

days_to_exp<-as.period(lubridate::interval(Sys.Date(),as.Date(exp)),unit = "day")
days_to_exp<-ceiling(days_to_exp@day*5/7)
ATM_IV<-as.numeric(head(arrange(calls,desc(Vol)) %>% select(IV),1))


prices<-c(A*.8,A,B,B*1.2)
profit_loss<-c(rep(-basis,2),rep(max_reward,2))
lm1<-lm(prices[2:4]~profit_loss[2:4])
BEP<-as.numeric(lm1[["coefficients"]][1])

plot(prices,profit_loss,type="o", main=c(symb,strategy))
lines(c(A*.8,B*1.2),c(0,0))
lines(rep(quote$Last,2),range(profit_loss),col="pink")

writeLines(c(paste0("Current Trade Value: ", -cost_to_close),
             paste0("Current Profit %: ", profit_pct),
             paste0("Days to Expiry: ", days_to_exp),
             paste0("Profit Target as 80% gains: ", max_reward*0.8),
             paste0("Profit Target 80% gains, net quote: ", (max_reward*0.8)/posn_size),
             paste0("Break-even Point: ", BEP),
             paste0("Suggested stop loss price for underlying: ", B*(1-(.01*days_to_exp)))),
           con = stdout(), sep = "\n", useBytes = FALSE)




