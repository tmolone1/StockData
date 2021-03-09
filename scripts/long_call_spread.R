library(quantmod)
library(tidyverse)
library(lubridate)
symb<-"IWM"
exp<-"2021-04-16"
chain<-getOptionChain(symb, Exp = exp)
quote<-getQuote(symb)
A<-230
B<-245
posn_size<-100

strategy<-"Short Call Spread"
calls<-chain[["calls"]] 
buy_price<-as.numeric(calls %>% filter(Strike == A) %>% select(Ask))
sell_price<-as.numeric(calls %>% filter(Strike == B) %>% select(Bid))
net_quote<-sell_price-buy_price
credit_debit<-ifelse(net_quote>=0, "credit", "debit")
net_credit<-net_quote*posn_size
max_reward<-((B-A)*posn_size)+net_credit
reward_risk_ratio<-max_reward/-net_credit
prem_pct<-net_quote/quote$Last
spread_midpoint<-A+(B-A)/2
trade_value_at_midpoint<-((spread_midpoint-A)*posn_size)+net_credit

days_to_exp<-as.period(lubridate::interval(Sys.Date(),as.Date(exp)),unit = "day")
days_to_exp<-ceiling(days_to_exp@day*5/7)
ATM_IV<-as.numeric(head(arrange(calls,desc(Vol)) %>% select(IV),1))
lm1<-lm(prices[2:4]~profit_loss[2:4])
BEP<-as.numeric(lm1[["coefficients"]][1])
stop_loss_est<-quote$Last-(2*net_quote)/-.3

prices<-c(A*.8,A,spread_midpoint,B,B*1.2)
profit_loss<-c(rep(net_credit,2),trade_value_at_midpoint,rep(max_reward,2))
plot(prices,profit_loss,type="o")
lines(c(A*.8,B*1.2),c(0,0))

writeLines(c(paste0("Net Debit: ", net_credit),
             paste0("Days to Expiry: ", days_to_exp),
             paste0("Reward/Risk: ", round(reward_risk_ratio,2)),
             paste0("Break-even Point: ", BEP),
             paste0("Suggested stop loss price for underlying: ", round(stop_loss_est,0))),
           con = stdout(), sep = "\n", useBytes = FALSE)




