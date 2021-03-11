rm(list=ls())
library(quantmod)
library(tidyverse)
library(lubridate)
symb<-"IWM"
exp<-"2021-05-21"
chain<-getOptionChain(symb, Exp = exp)
quote<-getQuote(symb)
A<-219
B<-235
posn_size<-100

strategy<-"Back Spread with Calls"
calls<-chain[["calls"]] 
A_price<-as.numeric(calls %>% filter(Strike == A) %>% select(Bid))
B_price<-as.numeric(calls %>% filter(Strike == B) %>% select(Ask))
net_quote<-2*B_price-A_price
cost_to_close<-net_quote*posn_size
A_price_sold<-17.46
B_price_bought<-8.05
basis<-(A_price_sold-2*B_price_bought)*-posn_size
max_risk<-(-(B-A)*posn_size)-basis
spread_midpoint<-A+(B-A)/2
trade_value_at_midpoint<-(-(spread_midpoint-A)*posn_size)-basis

days_to_exp<-as.period(lubridate::interval(Sys.Date(),as.Date(exp)),unit = "day")
days_to_exp<-ceiling(days_to_exp@day*5/7)
ATM_IV<-as.numeric(head(arrange(calls,desc(Vol)) %>% select(IV),1))
stop_loss_est<-quote$Last-(2*net_quote)/-.3
profit_pct<-round((cost_to_close-basis)/-basis,3)

prices<-c(A*.8,A,spread_midpoint,B,B*1.2)
profit_loss<-c(rep(-basis,2),trade_value_at_midpoint,max_risk,-(B*1.2-A)*posn_size+(B*1.2-B)*posn_size*2-basis)
lm1<-lm(prices[2:4]~profit_loss[2:4])
BEP1<-as.numeric(lm1[["coefficients"]][1])
lm2<-lm(prices[4:5]~profit_loss[4:5])
BEP2<-as.numeric(lm2[["coefficients"]][1])
target<-1.25*228 # set based on ATM IV and underlying price when trade was entered
upside_implied_profit_potential<-(target-A)*-posn_size+(target-B)*2*posn_size-basis
upside_reward_risk<-upside_implied_profit_potential/-max_risk
plot(prices,profit_loss,type="o", main=c(symb,strategy))
lines(c(A*.8,B*1.2),c(0,0))
lines(rep(quote$Last,2),range(profit_loss), col="pink")


writeLines(c(paste0("Symbol quote ", quote$Last),
             paste0("Current Profit ", profit_pct),
             paste0("Days to Expiry: ", days_to_exp),
             paste0("Current Cost to Close: ", cost_to_close),
             paste0("Upside Profit Target: ", round(upside_implied_profit_potential*0.8,2)),
             paste0("Upside Profit Target net quote: ", round((upside_implied_profit_potential*0.8)/posn_size),2),
             paste0("Break-even Points: ", BEP1, " ", BEP2),
             paste0("Generally close these trades with around 25-30 days to expiry")),
           con = stdout(), sep = "\n", useBytes = FALSE)




