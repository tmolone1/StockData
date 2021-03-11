rm(list=ls())
library(quantmod)
library(tidyverse)
library(lubridate)
symb<-"DOCU"
exp<-"2021-03-19"
chain<-getOptionChain(symb, Exp = exp)
quote<-getQuote(symb)
A<-170
B<-220
posn_size<-100

strategy<-"Bull Put Spread"
puts<-chain[["puts"]] 
A_price<-as.numeric(puts %>% filter(Strike == A) %>% select(Bid))
B_price<-as.numeric(puts %>% filter(Strike == B) %>% select(Ask))
net_quote<-A_price-B_price
cost_to_close<-net_quote*-posn_size
A_price_bought<-0.47
B_price_sold<-10.8
basis<-(B_price_sold-A_price_bought)*-posn_size
profit_pct<-(cost_to_close+basis)/basis
max_risk<-(B-A)*-posn_size-basis
stop_loss<-(max_risk*-0.5-basis)
close_target_85pct_gains<-(basis-basis*0.85)*-1
close_target_85pct_gains_net_quote<-close_target_85pct_gains/posn_size

days_to_exp<-as.period(lubridate::interval(Sys.Date(),as.Date(exp)),unit = "day")
days_to_exp<-ceiling(days_to_exp@day*5/7)
ATM_IV<-as.numeric(head(arrange(puts,desc(Vol)) %>% select(IV),1))

prices<-c(A*.8,A,B,B*1.2)
profit_loss<-c(rep(max_risk,2),rep(-basis,2))
plot(prices,profit_loss,type="o", main=c(symb,strategy))
lines(c(A*.8,B*1.2),c(0,0))
lines(rep(quote$Last,2),range(profit_loss),col="pink")
lm1<-lm(prices[2:3]~profit_loss[2:3])
BEP1<-as.numeric(lm1[["coefficients"]][1])

writeLines(c(paste0("Symbol quote: ", quote$Last),
             paste0("Cost to Close: ", cost_to_close),
             paste0("Current Profit: ", profit_pct),
             paste0("Days to Expiry: ", days_to_exp),
             paste0("Break-even Point: ", BEP1),
             paste0("Close Target 85% gains: ", close_target_85pct_gains),
             paste0("Close Target 85% gains net quote: ", close_target_85pct_gains_net_quote),
             paste0("Suggested stop loss cost to close: ", stop_loss),
             paste0("Suggested stop loss net quote: ", stop_loss/posn_size)),
           con = stdout(), sep = "\n", useBytes = FALSE)




