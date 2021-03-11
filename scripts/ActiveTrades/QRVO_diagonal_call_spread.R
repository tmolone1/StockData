rm(list=ls())
library(quantmod)
library(tidyverse)
library(lubridate)
symb<-"QRVO"
frontexp<-"2021-04-16"
backexp<-"2021-05-21"
frontchain<-getOptionChain(symb, Exp = frontexp)
backchain<-getOptionChain(symb, Exp = backexp)
quote<-getQuote(symb)
A<-170
B<-185
posn_size<-100

strategy<-"Diagonal Call Spread"
frontcalls<-frontchain[["calls"]] 
backcalls<-backchain[["calls"]] 
A_price<-as.numeric(frontcalls %>% filter(Strike == A) %>% select(Bid))
B_price<-as.numeric(backcalls %>% filter(Strike == B) %>% select(Ask))
A_price_backmonth<-as.numeric(backcalls %>% filter(Strike == A) %>% select(Bid))
A_price_sold<-8.05
B_price_bought<-7.85
basis<-(A_price_sold-B_price_bought)*-posn_size
net_quote<-A_price-B_price
cost_to_close<-net_quote*posn_size
profit_pct<-round((cost_to_close+basis)/basis,3)

close_target_50pct_gains<-basis*-0.5
close_target_80pct_gains<-basis*-0.2
stop_loss_50pct_loss_potential<-((B-A)*posn_size+basis)*-0.5

front_days_to_exp<-as.period(lubridate::interval(Sys.Date(),as.Date(frontexp)),unit = "day")
front_days_to_exp<-ceiling(front_days_to_exp@day*5/7)
back_days_to_exp<-as.period(lubridate::interval(Sys.Date(),as.Date(backexp)),unit = "day")
back_days_to_exp<-ceiling(back_days_to_exp@day*5/7)
front_ATM_IV<-as.numeric(head(arrange(frontcalls,desc(Vol)) %>% select(IV),1))
back_ATM_IV<-as.numeric(head(arrange(backcalls,desc(Vol)) %>% select(IV),1))
ATM_IV_diff<-back_ATM_IV-front_ATM_IV
projected_A_price_backmonth<-0.5*A_price_backmonth

prices<-c(A*.8,A,B,B*1.2)
profit_loss<-c(rep(-basis,2),rep(((B-A)*-posn_size-basis),2))
profit_loss_after_sell_backmonth_call<-c(rep(-basis,2),rep(((B-A)*-posn_size-basis),2))+rep(projected_A_price_backmonth*posn_size,length(profit_loss))
plot(prices,profit_loss,type="o", ylim = c(min(profit_loss),max(profit_loss_after_sell_backmonth_call)), main=c(symb,strategy))
lines(c(A*.8,B*1.2),c(0,0))
lines(prices,profit_loss_after_sell_backmonth_call,type="o", col="deepskyblue")

lines(rep(quote$Last,2), c(min(profit_loss),max(profit_loss_after_sell_backmonth_call)), col="pink")
lm1<-lm(prices[2:3]~profit_loss[2:3])
BEP<-as.numeric(lm1[["coefficients"]][1])
lm2<-lm(prices[2:3]~profit_loss_after_sell_backmonth_call[2:3])
BEP2<-as.numeric(lm2[["coefficients"]][1])

writeLines(c(paste0("Symbol quote ", quote$Last),
             paste0("Current Profit ", profit_pct),
             paste0("Days to Expiry (front month): ", front_days_to_exp),
             paste0("Days to Expiry (back month): ", back_days_to_exp),
             paste0("Current Cost to Close: ", cost_to_close),
             paste0("Front Month ATM IV: ", front_ATM_IV),
             paste0("Back Month ATM IV: ", back_ATM_IV),
             paste0("Break-even point: ", BEP),
             paste0("Break-even point after backmonth call sold: ", BEP2),
             paste0("Suggested stop loss cost to close: ", -stop_loss_50pct_loss_potential),
             paste0("Suggested profit target as 10x premium: ", 9*basis)),
             con = stdout(), sep = "\n", useBytes = FALSE)




