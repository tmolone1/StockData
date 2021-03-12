rm(list=setdiff(ls(),c("basis","profit_pct","Active_Trades")))
library(quantmod)
library(tidyverse)
library(lubridate)
symb<-"XRT"
exp<-"2021-04-16"
chain<-getOptionChain(symb, Exp = exp)
quote<-getQuote(symb)
A<-80
B<-100
posn_size<-100

strategy<-"Short Call Spread"
calls<-chain[["calls"]] 
A_price<-as.numeric(calls %>% filter(Strike == A) %>% select(Ask))
B_price<-as.numeric(calls %>% filter(Strike == B) %>% select(Bid))
A_price_sold<-12.68
B_price_bought<-2.68
basis<-(A_price_sold-B_price_bought)*-posn_size
net_quote<-A_price-B_price
cost_to_close<-net_quote*posn_size
profit_pct<-round((cost_to_close+basis)/basis,3)

close_target_50pct_gains<-basis*-0.5
close_target_80pct_gains<-basis*-0.2
stop_loss_50pct_loss_potential<-((B-A)*posn_size+basis)*-0.5
stop_loss_cost_to_close<-(stop_loss_50pct_loss_potential+basis)*-1

days_to_exp<-as.period(lubridate::interval(Sys.Date(),as.Date(exp)),unit = "day")
days_to_exp<-ceiling(days_to_exp@day*5/7)
ATM_IV<-as.numeric(head(arrange(calls,desc(Vol)) %>% select(IV),1))

prices<-c(A*.8,A,B,B*1.2)
profit_loss<-c(rep(-basis,2),rep(((B-A)*-posn_size-basis),2))
plot(prices,profit_loss,type="o", main=c(symb,strategy))
lines(c(A*.8,B*1.2),c(0,0))
lines(rep(quote$Last,2), c(-basis,basis), col="pink")
lm1<-lm(prices[2:3]~profit_loss[2:3])
BEP<-as.numeric(lm1[["coefficients"]][1])

writeLines(c(paste0("Symbol Quote: ", quote$Last),
             paste0("Current Profit ", profit_pct),
             paste0("Days to Expiry: ", days_to_exp),
             paste0("Current Cost to Close: ", cost_to_close),
             paste0("Close Target 50% gains ", close_target_50pct_gains),
             paste0("Close Target 80% gains ", close_target_80pct_gains),
             paste0("Break-even Point: ", BEP),
             paste0("Suggested stop loss cost to close: ", stop_loss_cost_to_close)),
           con = stdout(), sep = "\n", useBytes = FALSE)




