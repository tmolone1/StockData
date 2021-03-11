rm(list=setdiff(ls(),c("basis","profit_pct","Active_Trades")))
library(quantmod)
library(tidyverse)
library(lubridate)
symb<-"SPY"
exp<-"2021-03-19"
chain<-getOptionChain(symb, Exp = exp)
quote<-getQuote(symb)
A<-385
B<-390
C<-395
D<-400
posn_size<-300

strategy<-"Skip Strike Butterfly with Calls"
calls<-chain[["calls"]] 
A_price<-as.numeric(calls %>% filter(Strike == A) %>% select(Ask))
B_price<-as.numeric(calls %>% filter(Strike == B) %>% select(Bid))
D_price<-as.numeric(calls %>% filter(Strike == D) %>% select(Ask))
A_price_bought<-8.27
B_price_sold<-4.77
D_price_bought<-0.88
basis<-(2*B_price_sold-A_price_bought-D_price_bought)*-posn_size
net_quote<-2*B_price-A_price-D_price
cost_to_close<-net_quote*posn_size
profit_pct<-round((cost_to_close+basis)/basis,3)

profit_potential<-(B-A)*posn_size-basis
close_target_50pct_gains<-basis*-0.5
close_target_80pct_gains<-basis*-0.2
stop_loss_50pct_loss_potential<-((B-A)*posn_size+basis)*-0.5

days_to_exp<-as.period(lubridate::interval(Sys.Date(),as.Date(exp)),unit = "day")
days_to_exp<-ceiling(days_to_exp@day*5/7)
ATM_IV<-as.numeric(head(arrange(calls,desc(Vol)) %>% select(IV),1))

prices<-c(A*.9,A,B,D,D*1.1)
profit_loss<-c(rep(-basis,2),(B-A)*posn_size-basis,rep((D-C)*-posn_size-basis,2))
plot(prices,profit_loss,type="o", main=c(symb,strategy))
lines(c(A*.9,D*1.1),c(0,0))
lines(rep(quote$Last,2), range(profit_loss), col="pink")
lm1<-lm(prices[3:4]~profit_loss[3:4])
BEP<-as.numeric(lm1[["coefficients"]][1])

writeLines(c(paste0("Symbol quote ", quote$Last),
             paste0("Current Profit ", profit_pct),
             paste0("Days to Expiry: ", days_to_exp),
             paste0("ATM IV: ", ATM_IV),
             paste0("Current Cost to Close: ", cost_to_close),
             paste0("Break-even Point: ", BEP),
             paste0("Suggested stop loss cost to close: ", stop_loss_50pct_loss_potential),
           paste0("Suggested profit target as 80% maximum gain: ", (profit_potential)*.8),
           paste0("Suggested profit target net quote: ", ((profit_potential)*.8)/-posn_size)),
           con = stdout(), sep = "\n", useBytes = FALSE)




