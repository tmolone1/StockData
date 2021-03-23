rm(list=setdiff(ls(),c("basis","profit_pct","Active_Trades")))
library(quantmod)
library(tidyverse)
library(lubridate)
symb<-"XLF"
exp<-"2021-04-16"
chain<-getOptionChain(symb, Exp = exp)
quote<-getQuote(symb)
A<-31
B<-37
posn_size<-300

strategy<-"Back Spread with Puts"
puts<-chain[["puts"]] 
A_price<-as.numeric(puts %>% filter(Strike == A) %>% select(Bid))
B_price<-as.numeric(puts %>% filter(Strike == B) %>% select(Ask))
A_price_bought<-0.2
B_price_sold<-3.13
basis<-(B_price_sold-2*A_price_bought)*-posn_size
net_quote<-B_price-2*A_price
cost_to_close<-net_quote*posn_size
profit_pct<-round((cost_to_close+basis)/basis,3)

profit_potential<-basis*-1
downside_profit_potential<-(A*0.9-A)*-posn_size
close_target_50pct_gains<-basis*-0.5
close_target_80pct_gains<-basis*-0.2

days_to_exp<-as.period(lubridate::interval(Sys.Date(),as.Date(exp)),unit = "day")
days_to_exp<-ceiling(days_to_exp@day*5/7)
ATM_IV<-as.numeric(head(arrange(puts,desc(Vol)) %>% select(IV),1))

prices<-c(A*.85,A,B,B*1.15)
profit_loss<-c(((A*.85-A)*2*posn_size-((A*0.85-B)*posn_size)+basis)*-1,((B-A)*-posn_size)-basis, rep(-basis,2))
plot(prices,profit_loss,type="o", main=c(symb,strategy))
lines(c(A*.85,B*1.15),c(0,0))
lines(rep(quote$Last,2), range(profit_loss), col="pink")
lm1<-lm(prices[1:2]~profit_loss[1:2])
BEP1<-as.numeric(lm1[["coefficients"]][1])
lm2<-lm(prices[2:3]~profit_loss[2:3])
BEP2<-as.numeric(lm2[["coefficients"]][1])

writeLines(c(paste0("Symbol quote ", quote$Last),
             paste0("Current Profit ", profit_pct),
             paste0("Days to Expiry: ", days_to_exp),
             paste0("ATM IV: ", ATM_IV),
             paste0("Current Cost to Close: ", cost_to_close),
             paste0(c("Break-even Points: ", BEP1, BEP2)),
             paste0("Management: at 21 d2e, (15 calendar d2e), close non-profitable posns"),
             paste0("Suggested profit target as 80% maximum gain: ", (profit_potential)*.8),
             paste0("Suggested profit target net quote: ", ((profit_potential)*.8)/-posn_size)),
           con = stdout(), sep = "\n", useBytes = FALSE)




