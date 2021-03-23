rm(list=setdiff(ls(),c("basis","profit_pct","Active_Trades")))
dev.off()
dev.new()
library(quantmod)
library(tidyverse)
library(lubridate)
symb<-"XLF"
exp<-"2021-05-21"
chain<-getOptionChain(symb, Exp = exp)
quote<-getQuote(symb)
A<-35
B<-40
posn_size<-300

strategy<-"Back Spread with Calls"
calls<-chain[["calls"]] 
A_price<-as.numeric(calls %>% filter(Strike == A) %>% select(Bid))
B_price<-as.numeric(calls %>% filter(Strike == B) %>% select(Ask))
net_quote<-A_price-2*B_price
credit_debit<-ifelse(net_quote>=0, "credit", "debit")
basis<-(A_price-2*B_price)*-posn_size
max_risk<-(-(B-A)*posn_size)-basis
net_credit_risk_ratio<-basis/max_risk
sold_prem_pct<-net_quote/quote$Last

## after trade is entered
# strategy<-"Back Spread with Calls"
# calls<-chain[["calls"]] 
# A_price<-as.numeric(calls %>% filter(Strike == A) %>% select(Ask))
# B_price<-as.numeric(calls %>% filter(Strike == B) %>% select(Bid))
# net_quote<-2*B_price-A_price
# cost_to_close<-net_quote*-posn_size
# A_price_sold<-17.46
# B_price_bought<-8.05
# basis<-(A_price_sold-2*B_price_bought)*-posn_size
# max_risk<-(-(B-A)*posn_size)-basis

days_to_exp<-as.period(lubridate::interval(Sys.Date(),as.Date(exp)),unit = "day")
days_to_exp<-ceiling(days_to_exp@day*5/7)
ATM_IV<-as.numeric(head(arrange(calls,desc(Vol)) %>% select(IV),1))
stop_loss_est<-quote$Last-(2*net_quote)/-.3
profit_pct<-round((cost_to_close+basis)/basis,3)

prices<-c(A*.8,A,B,B*1.2)
profit_loss<-c(rep(-basis,2),max_risk,-(B*1.2-A)*posn_size+(B*1.2-B)*posn_size*2-basis)
lm1<-lm(prices[2:3]~profit_loss[2:3])
BEP1<-as.numeric(lm1[["coefficients"]][1])
lm2<-lm(prices[3:4]~profit_loss[3:4])
BEP2<-as.numeric(lm2[["coefficients"]][1])
target<-1.25*228 # set based on ATM IV and underlying price when trade was entered
upside_implied_profit_potential<-(target-A)*-posn_size+(target-B)*2*posn_size-basis
upside_reward_risk<-upside_implied_profit_potential/-max_risk
plot(prices,profit_loss,type="o", main=c(symb,strategy))
lines(c(A*.8,B*1.2),c(0,0))
lines(rep(quote$Last,2),range(profit_loss), col="pink")

#pre-entry screening
writeLines(c(paste0("Symbol quote ", quote$Last),
             paste0("Net Credit: ", -basis),
             paste0("Net Quote: ", net_quote),
             paste0("Days to Expiry: ", days_to_exp),
             paste0("Net Credit/Risk: ", round(net_credit_risk_ratio,2)),
             paste0("Upside Reward/Risk: ", round(upside_reward_risk,2)),
             paste0("Break-even Points: ", BEP1, " ", BEP2),
             paste0("Generally close these trades with around 25-30 days to expiry")),
           con = stdout(), sep = "\n", useBytes = FALSE)

##after trade is entered
# writeLines(c(paste0("Symbol quote ", quote$Last),
#              paste0("Current Profit ", profit_pct),
#              paste0("Days to Expiry: ", days_to_exp),
#              paste0("Current Cost to Close: ", round(cost_to_close,2)),
#              paste0("Upside Profit Target: ", round(upside_implied_profit_potential*0.5,2)),
#              paste0("Upside Profit Target net quote: ", round((upside_implied_profit_potential*0.5)/posn_size,2)),
#              paste0("Break-even Points: ", BEP1, " ", BEP2),
#              paste0("Generally close these trades with around 25-30 days to expiry")),
#            con = stdout(), sep = "\n", useBytes = FALSE)

