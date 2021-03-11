rm(list=setdiff(ls(),c("basis","profit_pct","Active_Trades")))
library(quantmod)
library(tidyverse)
library(lubridate)
symb<-"DIS"
exp<-"2021-04-16"
chain<-getOptionChain(symb, Exp = exp)
quote<-getQuote(symb)
width<-5
A<-195
B<-A+width
posn_size<-100

strategy<-"Front Spread with Calls"
calls<-chain[["calls"]] 

# pre-entry screening
A_price<-as.numeric(calls %>% filter(Strike == A) %>% select(Ask))
B_price<-as.numeric(calls %>% filter(Strike == B) %>% select(Bid))
basis<-(2*B_price-A_price)*-posn_size
net_quote<-2*B_price-A_price

## after trade is entered
# A_price<-as.numeric(calls %>% filter(Strike == A) %>% select(Bid))
# B_price<-as.numeric(calls %>% filter(Strike == B) %>% select(Ask))
# A_price_bought<-5.7
# B_price_sold<-3
# basis<-(2*B_price_sold-A_price_bought)*-posn_size
# net_quote<-2*B_price-A_price
# cost_to_close<-net_quote*posn_size
# profit_pct<-round((cost_to_close+basis)/basis,3)

profit_potential<-(B-A)*posn_size-basis
close_target_50pct_gains<-basis*-0.5
close_target_80pct_gains<-basis*-0.2
stop_loss<-0.1*profit_potential  # 10% of profit potential

days_to_exp<-as.period(lubridate::interval(Sys.Date(),as.Date(exp)),unit = "day")
days_to_exp<-ceiling(days_to_exp@day*5/7)
ATM_IV<-as.numeric(head(arrange(calls,desc(Vol)) %>% select(IV),1))

prices<-c(A*.85,A,B,B*1.15)
profit_loss<-c(rep(-basis,2),(B-A)*posn_size-basis,(((B*1.15-B)*2*posn_size-(B*1.15-A)*posn_size)+basis)*-1)
plot(prices,profit_loss,type="o", main=c(symb,strategy))
lines(c(A*.85,B*1.15),c(0,0))
lines(rep(quote$Last,2), range(profit_loss), col="pink")
lm1<-lm(prices[3:4]~profit_loss[3:4])
BEP<-as.numeric(lm1[["coefficients"]][1])
stop_price_underlying<-stop_loss*-lm1$coefficients[[2]]+lm1$coefficients[[1]]
lines(rep(stop_price_underlying,2),range(profit_loss),lty=2, col="red")

# Win Probability estimate
projected<-quote$Last*(1+(0.1/365.25*days_to_exp*7/5))
x<-seq(projected*(1-ATM_IV), projected*(1+ATM_IV), length=100)
hx<-dnorm(x, mean = projected, sd = ATM_IV*projected*0.5, log = FALSE)
plot(x, hx, type="l", lty=2, xlab="stock price",
     ylab="Density", main="Distribution of Outcomes")
lines(rep(quote$Last,2),range(hx),col="pink")
lines(rep(BEP,2),range(hx),col="green")
lines(rep(stop_price_underlying,2),range(hx),col="red")
cdf <- pnorm(x, projected, ATM_IV*projected*0.5)
tbl<-as_tibble(cbind(x,hx,cdf))
pp<-function(x) {paste0(round(x,2)*100,"%")}
win_pct<-head(tbl %>% mutate(match=x-BEP) %>% arrange(abs(match)) %>% select(cdf),1)
min_win_pct<-head(tbl %>% mutate(match=x-A) %>% arrange(abs(match)) %>% select(cdf),1)
maj_win_pct<-head(tbl %>% mutate(match=x-BEP) %>% arrange(abs(match)) %>% select(cdf),1)-head(tbl %>% mutate(match=x-A) %>% arrange(abs(match)) %>% select(cdf),1)
min_loss_pct<-head(tbl %>% mutate(match=x-stop_price_underlying) %>% arrange(abs(match)) %>% select(cdf),1)-head(tbl %>% mutate(match=x-BEP) %>% arrange(abs(match)) %>% select(cdf),1)
stop_loss_pct<-1-head(tbl %>% mutate(match=x-stop_price_underlying) %>% arrange(abs(match)) %>% select(cdf),1)
objective_fun<-as.numeric(maj_win_pct*0.5*profit_potential+min_win_pct*-basis+min_loss_pct*0.5*-stop_loss+stop_loss_pct*-stop_loss)

## pre-entry screening
writeLines(c(paste0("Symbol quote: ", quote$Last),
             paste0("Net Credit ", -basis),
             paste0("Net Quote: ", net_quote),
             paste0("Days to Expiry: ", days_to_exp),
             paste0("Profit Potential: ", round(profit_potential,2)),
             paste0("Reward/Risk @ 10% move past B: ", round(profit_potential/stop_loss,2)),
             paste0("ATM IV: ", ATM_IV),
             paste0("Break-even Point: ", BEP),
             paste0("Estimated Win rate: ", pp(win_pct)),
             paste0("Probability of minor win: ", pp(min_win_pct)),
             paste0("Probability of major win: ", pp(maj_win_pct)),
             paste0("Probability of small loss less than stop: ", pp(min_loss_pct)),
             paste0("Probability of stop loss: ", pp(stop_loss_pct)),
             paste0("Expected value per trade: ", floor(objective_fun)),
             paste0("Stop Loss net quote: ", round(stop_loss/posn_size,2))),
           con = stdout(), sep = "\n", useBytes = FALSE)


## after trade is entered
# writeLines(c(paste0("Symbol quote ", quote$Last),
#              paste0("Current Profit ", profit_pct),
#              paste0("Days to Expiry: ", days_to_exp),
#              paste0("ATM IV: ", ATM_IV),
#              paste0("Current Cost to Close: ", cost_to_close),
#              paste0("Break-even Point: ", BEP),
#              paste0("Suggested stop loss cost to close: ", round(stop_loss,2)),
#              paste0("Suggested profit target as 80% maximum gain: ", (profit_potential)*.8),
#              paste0("Suggested profit target net quote: ", ((profit_potential)*.8)/-posn_size)),
#            con = stdout(), sep = "\n", useBytes = FALSE)
