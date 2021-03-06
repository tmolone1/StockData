library(tidyverse)
library(readxl)
library(quantmod)
library(stringr)

ARKK<-read_excel("data/ETF_holdings.xlsx", sheet = "ARKK")
FLV<-read_excel("data/ETF_holdings.xlsx", sheet = "FLV")
ESGA<-read_excel("data/ETF_holdings.xlsx", sheet = "ESGA")
BUL<-read_excel("data/ETF_holdings.xlsx", sheet = "BUL")
FDG<-read_excel("data/ETF_holdings.xlsx", sheet = "FDG")
DGRW<-read_excel("data/ETF_holdings.xlsx", sheet = "DGRW")
ARKK<-ARKK %>% select(ticker,'weight(%)',fund) %>% mutate(my_weight=2.5)
ESGA<-ESGA %>% select(TICKER, WEIGHT) %>% mutate(fund="ESGA", my_weight=1)
FLV<-FLV %>% select(TICKER, WEIGHT) %>% mutate(fund="FLV", my_weight=1)
BUL<-BUL %>% select(StockTicker,Weightings,Account) %>% mutate(my_weight=1)
FDG<-FDG %>% select(TICKER, WEIGHT) %>% mutate(fund="FDG", my_weight=1)
DGRW<-DGRW %>% select(`Security Ticker`, `Weight %`) %>% mutate(fund="DGRW", my_weight=1)
names(ARKK)<-names(ESGA)
names(BUL)<-names(ESGA)
names(FLV)<-names(ESGA)
names(FDG)<-names(ESGA)
names(DGRW)<-names(ESGA)
DGRW<-DGRW %>% mutate(TICKER=str_split(DGRW$TICKER," ",simplify = TRUE)[,1])
ARKK<-ARKK %>% mutate(WEIGHT=WEIGHT/100)
dat<-rbind(ARKK,ESGA,BUL,FLV,FDG,DGRW)
dat<-dat %>% mutate(calc=WEIGHT*my_weight) %>% arrange(desc(calc))
summ<-dat %>% group_by(TICKER) %>% summarize(tot=sum(calc),etfs=n()) %>% arrange(desc(tot)) 
top20<-head(summ,20)
tbl<-as_tibble(cbind(top20,getQuote(top20$TICKER)))
tbl<-tbl %>% filter(Last <300, Volume > 500000)

top12<-head(tbl,12)
top7<-head(tbl,7)
print(top7)
print(tail(top12),5)
