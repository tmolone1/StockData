rm(list=ls())
library(quantmod)
library(tidyverse)
symbols<-c("AAL","FEYE","FMC","LOW","NEE","QQQ","QRVO","SPY","IWM","MDT","PYPL","SQ","TDOC","AAPL","MSFT","DOCU","Z","CRSP")
tbl<-tibble(Symbol=symbols, ATM_IV_1=0, ATM_IV_2=0)
i<-1
exp<-"2021-03-19"
exp2<-"2021-04-16"
  
  for (i in 1:length(symbols)) {
  symb<-symbols[i]
  chain<-getOptionChain(symb, Exp = exp)
  calls<-chain[["calls"]] 
  chain2<-getOptionChain(symb, Exp = exp2)
  calls2<-chain2[["calls"]] 
  tbl[i,]$ATM_IV_1<-as.numeric(head(arrange(calls,desc(Vol)) %>% select(IV),1))
  tbl[i,]$ATM_IV_2<-as.numeric(head(arrange(calls2,desc(Vol)) %>% select(IV),1))
    } 

days_to_exp<-as.period(lubridate::interval(Sys.Date(),as.Date(exp)),unit = "day")
days_to_exp<-ceiling(days_to_exp@day*5/7) 
days_to_exp2<-as.period(lubridate::interval(Sys.Date(),as.Date(exp2)),unit = "day")
days_to_exp2<-ceiling(days_to_exp2@day*5/7)

tbl<- tbl %>% mutate(IV_diff=ATM_IV_2-ATM_IV_1)
print(days_to_exp)
print(days_to_exp2)
View(tbl)
