rm(list = ls())

getwd()
setwd("C:/Users/aselv/Desktop/BA Case Study (Clustering)")


library(tidyverse)

# Importing dataset
ccgen<- read_csv("Dataset1.csv")
warning()

View(ccgen)
str(ccgen)               
names(ccgen)

# Checking missing values in the dataframe

NACs <- ccgen %>% summarise_all(funs(sum(is.na(.))))
NAs <- ccgen[!complete.cases(ccgen),]

# DF without missing values

newccgen <- na.omit(ccgen)

# Creating Intelligent KPIs 

newccgen$mon_PURCHASES <- newccgen$PURCHASES/newccgen$TENURE
newccgen$mon_CASH_ADVANCE <- newccgen$CASH_ADVANCE/newccgen$TENURE
newccgen$purchase_type_pref <- ifelse((newccgen$INSTALLMENTS_PURCHASES+newccgen$ONEOFF_PURCHASES) != 0, newccgen$INSTALLMENTS_PURCHASES/(newccgen$INSTALLMENTS_PURCHASES+newccgen$ONEOFF_PURCHASES), NA)
# ^scaling: tending to 1 implies installment dependent, tending to 0 implies One Off purchase dependent

newccgen$avg_per_PURCHASES <- newccgen$PURCHASES/(newccgen$PURCHASES_TRX)
newccgen$avg_per_CASH_ADVANCE <- newccgen$CASH_ADVANCE/(newccgen$CASH_ADVANCE_TRX)

newccgen$avg_per_PURCHASES[!is.finite(newccgen$avg_per_PURCHASES)] <- 0
newccgen$avg_per_CASH_ADVANCE[!is.finite(newccgen$avg_per_CASH_ADVANCE)] <- 0

newccgen$bal_credlim <-newccgen$BALANCE/newccgen$CREDIT_LIMIT
newccgen$pay_minpay <- newccgen$PAYMENTS/newccgen$MINIMUM_PAYMENTS

View(newccgen)
newNACs <- newccgen %>% summarise_all(funs(sum(is.na(.))))

# USD for Descriptive Stats


mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  pctls<-quantile(a, c(0.01, 0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99))
  max <- max(a)
  return(c(n=n, nmiss=nmiss,  mean=m, stdev=s,min = min, pctls=pctls,max=max))
}

mystats0 <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  med <- median(a)
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, median=med,mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}

write.csv(newccgen, "newDataset1.csv")

vars<- c('BALANCE', 'BALANCE_FREQUENCY', 'ONEOFF_PURCHASES','PURCHASES_FREQUENCY',
         'ONEOFF_PURCHASES_FREQUENCY', 'PURCHASES_INSTALLMENTS_FREQUENCY', 'CASH_ADVANCE_FREQUENCY',
         'PRC_FULL_PAYMENT', 'mon_PURCHASES', 'mon_CASH_ADVANCE', 'purchase_type_pref', 
         'avg_per_PURCHASES', 'avg_per_CASH_ADVANCE', 'bal_credlim', 'pay_minpay')

diag_stats<-t(data.frame(apply(newccgen[vars], 2,mystats)))
diag_stats0<-t(data.frame(apply(newccgen[vars], 2,mystats0)))

# Outlier Treatment 
# Omition

newccgen0 <- newccgen[max>UC | min<LC]


