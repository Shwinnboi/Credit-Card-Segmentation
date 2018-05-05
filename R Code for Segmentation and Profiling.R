getwd()
setwd("C:/Users/aselv/Desktop/BA Case Study (Clustering)")


library(tidyverse)

# Importing dataset
ccgen<- read_csv("C:/Users/aselv/Desktop/BA Case Study (Clustering)/Dataset1.csv")

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
newccgen$purchase_type_pref <- newccgen$INSTALLMENTS_PURCHASES/(newccgen$INSTALLMENTS_PURCHASES+newccgen$ONEOFF_PURCHASES)
# ^scaling: tending to 1 implies installment dependent, tending to 0 implies One Off purchase dependent

newccgen$avg_per_PURCHASES <- newccgen$PURCHASES/(newccgen$PURCHASES_TRX)
newccgen$avg_per_CASH_ADVANCE <- newccgen$CASH_ADVANCE/(newccgen$CASH_ADVANCE_TRX)

newccgen$avg_per_PURCHASES[!is.finite(newccgen$avg_per_PURCHASES)] <- 0
newccgen$avg_per_CASH_ADVANCE[!is.finite(newccgen$avg_per_CASH_ADVANCE)] <- 0

newccgen$bal_credlim <-newccgen$BALANCE/newccgen$CREDIT_LIMIT
newccgen$pay_minpay <- newccgen$PAYMENTS/newccgen$MINIMUM_PAYMENTS

View(newccgen)
newNACs <- ccgen %>% summarise_all(funs(sum(is.na(.))))
