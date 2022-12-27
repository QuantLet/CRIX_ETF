[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **CRIX_ETF** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : 'CRIX_ETF'



Published in : 'BRC-RDC' 


Description : 'Simulates an ETF on the CRIX (Haerdle and Trimborn 2015). Requires Cryptocurrency Data from https://box.hu-berlin.de/d/588d5e7e5de84ae58a51/ '


Keywords : 'Cryptocurrency, CRIX, ETF'


Author : 'Konstantin Häusler'

```

### R Code
```r

#################################
### Understanding Crypto ETFs ###
#################################

#set directory
#setwd("path/directory")

#load packages
libraries = c( "plyr","dplyr", "ggplot2", "viridis")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#############
#load data##
#############



#CRIX weights
#input structure: 
#rebalancing date (format = "%Y-%m-%d") / constituent name / weight
crix.weights <- read.csv("constituents.csv")
#crix.weights$date <- c(rep(c("2018-01-01"),5),  rep(c("2018-03-14"),5), rep(c("2018-04-13"),5))
#crix.weights$date <- as.date(crix.weights$date, format="%Y-%m-%d")
crix.weights$date <- as.factor(crix.weights$date)
crix.weights$index_name <- NULL
crix.weights$X <- NULL
names(crix.weights) <- c("date", "index_members", "weights")
crix.weights <- crix.weights[as.Date(crix.weights$date,format="%Y-%m-%d" ) <  "2021-06-02"
                             & as.Date(crix.weights$date,format="%Y-%m-%d" ) >  "2017-12-31" ,]

#CC data (obtained from CoinGecko)
load(file = "CCs.rda")

#spread data
load(file = "spread_df.RData")

# Simulated deposits via Google Trends
load(file="crix_deposits.rda")
crix.deposits <- crix.deposits[as.Date(crix.deposits$date, format="%Y-%m-%d") < "2021-06-02",]

#dates data frame
dates <- data.frame(d = unique(levels(crix.weights$date)))
dates$d <- as.Date(dates$d, format = "%Y-%m-%d")
dates <- dates[dates[["d"]] < "2021-06-02",]
dates <- as.data.frame(dates)
names(dates) <- "d"


#merge CC names and tickers of crix.weights and CCs
source("revalue_crix_weights.R")
crix.weights$index_members <- merge_ticks(crix.weights$index_members)

######################
#auxiliary functions##
######################

#fees
fee_schedule <- function(v){
  case_when(
    v<=10e03 ~ 0.005, 
    v<=50e03 ~ 0.0035, 
    v<=100e03 ~ 0.0025, 
    v<=1000e03 ~ 0.002, 
    v<=100000e03 ~ 0.0018, 
    v<=2000000e03 ~ 0.0015
  )
}

#spreads (as function of transaction volume)
spreads <- function(volume){
  #s <-  qr95$coefficients[1] + volume * qr95$coefficients[2]
  s <<- 2.26e-04  + volume *  2.37e-05
}

###############
#descriptives##
###############

#plot: number of CRIX constituents
png(file="number_of_constituents.png")
par(bg="transparent")
plot(table(factor(crix.weights$date)), ylab = "#constituents")
dev.off()


# plot: fee schedule
png(file="fee_schedule.png")
par(bg="transparent")
for (i in c(10e03,50e03,100e03,1000e03)) {
  amount <- fee_schedule(i)
  if(i==10e03) {
    i_minus <- 0  
    plot(x=c(0:10), y=rep(amount, length(0:10)), ylim=c(0,0.006), xlim=c(0,1000e03), 
         xlab="transaction volume (in USD)", ylab="fee", type="s", lwd=1.5)}
  else 
    lines(x=c(i_minus:i), y=rep(amount, length(c(i_minus:i))), lwd=1.5) #horizontal lines
  lines(x=c(i_minus,i_minus),   
        y=c(fee_schedule(i_minus),fee_schedule(i)), lwd=1.5) #vertical lines
  i_minus <- i
}
dev.off()
rm(i_minus)

###############
#rebalancing###
###############

# initial investment/ allocation of capital 

initial_invest <- function(date){
  tmp.df <- crix.weights[which(crix.weights$date %in% date),]
  tmp.df$prev_weight <- tmp.df$weights
  tmp.df$delta <- 0
  
  df <- merge(tmp.df, CCs[,c("Id","prices", "Datetime")], by.x =c("index_members", "date")  ,by.y=c("Id", "Datetime"))
  df$quantity.a.r.prev <- NA
  df$deposits <- 1000 #initial deposit 
  
  df$value.crix.pf.s.b.r <- df$deposits * df$weights #distribute money among CRIX constituents
  df$value.crix.pf.aggr.b.r <- sum(df$value.crix.pf.s)
  
  df$quantity <- (df$deposits * df$weights) /df$prices  #bought coins
  df$quantity.a.r <- df$quantity * (1-fee_schedule(df$quantity*df$prices))
  
  df$value.crix.pf.s.a.r <- df$quantity.a.r * df$prices
  df$value.crix.pf.aggr.a.r <- sum(df$value.crix.pf.s.a.r)
  
  df$scal.2 <- spread.df$relBTC.2[which(spread.df$Id %in% df$index_members 
                                        & spread.df$Datetime %in% df$date)]
  df$scal.5 <- spread.df$relBTC.5[which(spread.df$Id %in% df$index_members 
                                        & spread.df$Datetime %in% df$date)]
  df$scal.10 <- spread.df$relBTC.10[which(spread.df$Id %in% df$index_members 
                                          & spread.df$Datetime %in% df$date)]
  df$spread.2 <- spreads(df$quantity.a.r) * df$scal.2 
  df$spread.5 <- spreads(df$quantity.a.r) * df$scal.5 
  df$spread.10 <- spreads(df$quantity.a.r) * df$scal.10 
  
  df$costs <- fee_schedule(df$quantity*df$prices)
  
  
  
  df.prev <- df
  print(head(df.prev))
  return(df.prev)
}
#date <- "2020-07-01"
#df.prev <- initial_invest("2020-07-01")  
#rm(df.prev, df2)

#rebalancing

rebalance <- function(date){
  #grab data: CRIX weights
  tmp.df <- crix.weights[which(crix.weights$date %in% date),]
  sub.df <- crix.weights[which(crix.weights$date %in% c(date, date_minus1)),]
  
  # CCs that appear at both dates
  sub.df.double <- sub.df[duplicated(sub.df$index_members,  fromLast=TRUE), ] 
  colnames(sub.df.double)[colnames(sub.df.double)=="weights"] <- "prev_weight"
  #sub.df.double$occurence <- 2
  
  # constituents that dropped out at current rebalancing date
  sub.df.single <- sub.df[-which(sub.df$index_members %in% 
                                   sub.df.double$index_members),] 
  # need: quantity.a.r; sell at market price
  sub.df.single <- merge(sub.df.single, CCs[,c("Id","prices", "Datetime")], 
                         by.x =c("index_members", "date")  ,by.y=c("Id", "Datetime"))
  sub.df.single <- merge(sub.df.single, df.prev[,c("index_members", "date", "quantity.a.r")],
                         by= c("index_members", "date"), all.x = T)
  sub.df.single <- na.omit(sub.df.single) #only CCs that got dropped
  sub.df.single$spread <- spreads(sub.df.single$quantity)
  # * sub.df.single$prices #problem:quantity ist in BTC
  sub.df.single$costs <- (fee_schedule(sub.df.single$quantity.a.r *sub.df.single$prices))*
    sub.df.single$quantity.a.r * sub.df.single$prices
  sub.df.single <<- sub.df.single
  
  tmp.df <- merge(tmp.df, sub.df.double[,c("index_members", "prev_weight")],
                  by="index_members", all = TRUE) #[, c("index_members","prev_weight")]
  tmp.df[is.na(tmp.df)] <- 0 
  tmp.df$delta <- tmp.df$weights - tmp.df$prev_weight 
  
  #merge CRIX weights with CC prices 
  df <- merge(tmp.df, CCs[,c("Id","prices", "Datetime")], 
              by.x =c("index_members", "date")  ,by.y=c("Id", "Datetime"))
  
  #merge with data frame from previous round
  df2 <- merge(df,df.prev[,c("quantity.a.r", "index_members")], by="index_members", all.x = T)
  df2[is.na(df2)] <- 0 
  names(df2)[names(df2)=="quantity.a.r"] <- "quantity.a.r.prev"
  
  #calculations
  df2$deposits <- crix.deposits$rw.diff[crix.deposits$date==date]
  
  df2$value.crix.pf.s.b.r <- df2$quantity.a.r.prev * df2$prices  
  #da fehlen die, die aus dem CRIX geflogen sind
  df2$value.crix.pf.aggr.b.r <- sum(df2$value.crix.pf.s.b.r) +
    sum(sub.df.single$quantity.a.r*sub.df.single$prices)
  
  df2$quantity <- ((df2$value.crix.pf.aggr.b.r + df2$deposits) * df2$weights) /df2$prices 
  df2$quantity.a.r <- df2$quantity * (1-fee_schedule(df2$quantity*df2$prices))
  
  df2$value.crix.pf.s.a.r <- df2$quantity.a.r * df2$prices   #irgendwas muss ich hier noch
  df2$value.crix.pf.aggr.a.r <- sum(df2$value.crix.pf.s.a.r)         #beachten       
  
  df2$scal.2 <- spread.df$relBTC.2[which(spread.df$Id %in% df2$index_members 
                                         & spread.df$Datetime %in% df2$date)]
  df2$scal.5 <- spread.df$relBTC.5[which(spread.df$Id %in% df2$index_members 
                                         & spread.df$Datetime %in% df2$date)]
  df2$scal.10 <- spread.df$relBTC.10[which(spread.df$Id %in% df2$index_members 
                                           & spread.df$Datetime %in% df2$date)]
  df2$spread.2 <- spreads(df2$quantity.a.r) * df2$scal.2 
  df2$spread.5 <- spreads(df2$quantity.a.r) * df2$scal.5 
  df2$spread.10 <- spreads(df2$quantity.a.r) * df2$scal.10 
  
  df2$costs <- (fee_schedule(df2$delta * df2$value.crix.pf.aggr.b.r)*
                  ((df2$value.crix.pf.aggr.b.r* df2$delta)/
                     (df2$deposits + df2$value.crix.pf.aggr.b.r* df2$delta))+
                  fee_schedule(df2$deposits * df2$weights))*
    (df2$deposits/
       (df2$deposits + df2$value.crix.pf.aggr.b.r* df2$delta))
  #df2$costs <- fee_schedule(df2$delta * df2$value.crix.pf.aggr.b.r) *
  #            (df2$delta * df2$value.crix.pf.aggr.b.r) +
  #          fee_schedule(df2$deposits * df2$weights) *
  #         df2$deposits * df2$weights
  
  
  df.prev <<- df2
  print(head(df.prev))
  return(df.prev)
}
#date <- "2020-08-01"
#date_minus1 <- "2020-07-01"
#test.rebalance <- rebalance("2020-08-01") #"2018-03-01"


#function that calls the above function and combines results
stepwise <- function(date){
  if(date == dates[2,1]){ # "2020-08-01"
    step1 <<- initial_invest(date_minus1)
    df.prev <<- step1
    step2 <- rebalance(date)
    step3 <-rbind(step1,step2)
    dropped_coins <<- rbind(dropped_coins, sub.df.single)
    return(step3)
    print(tail(step3))
  }
  else{ 
    step2 <<- rebalance(date)
    step3 <- rbind(step3, step2)
    dropped_coins <<- rbind(dropped_coins, sub.df.single)
    print(tail(step3))
    return(step3)
  }
}

#step3 <- stepwise("2020-08-01") 
#rm(date, date_minus1, step2, step3, df.prev)

#execute code: loop over all rebalancing dates    
dropped_coins <- data.frame()
for (date in as.factor(dates[2:nrow(dates),1])) { #  dates[2:4,1] dates[2:nrow(dates),1]
  date_minus1 <- dates[which(dates$d==date)-1,]   %>% as.character
  step3 <- stepwise(date)
}
#rm(df.prev, step3, sub.df.double)

#-------------------------------------------------------------------------------
##########
##graphs##
##########

#create all plots
source("graphs.R")
draw_plots()














```

automatically created on 2022-12-27