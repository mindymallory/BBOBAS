########################################################################################
# Script loads xts rda files that were written by BBO_sample.R, 
# then performs analysis on them for our paper. 
#
# Things to calculate:
# 1) Average quotes per second, organized by hour
#    This allows us to compare quoting activity volumes with those on equity markets 
#    Compare with Xiaoyang's paper 3 results. 
#    My impression of the data is that there is not a lot of 'quote stuffing'
#
# 2) Cross correlation between nearby and defferred revisions to the top of the book
#    a) Nearby offer to deferred offer, contemporaneous and lagged
#    b) Nearby bid to deferred bid, contemporaneous and lagged 
#    c) Nearby BAS to deferred BAS, contemporaneous and lagged
#    d) Are contemporaneous correlations high? 
#    e) How many lags (seconds) until top of book revisions are uncorrelated
#
# 3) Duration to next BAS revision
#
# Copyright [2015] [Mindy L. Mallory]

########################################################################################
library(timeDate)
library(xts)
library(reshape)
library(dplyr)
library(xtable)
library(highfrequency)
library(ggplot2)

setwd('C:/Users/mallorym/Dropbox/Market Microstructure Soybean Futures/BBO_sample') #Dropbox

# File naming convention for xts objects written to disk
# Transactions - t_%y%m%d_contract.rda E.g., t_100106_1003 contains trasactions for the March 2010 contract on Jan 6, 2010
# Best Bid Ofr - q_%y%m%d_contract.rda E.g., q_100106_1003 contains trasactions for the March 2010 contract on Jan 6, 2010
# Contract key - Contracts%y%m%d.rda   E.g., Contracts100106 records all contracts 'on the board' for that day.
# Bad Prices   - BADPRICES%y%m%d.rda   Any prices recorded below $1/bushel were recorded and saved into these files. 

# Build the file names to be imported
# Define the dates to loop over
yearstart <- 2010
yearend <- 2010
dates <- timeSequence(from = paste(yearstart, "-01-04", sep = ""), 
                      to = paste(yearend, "-01-06", sep = ""))

# Skipped April 5, 2010. There was some kind of quote spoofing algorithm generating a lot of quotes, posting
# and canceling offers at the best offer. Also it appears that trading was halted. Really I skipped it because the
# file was 12 times larger than the typical size and it was taking too long to process. Would make an interesting case
# study to go back and investigate.

# Code below requires dates to be integers, here we change the format
dates <- dates[isBizday(dates,holidayNYSE(yearstart:yearend))]
dates <- as.numeric(format(dates, format = "%y%m%d"))

# Initializes data frames where analysis is stored
CUMULCORREL1_BID      <- as.data.frame(timeSequence(from = "2010-01-04 09:39:59", to = "2010-01-04 13:19:59", by = '10 min'))
CUMULCORREL2_BID      <- as.data.frame(timeSequence(from = "2010-01-04 09:39:59", to = "2010-01-04 13:19:59", by = '10 min'))
CUMULCORREL3_BID      <- as.data.frame(timeSequence(from = "2010-01-04 09:39:59", to = "2010-01-04 13:19:59", by = '10 min'))
CUMULCORREL1_BID_rets <- as.data.frame(timeSequence(from = "2010-01-04 09:39:59", to = "2010-01-04 13:19:59", by = '10 min'))
CUMULCORREL2_BID_rets <- as.data.frame(timeSequence(from = "2010-01-04 09:39:59", to = "2010-01-04 13:19:59", by = '10 min'))
CUMULCORREL3_BID_rets <- as.data.frame(timeSequence(from = "2010-01-04 09:39:59", to = "2010-01-04 13:19:59", by = '10 min'))

# Clean up first column 
  colnames(CUMULCORREL1_BID) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL1_BID$TimeBins), 2, substr, 12, 19)
  row.names(CUMULCORREL1_BID) <- temp
  
  colnames(CUMULCORREL2_BID) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL2_BID$TimeBins), 2, substr, 12, 19)
  row.names(CUMULCORREL2_BID) <- temp
  
  colnames(CUMULCORREL3_BID) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL3_BID$TimeBins), 2, substr, 12, 19)
  row.names(CUMULCORREL3_BID) <- temp
  
  colnames(CUMULCORREL1_BID_rets) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL1_BID_rets), 2, substr, 12, 19)
  row.names(CUMULCORREL1_BID_rets) <- temp
  
  colnames(CUMULCORREL2_BID_rets) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL2_BID_rets), 2, substr, 12, 19)
  row.names(CUMULCORREL2_BID_rets) <- temp
  
  colnames(CUMULCORREL3_BID_rets) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL3_BID_rets), 2, substr, 12, 19)
  row.names(CUMULCORREL3_BID_rets) <- temp 

CUMULCORREL1_OFR      <- as.data.frame(timeSequence(from = "2010-01-04 09:39:59", to = "2010-01-04 13:19:59", by = '10 min'))
CUMULCORREL2_OFR      <- as.data.frame(timeSequence(from = "2010-01-04 09:39:59", to = "2010-01-04 13:19:59", by = '10 min'))
CUMULCORREL3_OFR      <- as.data.frame(timeSequence(from = "2010-01-04 09:39:59", to = "2010-01-04 13:19:59", by = '10 min'))
CUMULCORREL1_OFR_rets <- as.data.frame(timeSequence(from = "2010-01-04 09:39:59", to = "2010-01-04 13:19:59", by = '10 min'))
CUMULCORREL2_OFR_rets <- as.data.frame(timeSequence(from = "2010-01-04 09:39:59", to = "2010-01-04 13:19:59", by = '10 min'))
CUMULCORREL3_OFR_rets <- as.data.frame(timeSequence(from = "2010-01-04 09:39:59", to = "2010-01-04 13:19:59", by = '10 min'))

  # Clean up first column 
  colnames(CUMULCORREL1_OFR) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL1_OFR$TimeBins), 2, substr, 12, 19)
  row.names(CUMULCORREL1_OFR) <- temp
  
  colnames(CUMULCORREL2_OFR) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL2_OFR$TimeBins), 2, substr, 12, 19)
  row.names(CUMULCORREL2_OFR) <- temp
  
  colnames(CUMULCORREL3_OFR) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL3_OFR$TimeBins), 2, substr, 12, 19)
  row.names(CUMULCORREL3_OFR) <- temp
  
  colnames(CUMULCORREL1_OFR_rets) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL1_OFR_rets), 2, substr, 12, 19)
  row.names(CUMULCORREL1_OFR_rets) <- temp
  
  colnames(CUMULCORREL2_OFR_rets) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL2_OFR_rets), 2, substr, 12, 19)
  row.names(CUMULCORREL2_OFR_rets) <- temp
  
  colnames(CUMULCORREL3_OFR_rets) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL3_OFR_rets), 2, substr, 12, 19)
  row.names(CUMULCORREL3_OFR_rets) <- temp 

for(i in 1:length(dates)){
    #This is already named 'DeliveryDates' upon loading. It must remember the name of the xts object it was saved from
    load(paste0('Contracts', as.character(dates[i]) ,".rda")) 

  #Load all the contracts on a single day
  for(j in 1:length(DeliveryDates)){
    #BBO
    load(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[j]), ".rda"))
    temp <- temp["T09:30:00/T13:15:00"] # Focus on the daytime session 9:30am - 1:15pm
    assign(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[j])), temp)
    
#     #Transactions
#     load(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[j]), ".rda"))
#     temp <- temp["T09:29:00/T13:16:00"] # Focus on the daytime session 9:30am - 1:15pm
#     assign(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[j])), temp)
  }
  
  rm(temp)

  #Now begin calculations
  # Identify nearby and two years of the forward maturities
  # Need to write a little code to 'roll' before maturity month
  qnearby <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[1])))
  qplus1  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[2])))
  qplus2  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[3])))
  qplus3  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[4]))) 
  qplus4  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[5])))
  qplus5  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[6])))
  qplus6  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[7])))
  qplus7  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[8])))
  qplus8  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[9])))
  qplus9  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[10])))

  #   tnearby <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[1])))
  #   tplus1  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[2])))
  #   tplus2  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[3])))
  #   tplus3  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[4]))) 
  #   tplus4  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[5])))
  #   tplus5  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[6])))
  #   tplus6  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[7])))
  #   tplus7  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[8])))
  #   tplus8  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[9])))
  #   tplus9  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[10])))
  
  rm(list = ls()[grep("^q_", ls())]) # Removes all varaibles that start with "q_"
  rm(list = ls()[grep("^t_", ls())]) # Removes all varaibles that start with "q_"

# Nearby and plus1
########################################################################################  

  nearby_BID        <- to.period(qnearby$BID, period = 'seconds', k = 1, OHLC = FALSE)
  nearby_BID_rets   <- diff.xts(nearby_BID, lag = 1, differences = 1, arithmetic = TRUE, log = TRUE, na.pad = TRUE)
  non_zeros         <- index(nearby_BID_rets)[which(nearby_BID_rets != 0)]
  nsecs_to_update   <- difftime(non_zeros[1:(length(non_zeros)-1)], non_zeros[2:length(non_zeros)], unit='secs')
  nearby_BID_rets   <- subset(nearby_BID_rets, BID != 0)


  plus1_BID         <- to.period(qplus1$BID, period = 'seconds', k = 1, OHLC = FALSE)
  plus1_BID_rets    <- diff.xts(plus1_BID, lag = 1, differences = 1, arithmetic = TRUE, log = TRUE, na.pad = TRUE)
  non_zeros         <- index(plus1_BID_rets)[which(plus1_BID_rets != 0)]
  p1secs_to_update_BID  <-difftime(non_zeros[1:(length(non_zeros)-1)], non_zeros[2:length(non_zeros)], unit='secs')
  plus1_BID_rets    <- subset(plus1_BID_rets, BID !=0)

  nearby_OFR        <- to.period(qnearby$OFR, period = 'seconds', k = 1, OHLC = FALSE)
  nearby_OFR_rets   <- diff.xts(nearby_OFR, lag = 1, differences = 1, arithmetic = TRUE, log = TRUE, na.pad = TRUE)
  non_zeros         <- index(nearby_OFR_rets)[which(nearby_OFR_rets != 0)]
  nsecs_to_update   <-difftime(non_zeros[1:(length(non_zeros)-1)], non_zeros[2:length(non_zeros)], unit='secs')
  
  plus1_OFR         <- to.period(qplus1$OFR, period = 'seconds', k = 1, OHLC = FALSE)
  plus1_OFR_rets    <- diff.xts(plus1_OFR, lag = 1, differences = 1, arithmetic = TRUE, log = TRUE, na.pad = TRUE)
  non_zeros         <- index(plus1_OFR_rets)[which(plus1_OFR_rets != 0)]
  p1secs_to_update_OFR  <-difftime(non_zeros[1:(length(non_zeros)-1)], non_zeros[2:length(non_zeros)], unit='secs')


# Full day correlations
  # BIDS, Levels
  near_plus1_BID        <- merge(nearby_BID, plus1_BID, all = TRUE, fill = NA, join = "outer", retside = TRUE, 
                             retclass = "xts")                
  near_plus1_BID.df     <- as.data.frame(near_plus1_BID)
  cor(near_plus1_BID.df, use = "pairwise.complete.obs")

  # BIDS, 'Returns'
  near_plus1_BID_rets   <- merge(nearby_BID_rets, plus1_BID_rets, all = TRUE, fill = NA, join = "outer", 
                             retside = TRUE, retclass = "xts")                
  near_plus1_BID_rets.df<- as.data.frame(near_plus1_BID_rets)
  cor(near_plus1_BID_rets.df, use = "pairwise.complete.obs")

  # OFR, Levels
  near_plus1_OFR        <- merge(nearby_OFR, plus1_OFR, all = TRUE, fill = NA, join = "outer", retside = TRUE, 
                             retclass = "xts")                
  near_plus1_OFR.df     <- as.data.frame(near_plus1_OFR)
  cor(near_plus1_OFR.df, use = "pairwise.complete.obs")
  
  # 'Returns'
  near_plus1_OFR_rets   <- merge(nearby_OFR_rets, plus1_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                             retside = TRUE, retclass = "xts")                
  near_plus1_OFR_rets.df<- as.data.frame(near_plus1_OFR_rets)
  cor(near_plus1_OFR_rets.df, use = "pairwise.complete.obs")

# In time bins - ten minutes
# Date in the CUMULCORREL1_BID timestamp is irrelevant. We are creating 10 minute bins, in which we place 
# correlations from each day in columns. Last time stamp says 13:19:50, but really it is 13:15:50
# but in the timeSequence function, partial bins are not allowed and 13:09:59 to 13:15:59 is not a full 
# ten minutes.
  # BIDS, Levels
  ep <- endpoints(near_plus1_BID.df, 'minutes', k=10)
  correl1<- period.apply(near_plus1_BID.df, INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
  temp <- apply(as.data.frame(rownames(correl1)), 2, substr, 12, 19)
  row.names(correl1) <- temp
  CUMULCORREL1_BID <- cbind(CUMULCORREL1_BID, correl1$X2)
  
  # BIDS, 'Returns'
  ep <- endpoints(near_plus1_BID_rets.df, 'minutes', k=10)
  correl1_rets<- period.apply(near_plus1_BID_rets.df, INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
  temp <- apply(as.data.frame(rownames(correl1_rets)), 2, substr, 12, 19)
  row.names(correl1_rets) <- temp 
  CUMULCORREL1_BID_rets <- cbind(CUMULCORREL1_BID_rets, correl1$X2)
  
  # OFRS, Levels
  ep <- endpoints(near_plus1_OFR.df, 'minutes', k=10)
  correl1<- period.apply(near_plus1_OFR.df, INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
  temp <- apply(as.data.frame(rownames(correl1)), 2, substr, 12, 19)
  row.names(correl1) <- temp
  CUMULCORREL1_OFR <- cbind(CUMULCORREL1_OFR, correl1$X2)
  
  # OFRS, 'Returns'
  ep <- endpoints(near_plus1_OFR_rets.df, 'minutes', k=10)
  correl1_rets<- period.apply(near_plus1_OFR_rets.df, INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
  temp <- apply(as.data.frame(rownames(correl1_rets)), 2, substr, 12, 19)
  row.names(correl1_rets) <- temp 
  CUMULCORREL1_OFR_rets <- cbind(CUMULCORREL1_OFR_rets, correl1$X2)
######################################################################################## 

#Nearby and plus2
########################################################################################  
plus2_BID         <- to.period(qplus2$BID, period = 'seconds', k = 1, OHLC = FALSE)
plus2_BID_rets    <- diff.xts(plus2_BID, lag = 1, differences = 1, arithmetic = TRUE, log = TRUE, na.pad = TRUE)
non_zeros         <- index(plus2_BID_rets)[which(plus2_BID_rets != 0)]
p2secs_to_update_BID  <-difftime(non_zeros[1:(length(non_zeros)-1)], non_zeros[2:length(non_zeros)], unit='secs')
plus2_BID_rets    <- subset(plus2_BID_rets, BID !=0)

plus2_OFR         <- to.period(qplus2$OFR, period = 'seconds', k = 1, OHLC = FALSE)
plus2_OFR_rets    <- diff.xts(plus2_OFR, lag = 1, differences = 1, arithmetic = TRUE, log = TRUE, na.pad = TRUE)
non_zeros         <- index(plus2_OFR_rets)[which(plus2_OFR_rets != 0)]
p2secs_to_update_OFR  <-difftime(non_zeros[1:(length(non_zeros)-1)], non_zeros[2:length(non_zeros)], unit='secs')


# Full day correlations
  # BIDS, Levels
  near_plus2_BID        <- merge(nearby_BID, plus2_BID, all = TRUE, fill = NA, join = "outer", retside = TRUE, 
                             retclass = "xts")                
  near_plus2_BID.df     <- as.data.frame(near_plus2_BID)
  cor(near_plus2_BID.df, use = "pairwise.complete.obs")
  
  # BIDS, 'Returns'
  near_plus2_BID_rets   <- merge(nearby_BID_rets, plus2_BID_rets, all = TRUE, fill = NA, join = "outer", 
                             retside = TRUE, retclass = "xts")                
  near_plus2_BID_rets.df<- as.data.frame(near_plus2_BID_rets)
  cor(near_plus2_BID_rets.df, use = "pairwise.complete.obs")

  # OFRS, Levels
  near_plus2_OFR        <- merge(nearby_OFR, plus2_OFR, all = TRUE, fill = NA, join = "outer", retside = TRUE, 
                             retclass = "xts")                
  near_plus2_OFR.df     <- as.data.frame(near_plus2_OFR)
  cor(near_plus2_OFR.df, use = "pairwise.complete.obs")
  
  # OFRS, 'Returns'
  near_plus2_OFR_rets   <- merge(nearby_OFR_rets, plus2_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                             retside = TRUE, retclass = "xts")                
  near_plus2_OFR_rets.df<- as.data.frame(near_plus2_OFR_rets)
  cor(near_plus2_OFR_rets.df, use = "pairwise.complete.obs")

# In time bins - ten minutes
# Date in the CUMULCORREL1_BID timestamp is irrelevant. We are creating 10 minute bins, in which we place 
# correlations from each day in columns. Last time stamp says 13:19:50, but really it is 13:15:50
# but in the timeSequence function, partial bins are not allowed and 13:09:59 to 13:15:59 is not a full 
# ten minutes.
  # BIDS, Levels
  ep <- endpoints(near_plus2_BID.df, 'minutes', k=10)
  correl2<- period.apply(near_plus2_BID.df, INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
  temp <- apply(as.data.frame(rownames(correl2)), 2, substr, 12, 19)
  row.names(correl2) <- temp
  CUMULCORREL2_BID <- cbind(CUMULCORREL2_BID, correl2$X2)
  
  # BIDS, 'Returns'
  ep <- endpoints(near_plus2_BID_rets.df, 'minutes', k=10)
  correl2_rets<- period.apply(near_plus2_BID_rets.df, INDEX=ep, FUN=cor, use = "complete.obs")
  temp <- apply(as.data.frame(rownames(correl2_rets)), 2, substr, 12, 19)
  row.names(correl2_rets) <- temp 
  CUMULCORREL2_BID_rets <- cbind(CUMULCORREL2_BID_rets, correl2_rets$X2)


  # OFRS, Levels
  ep <- endpoints(near_plus2_OFR.df, 'minutes', k=10)
  correl2<- period.apply(near_plus2_OFR.df, INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
  temp <- apply(as.data.frame(rownames(correl2)), 2, substr, 12, 19)
  row.names(correl2) <- temp
  CUMULCORREL2_OFR <- cbind(CUMULCORREL2_OFR, correl2$X2)
  
  # OFRS, 'Returns'
  ep <- endpoints(near_plus2_OFR_rets.df, 'minutes', k=10)
  correl2_rets<- period.apply(near_plus2_OFR_rets.df, INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
  temp <- apply(as.data.frame(rownames(correl2_rets)), 2, substr, 12, 19)
  row.names(correl2_rets) <- temp 
  CUMULCORREL2_OFR_rets <- cbind(CUMULCORREL2_OFR_rets, correl2_rets$X2)
######################################################################################## 
 

#Nearby and plus3
########################################################################################  

  plus3_BID         <- to.period(qplus3$BID, period = 'seconds', k = 1, OHLC = FALSE)
  plus3_BID_rets    <- diff.xts(plus3_BID, lag = 1, differences = 1, arithmetic = TRUE, log = TRUE, na.pad = TRUE)
  non_zeros         <- index(plus3_BID_rets)[which(plus3_BID_rets != 0)]
  p3secs_to_update_BID  <-difftime(non_zeros[1:(length(non_zeros)-1)], non_zeros[2:length(non_zeros)], unit='secs')
  
  plus3_OFR         <- to.period(qplus3$OFR, period = 'seconds', k = 1, OHLC = FALSE)
  plus3_OFR_rets    <- diff.xts(plus3_OFR, lag = 1, differences = 1, arithmetic = TRUE, log = TRUE, na.pad = TRUE)
  non_zeros         <- index(plus3_OFR_rets)[which(plus3_OFR_rets != 0)]
  p3secs_to_update_OFR  <-difftime(non_zeros[1:(length(non_zeros)-1)], non_zeros[2:length(non_zeros)], unit='secs')

# Full day correlations
  # BIDS, Levels
  near_plus3_BID        <- merge(nearby_BID, plus3_BID, all = TRUE, fill = NA, join = "outer", retside = TRUE, 
                             retclass = "xts")                
  near_plus3_BID.df     <- as.data.frame(near_plus3_BID)
  cor(near_plus3_BID.df, use = "pairwise.complete.obs")
  
  # BIDS, 'Returns'
  near_plus3_BID_rets   <- merge(nearby_BID_rets, plus3_BID_rets, all = TRUE, fill = NA, join = "outer", 
                             retside = TRUE, retclass = "xts")                
  near_plus3_BID_rets.df<- as.data.frame(near_plus3_BID)
  cor(near_plus3_BID_rets.df, use = "pairwise.complete.obs")

# Full day correlations
  # OFRS, Levels
  near_plus3_OFR        <- merge(nearby_OFR, plus3_OFR, all = TRUE, fill = NA, join = "outer", retside = TRUE, 
                                 retclass = "xts")                
  near_plus3_OFR.df     <- as.data.frame(near_plus3_OFR)
  cor(near_plus3_OFR.df, use = "pairwise.complete.obs")
  
  # OFRS, 'Returns'
  near_plus3_OFR_rets   <- merge(nearby_OFR_rets, plus3_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                                 retside = TRUE, retclass = "xts")                
  near_plus3_OFR_rets.df<- as.data.frame(near_plus3_OFR)
  cor(near_plus3_OFR_rets.df, use = "pairwise.complete.obs")

# In time bins - ten minutes
# Date in the CUMULCORREL1_BID timestamp is irrelevant. We are creating 10 minute bins, in which we place 
# correlations from each day in columns. Last time stamp says 13:19:50, but really it is 13:15:50
# but in the timeSequence function, partial bins are not allowed and 13:09:59 to 13:15:59 is not a full 
# ten minutes.
# BIDS, Levels
ep <- endpoints(near_plus3_BID.df, 'minutes', k=10)
correl3<- period.apply(near_plus3_BID.df, INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.data.frame(rownames(correl3)), 2, substr, 12, 19)
row.names(correl3) <- temp
CUMULCORREL3_BID <- cbind(CUMULCORREL3_BID, correl3$X2)

# BIDS, 'Returns'
ep <- endpoints(near_plus3_BID_rets.df, 'minutes', k=10)
correl3_rets<- period.apply(near_plus3_BID_rets.df, INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.data.frame(rownames(correl3_rets)), 2, substr, 12, 19)
row.names(correl3_rets) <- temp 
CUMULCORREL3_BID_rets <- cbind(CUMULCORREL3_BID_rets, correl3$X2)

# OFRS, Levels
ep <- endpoints(near_plus3_OFR.df, 'minutes', k=10)
correl3<- period.apply(near_plus3_OFR.df, INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.data.frame(rownames(correl3)), 2, substr, 12, 19)
row.names(correl3) <- temp
CUMULCORREL3_OFR <- cbind(CUMULCORREL3_OFR, correl3$X2)

# OFRS, 'Returns'
ep <- endpoints(near_plus3_OFR_rets.df, 'minutes', k=10)
correl3_rets<- period.apply(near_plus3_OFR_rets.df, INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.data.frame(rownames(correl3_rets)), 2, substr, 12, 19)
row.names(correl3_rets) <- temp 
CUMULCORREL3_OFR_rets <- cbind(CUMULCORREL3_OFR_rets, correl3$X2)
######################################################################################## 


}


# Plotting summaries
  # Bids Contemporaneous Plus1
  colnames(CUMULCORREL1_BID_rets) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL1_BID_rets$TimeBins), 2, substr, 12, 19)
  colnames(temp) <- "TimeBins" # Name assignments seem redundent, but it wanted to override the name otherwise
  CUMULCORREL1_BID_rets$TimeBins <- factor(temp)
  
  CUMULCORREL1_BID_rets$MEANS <- apply(CUMULCORREL1_BID_rets[,2:dim(CUMULCORREL1_BID_rets)[2]], 1, mean, na.rm = TRUE)
  CUMULCORREL1_BID_rets$sdS <- apply(CUMULCORREL1_BID_rets[,2:dim(CUMULCORREL1_BID_rets)[2]], 1, sd, na.rm = TRUE) 
  CUMULCORREL1_BID_rets$contract <- factor("plus1")  

  # Bids Contemporaneous Plus2
  colnames(CUMULCORREL2_BID_rets) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL2_BID_rets$TimeBins), 2, substr, 12, 19)
  colnames(temp) <- "TimeBins" # Name assignments seem redundent, but it wanted to override the name otherwise
  CUMULCORREL2_BID_rets$TimeBins <- factor(temp)
  
  CUMULCORREL2_BID_rets$MEANS <- apply(CUMULCORREL2_BID_rets[,2:dim(CUMULCORREL2_BID_rets)[2]], 1, mean, na.rm = TRUE)
  CUMULCORREL2_BID_rets$sdS <- apply(CUMULCORREL2_BID_rets[,2:dim(CUMULCORREL2_BID_rets)[2]], 1, sd, na.rm = TRUE) 
  CUMULCORREL2_BID_rets$contract <- factor("plus2")

  # Bids Contemporaneous Plus2
  colnames(CUMULCORREL3_BID_rets) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL3_BID_rets$TimeBins), 2, substr, 12, 19)
  colnames(temp) <- "TimeBins" # Name assignments seem redundent, but it wanted to override the name otherwise
  CUMULCORREL3_BID_rets$TimeBins <- factor(temp)
  
  CUMULCORREL3_BID_rets$MEANS <- apply(CUMULCORREL3_BID_rets[,2:dim(CUMULCORREL3_BID_rets)[2]], 1, mean, na.rm = TRUE)
  CUMULCORREL3_BID_rets$sdS <- apply(CUMULCORREL3_BID_rets[,2:dim(CUMULCORREL3_BID_rets)[2]], 1, sd, na.rm = TRUE) 
  CUMULCORREL3_BID_rets$contract <- factor("plus3")


  CUMULCORREL_BID_rets <- rbind(CUMULCORREL1_BID_rets, CUMULCORREL2_BID_rets, CUMULCORREL3_BID_rets)
  
  pd <- position_dodge(0.4)
  MAXES <- min(CUMULCORREL_BID_rets$MEANS - CUMULCORREL_BID_rets$sdS,1)
  MINS <- max(CUMULCORREL_BID_rets$MEANS - CUMULCORREL_BID_rets$sdS,0)
  Bid_plot <- ggplot(CUMULCORREL_BID_rets, aes(TimeBins, MEANS, ymin = MEANS-sdS, 
                                                 ymax = MEANS+sdS, colour=contract, group=contract) ) + 
    geom_errorbar(size=1, position=pd) +
    geom_point(size=4, position=pd) + 
    ggtitle('Contemporanious Correleation in Bids') +
    #theme(axis.text.x=element_text(angle=45), axis.title.x=element_blank()) +
    theme_bw() +
    #scale_colour_grey() + 
    ylab("Correlation")

    Bid_plot




