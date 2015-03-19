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

CUMULCORREL1 <- as.data.frame(timeSequence(from = "2010-01-04 09:29:00", to = "2010-01-04 13:15:00", by = 'min'))
CUMULCORREL2 <- as.data.frame(timeSequence(from = "2010-01-04 09:29:00", to = "2010-01-04 13:15:00", by = 'min'))
CUMULCORREL3 <- as.data.frame(timeSequence(from = "2010-01-04 09:29:00", to = "2010-01-04 13:15:00", by = 'min'))
for(i in 1:length(dates)){
    #This is already named 'DeliveryDates' upon loading. It must remember the name of the xts object it was saved from
    load(paste0('Contracts', as.character(dates[i]) ,".rda")) 

  #Load all the contracts on a single day
  for(j in 1:length(DeliveryDates)){
    #BBO
    load(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[j]), ".rda"))
    temp <- temp["T09:29:00/T13:16:00"] # Focus on the daytime session 9:30am - 1:15pm
    assign(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[j])), temp)
    
#     #Transactions
#     load(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[j]), ".rda"))
#     temp <- temp["T09:29:00/T13:16:00"] # Focus on the daytime session 9:30am - 1:15pm
#     assign(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[j])), temp)
  }
  
  rm(temp)

  #Now begin calculations
  # Identify nearby and two years of the forward maturities
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
  nearby_BID <- to.period(qnearby$BID, period = 'seconds', k = 1, OHLC = FALSE)
  plus1_BID <- to.period(qplus1$BID, period = 'seconds', k = 1, OHLC = FALSE)
  
  near_plus1 <- merge(nearby_BID, plus1_BID, all = TRUE, fill = NA, join = "outer", 
                retside = TRUE, retclass = "xts")

  near_plus1.df <- as.data.frame(near_plus1)
  cor(near_plus1.df, use = "pairwise.complete.obs")

  ep <- endpoints(near_plus1.df, 'minutes')
  correl1<- period.apply(near_plus1.df, INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
  CUMULCORREL1 <- cbind(CUMULCORREL1, as.data.frame(correl1$X2))

  #Nearby and plus2
  plus2_BID <- to.period(qplus1$BID, period = 'seconds', k = 1, OHLC = FALSE)
  
  near_plus2 <- merge(nearby_BID, plus2_BID, all = TRUE, fill = NA, join = "outer", 
                      retside = TRUE, retclass = "xts")
  
  near_plus2.df <- as.data.frame(near_plus2)
  cor(near_plus2.df, use = "pairwise.complete.obs")
  
  ep <- endpoints(near_plus2.df, 'minutes')
  correl2<- period.apply(near_plus2.df, INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
  CUMULCORREL2 <- cbind(CUMULCORREL2, as.data.frame(correl2$X2))

  #Nearby and plus3
  plus3_BID <- to.period(qplus3$BID, period = 'seconds', k = 1, OHLC = FALSE)
  
  near_plus3 <- merge(nearby_BID, plus3_BID, all = TRUE, fill = NA, join = "outer", 
                      retside = TRUE, retclass = "xts")
  
  near_plus3.df <- as.data.frame(near_plus3)
  cor(near_plus3.df, use = "pairwise.complete.obs")
  
  ep <- endpoints(near_plus3.df, 'minutes')
  correl3<- period.apply(near_plus3.df, INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
  CUMULCORREL3 <- cbind(CUMULCORREL3, as.data.frame(correl3$X2))

  
  # Delete every day's xts objects before moving on to next day to manage memory
  #rm(list = ls()[grep("^q_", ls())]) # Removes all varaibles that start with "q_"
  #rm(list = ls()[grep("^t_", ls())]) # Removes all varaibles that start with "q_"
}


#hist(as.numeric(CUMULCORREL1[50,2:4]))
#See ggplot2 - 





