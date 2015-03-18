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
  q_nearby <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[1])))
  q_plus1  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[2])))
  q_plus2  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[3])))
  q_plus3  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[4]))) 
  q_plus4  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[5])))
  q_plus5  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[6])))
  q_plus6  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[7])))
  q_plus7  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[8])))
  q_plus8  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[9])))
  q_plus9  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[10])))

  #   t_nearby <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[1])))
  #   t_plus1  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[2])))
  #   t_plus2  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[3])))
  #   t_plus3  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[4]))) 
  #   t_plus4  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[5])))
  #   t_plus5  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[6])))
  #   t_plus6  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[7])))
  #   t_plus7  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[8])))
  #   t_plus8  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[9])))
  #   t_plus9  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[10])))
  
  rm(list = ls()[grep("^q_", ls())]) # Removes all varaibles that start with "q_"
  rm(list = ls()[grep("^t_", ls())]) # Removes all varaibles that start with "q_"
  
  nearby_BID <- to.period(q_nearby$BID, period = 'seconds', k = 1, OHLC = FALSE)
  plus1_BID <- to.period(q_plus1$BID, period = 'seconds', k = 1, OHLC = FALSE)

  # Probably need to merge then drop so that the data line up.
  ccf(drop(nearby_BID), drop(plus1_BID), lag.max = NULL, type = "correlation",
    plot = TRUE, na.action = na.pass)
  
  
  # Delete every day's xts objects before moving on to next day to manage memory
  #rm(list = ls()[grep("^q_", ls())]) # Removes all varaibles that start with "q_"
  #rm(list = ls()[grep("^t_", ls())]) # Removes all varaibles that start with "q_"
}









