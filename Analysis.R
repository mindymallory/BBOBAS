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
library(gridExtra)
library(magrittr)

options(scipen = 999)  # Scientific notation is no good in tables for reading p_values

#setwd('C:/Users/mallorym/Dropbox/Market Microstructure Soybean Futures/BBO_sample') # Dropbox
setwd('C:/Users/mallorym/BBOCORNDATA/ProcessedData') # Office PC

# File naming convention for xts objects written to disk
# Transactions - t_%y%m%d_contract.rda E.g., t_100106_1003 contains trasactions for the March 2010 contract on Jan 6, 2010
# Best Bid Ofr - q_%y%m%d_contract.rda E.g., q_100106_1003 contains trasactions for the March 2010 contract on Jan 6, 2010
# Contract key - Contracts%y%m%d.rda   E.g., Contracts100106 records all contracts 'on the board' for that day.
# Bad Prices   - BADPRICES%y%m%d.rda   Any prices recorded below $1/bushel were recorded and saved into these files. 

# Build the file names to be imported
# Define the dates to loop over
yearstart <- 2008
yearend <- 2011
dates <- timeSequence(from = paste(yearstart, "-01-14", sep = ""), 
                      to = paste(yearend, "-11-04", sep = ""))



# Code below requires dates to be integers, here we change the format
dates <- dates[isBizday(dates,holidayNYSE(yearstart:yearend))]
dates <- as.numeric(format(dates, format = "%y%m%d"))
  dates <- subset(dates, dates != 100405)
  # Skipped April 5, 2010. There was some kind of quote spoofing algorithm generating a lot of quotes, posting
  # and canceling offers at the best offer. Also it appears that trading was halted. Really I skipped it because the
  # file was 12 times larger than the typical size and it was taking too long to process. Would make an interesting case
  # study to go back and investigate.

# Delete Limit days when there were no quote revisions. Couldn't make all delitions in one line; not sure why not.
dates <- subset(dates, dates != c('100112'))# Revision to Crop Production report
dates <- subset(dates, dates != c('101008'))
dates <- subset(dates, dates != c('111209'))
dates <- subset(dates, dates != c('110331')) # Prospective Plantings report (Not included in our report days)
dates <- subset(dates, dates != c('110630')) # Planted Acres report 
dates <- subset(dates, dates != c('110705')) # Light trade after the 4th holiday. No trades or quotes for '3 deferred' which
                                             # would have been the March 2012 contract.
dates <- subset(dates, dates != c('80707')) # After 4th holiday, Informa came out with larger than WASDE forecast yeild. \
dates <- subset(dates, dates != c('90102')) 
dates <- subset(dates, dates != c('90103'))
dates <- subset(dates, dates != c('90104'))
dates <- subset(dates, dates != c('90105'))
dates <- subset(dates, dates != c('90106'))
dates <- subset(dates, dates != c('90107'))
dates <- subset(dates, dates != c('90108'))
dates <- subset(dates, dates != c('90109'))
dates <- subset(dates, dates != c('90110'))
dates <- subset(dates, dates != c('90111'))
dates <- subset(dates, dates != c('90112'))
dates <- subset(dates, dates != c('90113')) # No clue why but these dates are missing from the dataset. 
# Initializes data frames where analysis is stored
#########################################################
CUMULCORREL1_BID      <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_BID      <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_BID      <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_BID_rets <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_BID_rets <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_BID_rets <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_BID_rets_no0s <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_BID_rets_no0s <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_BID_rets_no0s <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_BID_rets_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_BID_rets_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_BID_rets_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_BID_rets_no0s_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_BID_rets_no0s_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_BID_rets_no0s_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_BID_rets_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_BID_rets_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_BID_rets_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_BID_rets_no0s_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_BID_rets_no0s_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_BID_rets_no0s_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1N_BID_rets_1sec     <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1N_BID_rets_10sec     <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))

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

  colnames(CUMULCORREL1_BID_rets_no0s) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL1_BID_rets_no0s), 2, substr, 12, 19)
  row.names(CUMULCORREL1_BID_rets_no0s) <- temp
  
  colnames(CUMULCORREL2_BID_rets_no0s) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL2_BID_rets_no0s), 2, substr, 12, 19)
  row.names(CUMULCORREL2_BID_rets_no0s) <- temp
  
  colnames(CUMULCORREL3_BID_rets_no0s) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL3_BID_rets_no0s), 2, substr, 12, 19)
  row.names(CUMULCORREL3_BID_rets_no0s) <- temp 

  colnames(CUMULCORREL1_BID_rets_1sec) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL1_BID_rets_1sec), 2, substr, 12, 19)
  row.names(CUMULCORREL1_BID_rets_1sec) <- temp
  
  colnames(CUMULCORREL2_BID_rets_1sec) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL2_BID_rets_1sec), 2, substr, 12, 19)
  row.names(CUMULCORREL2_BID_rets_1sec) <- temp
  
  colnames(CUMULCORREL3_BID_rets_1sec) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL3_BID_rets_1sec), 2, substr, 12, 19)
  row.names(CUMULCORREL3_BID_rets_1sec) <- temp 
  
  colnames(CUMULCORREL1_BID_rets_no0s_1sec) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL1_BID_rets_no0s_1sec), 2, substr, 12, 19)
  row.names(CUMULCORREL1_BID_rets_no0s_1sec) <- temp
  
  colnames(CUMULCORREL2_BID_rets_no0s_1sec) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL2_BID_rets_no0s_1sec), 2, substr, 12, 19)
  row.names(CUMULCORREL2_BID_rets_no0s_1sec) <- temp
  
  colnames(CUMULCORREL3_BID_rets_no0s_1sec) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL3_BID_rets_no0s_1sec), 2, substr, 12, 19)
  row.names(CUMULCORREL3_BID_rets_no0s_1sec) <- temp 
  
  colnames(CUMULCORREL1_BID_rets_10sec) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL1_BID_rets_10sec), 2, substr, 12, 19)
  row.names(CUMULCORREL1_BID_rets_10sec) <- temp
  
  colnames(CUMULCORREL2_BID_rets_10sec) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL2_BID_rets_10sec), 2, substr, 12, 19)
  row.names(CUMULCORREL2_BID_rets_10sec) <- temp
  
  colnames(CUMULCORREL3_BID_rets_10sec) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL3_BID_rets_10sec), 2, substr, 12, 19)
  row.names(CUMULCORREL3_BID_rets_10sec) <- temp 
  
  colnames(CUMULCORREL1_BID_rets_no0s_10sec) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL1_BID_rets_no0s_10sec), 2, substr, 12, 19)
  row.names(CUMULCORREL1_BID_rets_no0s_10sec) <- temp
  
  colnames(CUMULCORREL2_BID_rets_no0s_10sec) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL2_BID_rets_no0s_10sec), 2, substr, 12, 19)
  row.names(CUMULCORREL2_BID_rets_no0s_10sec) <- temp
  
  colnames(CUMULCORREL3_BID_rets_no0s_10sec) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL3_BID_rets_no0s_10sec), 2, substr, 12, 19)
  row.names(CUMULCORREL3_BID_rets_no0s_10sec) <- temp 
  
  colnames(CUMULCORREL1N_BID_rets_1sec) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL1N_BID_rets_1sec), 2, substr, 12, 19)
  row.names(CUMULCORREL1N_BID_rets_1sec) <- temp
  
  colnames(CUMULCORREL1N_BID_rets_10sec) <- "TimeBins"
  temp <- apply(as.data.frame(CUMULCORREL1N_BID_rets_10sec), 2, substr, 12, 19)
  row.names(CUMULCORREL1N_BID_rets_10sec) <- temp
  
  
CUMULCORREL1_OFR      <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_OFR      <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_OFR      <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_OFR_rets <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_OFR_rets <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_OFR_rets <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_OFR_rets_no0s <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_OFR_rets_no0s <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_OFR_rets_no0s <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_OFR_rets_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_OFR_rets_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_OFR_rets_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_OFR_rets_no0s_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_OFR_rets_no0s_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_OFR_rets_no0s_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_OFR_rets_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_OFR_rets_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_OFR_rets_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_OFR_rets_no0s_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_OFR_rets_no0s_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_OFR_rets_no0s_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1N_OFR_rets_1sec     <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1N_OFR_rets_10sec     <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))


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

colnames(CUMULCORREL1_OFR_rets_no0s) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL1_OFR_rets_no0s), 2, substr, 12, 19)
row.names(CUMULCORREL1_OFR_rets_no0s) <- temp

colnames(CUMULCORREL2_OFR_rets_no0s) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL2_OFR_rets_no0s), 2, substr, 12, 19)
row.names(CUMULCORREL2_OFR_rets_no0s) <- temp

colnames(CUMULCORREL3_OFR_rets_no0s) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL3_OFR_rets_no0s), 2, substr, 12, 19)
row.names(CUMULCORREL3_OFR_rets_no0s) <- temp 

colnames(CUMULCORREL1_OFR_rets_1sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL1_OFR_rets_1sec), 2, substr, 12, 19)
row.names(CUMULCORREL1_OFR_rets_1sec) <- temp

colnames(CUMULCORREL2_OFR_rets_1sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL2_OFR_rets_1sec), 2, substr, 12, 19)
row.names(CUMULCORREL2_OFR_rets_1sec) <- temp

colnames(CUMULCORREL3_OFR_rets_1sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL3_OFR_rets_1sec), 2, substr, 12, 19)
row.names(CUMULCORREL3_OFR_rets_1sec) <- temp 

colnames(CUMULCORREL1_OFR_rets_no0s_1sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL1_OFR_rets_no0s_1sec), 2, substr, 12, 19)
row.names(CUMULCORREL1_OFR_rets_no0s_1sec) <- temp

colnames(CUMULCORREL2_OFR_rets_no0s_1sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL2_OFR_rets_no0s_1sec), 2, substr, 12, 19)
row.names(CUMULCORREL2_OFR_rets_no0s_1sec) <- temp

colnames(CUMULCORREL3_OFR_rets_no0s_1sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL3_OFR_rets_no0s_1sec), 2, substr, 12, 19)
row.names(CUMULCORREL3_OFR_rets_no0s_1sec) <- temp 

colnames(CUMULCORREL1_OFR_rets_10sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL1_OFR_rets_10sec), 2, substr, 12, 19)
row.names(CUMULCORREL1_OFR_rets_10sec) <- temp

colnames(CUMULCORREL2_OFR_rets_10sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL2_OFR_rets_10sec), 2, substr, 12, 19)
row.names(CUMULCORREL2_OFR_rets_10sec) <- temp

colnames(CUMULCORREL3_OFR_rets_10sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL3_OFR_rets_10sec), 2, substr, 12, 19)
row.names(CUMULCORREL3_OFR_rets_10sec) <- temp 

colnames(CUMULCORREL1_OFR_rets_no0s_10sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL1_OFR_rets_no0s_10sec), 2, substr, 12, 19)
row.names(CUMULCORREL1_OFR_rets_no0s_10sec) <- temp

colnames(CUMULCORREL2_OFR_rets_no0s_10sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL2_OFR_rets_no0s_10sec), 2, substr, 12, 19)
row.names(CUMULCORREL2_OFR_rets_no0s_10sec) <- temp

colnames(CUMULCORREL3_OFR_rets_no0s_10sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL3_OFR_rets_no0s_10sec), 2, substr, 12, 19)
row.names(CUMULCORREL3_OFR_rets_no0s_10sec) <- temp 

colnames(CUMULCORREL1N_OFR_rets_1sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL1N_OFR_rets_1sec), 2, substr, 12, 19)
row.names(CUMULCORREL1N_OFR_rets_1sec) <- temp

colnames(CUMULCORREL1N_OFR_rets_10sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL1N_OFR_rets_10sec), 2, substr, 12, 19)
row.names(CUMULCORREL1N_OFR_rets_10sec) <- temp


CUMULCORREL1_BIDOFR      <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_BIDOFR      <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_BIDOFR      <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_BIDOFR_rets <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_BIDOFR_rets <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_BIDOFR_rets <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_BIDOFR_rets_no0s <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_BIDOFR_rets_no0s <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_BIDOFR_rets_no0s <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_BIDOFR_rets_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_BIDOFR_rets_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_BIDOFR_rets_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_BIDOFR_rets_no0s_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_BIDOFR_rets_no0s_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_BIDOFR_rets_no0s_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_BIDOFR_rets_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_BIDOFR_rets_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_BIDOFR_rets_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_BIDOFR_rets_no0s_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_BIDOFR_rets_no0s_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_BIDOFR_rets_no0s_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))

# Clean up first column 
colnames(CUMULCORREL1_BIDOFR) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL1_BIDOFR$TimeBins), 2, substr, 12, 19)
row.names(CUMULCORREL1_BIDOFR) <- temp

colnames(CUMULCORREL2_BIDOFR) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL2_BIDOFR$TimeBins), 2, substr, 12, 19)
row.names(CUMULCORREL2_BIDOFR) <- temp

colnames(CUMULCORREL3_BIDOFR) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL3_BIDOFR$TimeBins), 2, substr, 12, 19)
row.names(CUMULCORREL3_BIDOFR) <- temp

colnames(CUMULCORREL1_BIDOFR_rets) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL1_BIDOFR_rets), 2, substr, 12, 19)
row.names(CUMULCORREL1_BIDOFR_rets) <- temp

colnames(CUMULCORREL2_BIDOFR_rets) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL2_BIDOFR_rets), 2, substr, 12, 19)
row.names(CUMULCORREL2_BIDOFR_rets) <- temp

colnames(CUMULCORREL3_BIDOFR_rets) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL3_BIDOFR_rets), 2, substr, 12, 19)
row.names(CUMULCORREL3_BIDOFR_rets) <- temp 

colnames(CUMULCORREL1_BIDOFR_rets_no0s) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL1_BIDOFR_rets_no0s), 2, substr, 12, 19)
row.names(CUMULCORREL1_BIDOFR_rets_no0s) <- temp

colnames(CUMULCORREL2_BIDOFR_rets_no0s) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL2_BIDOFR_rets_no0s), 2, substr, 12, 19)
row.names(CUMULCORREL2_BIDOFR_rets_no0s) <- temp

colnames(CUMULCORREL3_BIDOFR_rets_no0s) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL3_BIDOFR_rets_no0s), 2, substr, 12, 19)
row.names(CUMULCORREL3_BIDOFR_rets_no0s) <- temp 

colnames(CUMULCORREL1_BIDOFR_rets_1sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL1_BIDOFR_rets_1sec), 2, substr, 12, 19)
row.names(CUMULCORREL1_BIDOFR_rets_1sec) <- temp

colnames(CUMULCORREL2_BIDOFR_rets_1sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL2_BIDOFR_rets_1sec), 2, substr, 12, 19)
row.names(CUMULCORREL2_BIDOFR_rets_1sec) <- temp

colnames(CUMULCORREL3_BIDOFR_rets_1sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL3_BIDOFR_rets_1sec), 2, substr, 12, 19)
row.names(CUMULCORREL3_BIDOFR_rets_1sec) <- temp 

colnames(CUMULCORREL1_BIDOFR_rets_no0s_1sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL1_BIDOFR_rets_no0s_1sec), 2, substr, 12, 19)
row.names(CUMULCORREL1_BIDOFR_rets_no0s_1sec) <- temp

colnames(CUMULCORREL2_BIDOFR_rets_no0s_1sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL2_BIDOFR_rets_no0s_1sec), 2, substr, 12, 19)
row.names(CUMULCORREL2_BIDOFR_rets_no0s_1sec) <- temp

colnames(CUMULCORREL3_BIDOFR_rets_no0s_1sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL3_BIDOFR_rets_no0s_1sec), 2, substr, 12, 19)
row.names(CUMULCORREL3_BIDOFR_rets_no0s_1sec) <- temp 

colnames(CUMULCORREL1_BIDOFR_rets_10sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL1_BIDOFR_rets_10sec), 2, substr, 12, 19)
row.names(CUMULCORREL1_BIDOFR_rets_10sec) <- temp

colnames(CUMULCORREL2_BIDOFR_rets_10sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL2_BIDOFR_rets_10sec), 2, substr, 12, 19)
row.names(CUMULCORREL2_BIDOFR_rets_10sec) <- temp

colnames(CUMULCORREL3_BIDOFR_rets_10sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL3_BIDOFR_rets_10sec), 2, substr, 12, 19)
row.names(CUMULCORREL3_BIDOFR_rets_10sec) <- temp 

colnames(CUMULCORREL1_BIDOFR_rets_no0s_10sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL1_BIDOFR_rets_no0s_10sec), 2, substr, 12, 19)
row.names(CUMULCORREL1_BIDOFR_rets_no0s_10sec) <- temp

colnames(CUMULCORREL2_BIDOFR_rets_no0s_10sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL2_BIDOFR_rets_no0s_10sec), 2, substr, 12, 19)
row.names(CUMULCORREL2_BIDOFR_rets_no0s_10sec) <- temp

colnames(CUMULCORREL3_BIDOFR_rets_no0s_10sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL3_BIDOFR_rets_no0s_10sec), 2, substr, 12, 19)
row.names(CUMULCORREL3_BIDOFR_rets_no0s_10sec) <- temp 

CUMULCORREL1_OFRBID      <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_OFRBID      <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_OFRBID      <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_OFRBID_rets <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_OFRBID_rets <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_OFRBID_rets <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_OFRBID_rets_no0s <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_OFRBID_rets_no0s <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_OFRBID_rets_no0s <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_OFRBID_rets_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_OFRBID_rets_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_OFRBID_rets_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_OFRBID_rets_no0s_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_OFRBID_rets_no0s_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_OFRBID_rets_no0s_1sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_OFRBID_rets_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_OFRBID_rets_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_OFRBID_rets_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL1_OFRBID_rets_no0s_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL2_OFRBID_rets_no0s_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))
CUMULCORREL3_OFRBID_rets_no0s_10sec <- as.data.frame(timeSequence(from = "2010-01-04 09:30:00", to = "2010-01-04 13:20:00", by = '10 min'))

# Clean up first column 
colnames(CUMULCORREL1_OFRBID) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL1_OFRBID$TimeBins), 2, substr, 12, 19)
row.names(CUMULCORREL1_OFRBID) <- temp

colnames(CUMULCORREL2_OFRBID) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL2_OFRBID$TimeBins), 2, substr, 12, 19)
row.names(CUMULCORREL2_OFRBID) <- temp

colnames(CUMULCORREL3_OFRBID) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL3_OFRBID$TimeBins), 2, substr, 12, 19)
row.names(CUMULCORREL3_OFRBID) <- temp

colnames(CUMULCORREL1_OFRBID_rets) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL1_OFRBID_rets), 2, substr, 12, 19)
row.names(CUMULCORREL1_OFRBID_rets) <- temp

colnames(CUMULCORREL2_OFRBID_rets) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL2_OFRBID_rets), 2, substr, 12, 19)
row.names(CUMULCORREL2_OFRBID_rets) <- temp

colnames(CUMULCORREL3_OFRBID_rets) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL3_OFRBID_rets), 2, substr, 12, 19)
row.names(CUMULCORREL3_OFRBID_rets) <- temp 

colnames(CUMULCORREL1_OFRBID_rets_no0s) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL1_OFRBID_rets_no0s), 2, substr, 12, 19)
row.names(CUMULCORREL1_OFRBID_rets_no0s) <- temp

colnames(CUMULCORREL2_OFRBID_rets_no0s) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL2_OFRBID_rets_no0s), 2, substr, 12, 19)
row.names(CUMULCORREL2_OFRBID_rets_no0s) <- temp

colnames(CUMULCORREL3_OFRBID_rets_no0s) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL3_OFRBID_rets_no0s), 2, substr, 12, 19)
row.names(CUMULCORREL3_OFRBID_rets_no0s) <- temp 

colnames(CUMULCORREL1_OFRBID_rets_1sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL1_OFRBID_rets_1sec), 2, substr, 12, 19)
row.names(CUMULCORREL1_OFRBID_rets_1sec) <- temp

colnames(CUMULCORREL2_OFRBID_rets_1sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL2_OFRBID_rets_1sec), 2, substr, 12, 19)
row.names(CUMULCORREL2_OFRBID_rets_1sec) <- temp

colnames(CUMULCORREL3_OFRBID_rets_1sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL3_OFRBID_rets_1sec), 2, substr, 12, 19)
row.names(CUMULCORREL3_OFRBID_rets_1sec) <- temp 

colnames(CUMULCORREL1_OFRBID_rets_no0s_1sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL1_OFRBID_rets_no0s_1sec), 2, substr, 12, 19)
row.names(CUMULCORREL1_OFRBID_rets_no0s_1sec) <- temp

colnames(CUMULCORREL2_OFRBID_rets_no0s_1sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL2_OFRBID_rets_no0s_1sec), 2, substr, 12, 19)
row.names(CUMULCORREL2_OFRBID_rets_no0s_1sec) <- temp

colnames(CUMULCORREL3_OFRBID_rets_no0s_1sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL3_OFRBID_rets_no0s_1sec), 2, substr, 12, 19)
row.names(CUMULCORREL3_OFRBID_rets_no0s_1sec) <- temp 

colnames(CUMULCORREL1_OFRBID_rets_10sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL1_OFRBID_rets_10sec), 2, substr, 12, 19)
row.names(CUMULCORREL1_OFRBID_rets_10sec) <- temp

colnames(CUMULCORREL2_OFRBID_rets_10sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL2_OFRBID_rets_10sec), 2, substr, 12, 19)
row.names(CUMULCORREL2_OFRBID_rets_10sec) <- temp

colnames(CUMULCORREL3_OFRBID_rets_10sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL3_OFRBID_rets_10sec), 2, substr, 12, 19)
row.names(CUMULCORREL3_OFRBID_rets_10sec) <- temp 

colnames(CUMULCORREL1_OFRBID_rets_no0s_10sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL1_OFRBID_rets_no0s_10sec), 2, substr, 12, 19)
row.names(CUMULCORREL1_OFRBID_rets_no0s_10sec) <- temp

colnames(CUMULCORREL2_OFRBID_rets_no0s_10sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL2_OFRBID_rets_no0s_10sec), 2, substr, 12, 19)
row.names(CUMULCORREL2_OFRBID_rets_no0s_10sec) <- temp

colnames(CUMULCORREL3_OFRBID_rets_no0s_10sec) <- "TimeBins"
temp <- apply(as.data.frame(CUMULCORREL3_OFRBID_rets_no0s_10sec), 2, substr, 12, 19)
row.names(CUMULCORREL3_OFRBID_rets_no0s_10sec) <- temp 

# Create TimeBins as factor allows for merging over days easily
CUMULCORREL1_BID_rets$TimeBins <- factor(row.names(CUMULCORREL1_BID_rets))
CUMULCORREL1_BID_rets_1sec$TimeBins <- factor(row.names(CUMULCORREL1_BID_rets_1sec))
CUMULCORREL1_BID_rets_10sec$TimeBins <- factor(row.names(CUMULCORREL1_BID_rets_10sec))
CUMULCORREL1_BID_rets_no0s$TimeBins <- factor(row.names(CUMULCORREL1_BID_rets_no0s))
CUMULCORREL1N_BID_rets_1sec$TimeBins <- factor(row.names(CUMULCORREL1N_BID_rets_1sec))
CUMULCORREL1N_BID_rets_10sec$TimeBins <- factor(row.names(CUMULCORREL1N_BID_rets_10sec))


CUMULCORREL1_OFR_rets$TimeBins <- factor(row.names(CUMULCORREL1_OFR_rets))
CUMULCORREL1_OFR_rets_1sec$TimeBins <- factor(row.names(CUMULCORREL1_OFR_rets_1sec))
CUMULCORREL1_OFR_rets_10sec$TimeBins <- factor(row.names(CUMULCORREL1_OFR_rets_10sec))
CUMULCORREL1_OFR_rets_no0s$TimeBins <- factor(row.names(CUMULCORREL1_OFR_rets_no0s))
CUMULCORREL1N_OFR_rets_1sec$TimeBins <- factor(row.names(CUMULCORREL1N_OFR_rets_1sec))
CUMULCORREL1N_OFR_rets_10sec$TimeBins <- factor(row.names(CUMULCORREL1N_OFR_rets_10sec))


CUMULCORREL2_BID_rets$TimeBins <- factor(row.names(CUMULCORREL2_BID_rets))
CUMULCORREL2_BID_rets_1sec$TimeBins <- factor(row.names(CUMULCORREL2_BID_rets_1sec))
CUMULCORREL2_BID_rets_10sec$TimeBins <- factor(row.names(CUMULCORREL2_BID_rets_10sec))
CUMULCORREL2_BID_rets_no0s$TimeBins <- factor(row.names(CUMULCORREL2_BID_rets_no0s))

CUMULCORREL2_OFR_rets$TimeBins <- factor(row.names(CUMULCORREL2_OFR_rets))
CUMULCORREL2_OFR_rets_1sec$TimeBins <- factor(row.names(CUMULCORREL2_OFR_rets_1sec))
CUMULCORREL2_OFR_rets_10sec$TimeBins <- factor(row.names(CUMULCORREL2_OFR_rets_10sec))
CUMULCORREL2_OFR_rets_no0s$TimeBins <- factor(row.names(CUMULCORREL2_OFR_rets_no0s))

CUMULCORREL3_BID_rets$TimeBins <- factor(row.names(CUMULCORREL3_BID_rets))
CUMULCORREL3_BID_rets_1sec$TimeBins <- factor(row.names(CUMULCORREL3_BID_rets_1sec))
CUMULCORREL3_BID_rets_10sec$TimeBins <- factor(row.names(CUMULCORREL3_BID_rets_10sec))
CUMULCORREL3_BID_rets_no0s$TimeBins <- factor(row.names(CUMULCORREL3_BID_rets_no0s))

CUMULCORREL3_OFR_rets$TimeBins <- factor(row.names(CUMULCORREL3_OFR_rets))
CUMULCORREL3_OFR_rets_1sec$TimeBins <- factor(row.names(CUMULCORREL3_OFR_rets_1sec))
CUMULCORREL3_OFR_rets_10sec$TimeBins <- factor(row.names(CUMULCORREL3_OFR_rets_10sec))
CUMULCORREL3_OFR_rets_no0s$TimeBins <- factor(row.names(CUMULCORREL3_OFR_rets_no0s))

CUMULCORREL1_BIDOFR_rets$TimeBins <- factor(row.names(CUMULCORREL1_BIDOFR_rets))
CUMULCORREL1_BIDOFR_rets_1sec$TimeBins <- factor(row.names(CUMULCORREL1_BIDOFR_rets_1sec))
CUMULCORREL1_BIDOFR_rets_10sec$TimeBins <- factor(row.names(CUMULCORREL1_BIDOFR_rets_10sec))
CUMULCORREL1_BIDOFR_rets_no0s$TimeBins <- factor(row.names(CUMULCORREL1_BIDOFR_rets_no0s))

CUMULCORREL2_BIDOFR_rets$TimeBins <- factor(row.names(CUMULCORREL2_BIDOFR_rets))
CUMULCORREL2_BIDOFR_rets_1sec$TimeBins <- factor(row.names(CUMULCORREL2_BIDOFR_rets_1sec))
CUMULCORREL2_BIDOFR_rets_10sec$TimeBins <- factor(row.names(CUMULCORREL2_BIDOFR_rets_10sec))
CUMULCORREL2_BIDOFR_rets_no0s$TimeBins <- factor(row.names(CUMULCORREL2_BIDOFR_rets_no0s))

CUMULCORREL3_BIDOFR_rets$TimeBins <- factor(row.names(CUMULCORREL3_BIDOFR_rets))
CUMULCORREL3_BIDOFR_rets_1sec$TimeBins <- factor(row.names(CUMULCORREL3_BIDOFR_rets_1sec))
CUMULCORREL3_BIDOFR_rets_10sec$TimeBins <- factor(row.names(CUMULCORREL3_BIDOFR_rets_10sec))
CUMULCORREL3_BIDOFR_rets_no0s$TimeBins <- factor(row.names(CUMULCORREL3_BIDOFR_rets_no0s))

CUMULCORREL1_OFRBID_rets$TimeBins <- factor(row.names(CUMULCORREL1_OFRBID_rets))
CUMULCORREL1_OFRBID_rets_1sec$TimeBins <- factor(row.names(CUMULCORREL1_OFRBID_rets_1sec))
CUMULCORREL1_OFRBID_rets_10sec$TimeBins <- factor(row.names(CUMULCORREL1_OFRBID_rets_10sec))
CUMULCORREL1_OFRBID_rets_no0s$TimeBins <- factor(row.names(CUMULCORREL1_OFRBID_rets_no0s))

CUMULCORREL2_OFRBID_rets$TimeBins <- factor(row.names(CUMULCORREL2_OFRBID_rets))
CUMULCORREL2_OFRBID_rets_1sec$TimeBins <- factor(row.names(CUMULCORREL2_OFRBID_rets_1sec))
CUMULCORREL2_OFRBID_rets_10sec$TimeBins <- factor(row.names(CUMULCORREL2_OFRBID_rets_10sec))
CUMULCORREL2_OFRBID_rets_no0s$TimeBins <- factor(row.names(CUMULCORREL2_OFRBID_rets_no0s))

CUMULCORREL3_OFRBID_rets$TimeBins <- factor(row.names(CUMULCORREL3_OFRBID_rets))
CUMULCORREL3_OFRBID_rets_1sec$TimeBins <- factor(row.names(CUMULCORREL3_OFRBID_rets_1sec))
CUMULCORREL3_OFRBID_rets_10sec$TimeBins <- factor(row.names(CUMULCORREL3_OFRBID_rets_10sec))
CUMULCORREL3_OFRBID_rets_no0s$TimeBins <- factor(row.names(CUMULCORREL3_OFRBID_rets_no0s))


#########################################################
# Fix the missing '0' in the files names for 08 and 09
dates[1:488] <- paste0('0',dates[1:497])

for(i in 1:length(dates)){
    #This is already named 'DeliveryDates' upon loading. It must remember the name of the xts object it was saved from
    load(paste0('Contracts', as.character(dates[i]) ,".rda")) 

  
  
  #Load all the contracts on a single day
  for(j in 1:(length(DeliveryDates)-1)){
    
    # This if statement takes care of the issue created by the fact that 08 and 09 are expressed without a 0 in front.
    if( nchar(DeliveryDates[j]) == 3 && substr(DeliveryDates[j], 3, 3) == 9){
      
      DeliveryDates <- DeliveryDates[-j]                                             # Deletes September in years 2008 and 2009
      
    } else if( nchar(DeliveryDates[j]) == 4 && substr(DeliveryDates[j], 4, 4) == 9){
      
      DeliveryDates <- DeliveryDates[-j]                                             # Deletes September
      
    } 
  }
  
  for(j in 1:(length(DeliveryDates)-1)){                                            # Since deleting Sept changes length(DeliveryDates) 

        #Quotes
        load(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[j]), ".rda"))
        temp <- temp["T09:30:00/T13:15:00"] # Focus on the daytime session 9:30am - 1:15pm
        assign(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[j])), temp)
        
#         #Transactions
#         load(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[j]), ".rda"))
#         try(temp <- temp["T09:29:00/T13:16:00"]) # Focus on the daytime session 9:30am - 1:15pm
#         try(assign(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[j])), temp))

  }
rm(temp)

  #Now begin calculations
  # Identify nearby and two years of the forward maturities including an if statement to 
  # 'roll' before maturity month. Second part of the && statement is only <15 because then the
  # nearby goes off the board and you can reference DeliveryDates[1] for the nearby again

  if( substr(dates[i], 3, 4) == substr(DeliveryDates[1], 3, 4) && as.numeric(substr(dates[i], 5, 6)) < 15){
    qnearby <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[2])))
    qplus1  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[3])))
    qplus2  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[4])))
    qplus3  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[5]))) 
    # qplus4  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[6])))
    # qplus5  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[7])))
    # qplus6  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[8])))
    # qplus7  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[9])))
    # qplus8  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[10])))
    # qplus9  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[11])))
    
    #   tnearby <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[2])))
    #   tplus1  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[3])))
    #   tplus2  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[4])))
    #   tplus3  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[5]))) 
    #   tplus4  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[6])))
    #   tplus5  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[7])))
    #   tplus6  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[8])))
    #   tplus7  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[9])))
    #   tplus8  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[10])))
    #   tplus9  <- get(paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[11])))
    
    rm(list = ls()[grep("^q_", ls())]) # Removes all varaibles that start with "q_"
    rm(list = ls()[grep("^t_", ls())]) # Removes all varaibles that start with "q_"
    }  else{ 
    qnearby <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[1])))
    qplus1  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[2])))
    qplus2  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[3])))
    qplus3  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[4]))) 
    # qplus4  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[5])))
    # qplus5  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[6])))
    # qplus6  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[7])))
    # qplus7  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[8])))
    # qplus8  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[9])))
    # qplus9  <- get(paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[10])))
    
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
   }
    
# Nearby and plus1
########################################################################################  

nearby_BID        <- to.period(qnearby$BID, period = 'seconds', k = 1, OHLC = FALSE)
nearby_BID_rets   <- diff.xts(nearby_BID, lag = 1, differences = 1, arithmetic = TRUE, log = TRUE, na.pad = TRUE)
non_zeros         <- index(nearby_BID_rets)[which(nearby_BID_rets != 0)]
nsecs_to_update_BID_rets   <- difftime(non_zeros[1:(length(non_zeros)-1)], non_zeros[2:length(non_zeros)], unit='secs')
# nearby_BID_rets_no0s<- subset(nearby_BID_rets, BID != 0)
# nearby_BID_rets_1sec<- lag.xts(nearby_BID_rets, k=-1)
# nearby_BID_rets_10sec<- lag.xts(nearby_BID_rets, k=-10)

# Learning to Use Pipes
nearby_BID_rets_no0s <- 
  qnearby$BID %>%
  to.period(period = 'seconds', k = 1, OHLC = FALSE) %>%
  diff.xts(lag = 1, differences = 1, arithmetic = TRUE, log = TRUE, na.pad = TRUE) %>%
  subset(BID != 0)

nearby_BID_rets_no0s_1sec <-  #Time Lag (lag.xts) lags 10 places, not ten seconds, so it need to lag then filter !=0 to get true 1 secs
  qnearby$BID %>%
  to.period(period = 'seconds', k = 1, OHLC = FALSE) %>%
  diff.xts(lag = 1, differences = 1, arithmetic = TRUE, log = TRUE, na.pad = TRUE) %>%
  lag.xts(k=-1) %>%
  subset(BID != 0)

nearby_BID_rets_no0s_10sec <-  #Time Lag (lag.xts) lags 10 places, not ten seconds, so it need to lag then filter !=0 to get true 10 secs
  qnearby$BID %>%
  to.period(period = 'seconds', k = 1, OHLC = FALSE) %>%
  diff.xts(lag = 1, differences = 1, arithmetic = TRUE, log = TRUE, na.pad = TRUE) %>%
  lag.xts(k=-10) %>%
  subset(BID != 0)   

plus1_BID         <- to.period(qplus1$BID, period = 'seconds', k = 1, OHLC = FALSE)
plus1_BID_rets    <- diff.xts(plus1_BID, lag = 1, differences = 1, arithmetic = TRUE, log = TRUE, na.pad = TRUE)
non_zeros         <- index(plus1_BID_rets)[which(plus1_BID_rets != 0)]
p1secs_to_update_BID_rets  <-difftime(non_zeros[1:(length(non_zeros)-1)], non_zeros[2:length(non_zeros)], unit='secs')
plus1_BID_rets_no0s<- subset(plus1_BID_rets, BID !=0)
plus1_BID_rets_1sec<- lag.xts(plus1_BID_rets, k=-1)
plus1_BID_rets_10sec<- lag.xts(plus1_BID_rets, k=-10)
plus1_BID_rets_no0s_1sec<- subset(plus1_BID_rets_1sec, BID !=0)
plus1_BID_rets_no0s_10sec<- subset(plus1_BID_rets_10sec, BID !=0)


nearby_OFR        <- to.period(qnearby$OFR, period = 'seconds', k = 1, OHLC = FALSE)
nearby_OFR_rets   <- diff.xts(nearby_OFR, lag = 1, differences = 1, arithmetic = TRUE, log = TRUE, na.pad = TRUE)
non_zeros         <- index(nearby_OFR_rets)[which(nearby_OFR_rets != 0)]
nsecs_to_update_OFR_rets   <-difftime(non_zeros[1:(length(non_zeros)-1)], non_zeros[2:length(non_zeros)], unit='secs')
# nearby_OFR_rets_no0s<- subset(nearby_OFR_rets, OFR != 0)
# nearby_OFR_rets_1sec<- lag.xts(nearby_OFR_rets, k=-1)
# nearby_OFR_rets_10sec<- lag.xts(nearby_OFR_rets, k=-10)

# Learning to Use Pipes
nearby_OFR_rets_no0s <- 
  qnearby$OFR %>%
  to.period(period = 'seconds', k = 1, OHLC = FALSE) %>%
  diff.xts(lag = 1, differences = 1, arithmetic = TRUE, log = TRUE, na.pad = TRUE) %>%
  subset(OFR != 0)

nearby_OFR_rets_no0s_1sec <-  #Time Lag (lag.xts) lags 10 places, not ten seconds, so it need to lag then filter !=0 to get true 1 secs
  qnearby$OFR %>%
  to.period(period = 'seconds', k = 1, OHLC = FALSE) %>%
  diff.xts(lag = 1, differences = 1, arithmetic = TRUE, log = TRUE, na.pad = TRUE) %>%
  lag.xts(k=-1) %>%
  subset(OFR != 0)

nearby_OFR_rets_no0s_10sec <-  #Time Lag (lag.xts) lags 10 places, not ten seconds, so it need to lag then filter !=0 to get true 10 secs
  qnearby$OFR %>%
  to.period(period = 'seconds', k = 1, OHLC = FALSE) %>%
  diff.xts(lag = 1, differences = 1, arithmetic = TRUE, log = TRUE, na.pad = TRUE) %>%
  lag.xts(k=-10) %>%
  subset(OFR != 0)   


plus1_OFR         <- to.period(qplus1$OFR, period = 'seconds', k = 1, OHLC = FALSE)
plus1_OFR_rets    <- diff.xts(plus1_OFR, lag = 1, differences = 1, arithmetic = TRUE, log = TRUE, na.pad = TRUE)
non_zeros         <- index(plus1_OFR_rets)[which(plus1_OFR_rets != 0)]
p1secs_to_update_OFR_rets  <-difftime(non_zeros[1:(length(non_zeros)-1)], non_zeros[2:length(non_zeros)], unit='secs')
plus1_OFR_rets_no0s<- subset(plus1_OFR_rets, OFR !=0)
plus1_OFR_rets_1sec<- lag.xts(plus1_OFR_rets, k=-1)
plus1_OFR_rets_10sec<- lag.xts(plus1_OFR_rets, k=-10)
plus1_OFR_rets_no0s_1sec<-  subset(plus1_OFR_rets_1sec, OFR !=0)
plus1_OFR_rets_no0s_10sec<- subset(plus1_OFR_rets_10sec, OFR !=0)

# Making contract pairs
# BIDS, Levels
near_plus1_BID        <- merge(nearby_BID, plus1_BID, all = TRUE, fill = NA, join = "outer", retside = TRUE, 
                               retclass = "xts")                
near_plus1_BID.df     <- as.data.frame(near_plus1_BID)


# BIDS, 'Returns'
#Keep zeroes
near_plus1_BID_rets   <- merge(nearby_BID_rets, plus1_BID_rets, all = TRUE, fill = NA, join = "outer", 
                               retside = TRUE, retclass = "xts")                
near_plus1_BID_rets.df<- as.data.frame(near_plus1_BID_rets)

# No zeros
near_plus1_BID_rets_no0s   <- merge(nearby_BID_rets_no0s, plus1_BID_rets_no0s, all = TRUE, fill = NA, join = "outer", 
                                    retside = TRUE, retclass = "xts")                
near_plus1_BID_rets_no0s.df<- as.data.frame(near_plus1_BID_rets_no0s)

near_plus1_BID_rets_1sec   <- merge(nearby_BID_rets_no0s_1sec, plus1_BID_rets, all = TRUE, fill = NA, join = "outer", 
                                    retside = TRUE, retclass = "xts")                
near_plus1_BID_rets_1sec.df<- as.data.frame(near_plus1_BID_rets_1sec)

near_plus1_BID_rets_10sec   <- merge(nearby_BID_rets_no0s_10sec, plus1_BID_rets, all = TRUE, fill = NA, join = "outer", 
                                     retside = TRUE, retclass = "xts")                
near_plus1_BID_rets_10sec.df<- as.data.frame(near_plus1_BID_rets_10sec)

# No Zeros - discovery in distant (lag the plus 1)
# Bids

plus1_near_BID_rets_1sec   <- merge(nearby_BID_rets, plus1_BID_rets_no0s_1sec, all = TRUE, fill = NA, join = "outer", 
                                    retside = TRUE, retclass = "xts")                
plus1_near_BID_rets_1sec.df<- as.data.frame(near_plus1_BID_rets_1sec)

plus1_near_BID_rets_10sec   <- merge(nearby_BID_rets, plus1_BID_rets_no0s_10sec, all = TRUE, fill = NA, join = "outer", 
                                    retside = TRUE, retclass = "xts")                
plus1_near_BID_rets_10sec.df<- as.data.frame(near_plus1_BID_rets_10sec)



# OFR, Levels
near_plus1_OFR        <- merge(nearby_OFR, plus1_OFR, all = TRUE, fill = NA, join = "outer", retside = TRUE, 
                               retclass = "xts")                
near_plus1_OFR.df     <- as.data.frame(near_plus1_OFR)


# 'Returns'
near_plus1_OFR_rets   <- merge(nearby_OFR_rets, plus1_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                               retside = TRUE, retclass = "xts")                
near_plus1_OFR_rets.df<- as.data.frame(near_plus1_OFR_rets)
near_plus1_OFR_rets_1sec   <- merge(nearby_OFR_rets_no0s_1sec, plus1_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                                    retside = TRUE, retclass = "xts")                
near_plus1_OFR_rets_1sec.df<- as.data.frame(near_plus1_OFR_rets_1sec)
near_plus1_OFR_rets_10sec   <- merge(nearby_OFR_rets_no0s_10sec, plus1_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                                     retside = TRUE, retclass = "xts")                
near_plus1_OFR_rets_10sec.df<- as.data.frame(near_plus1_OFR_rets_10sec)

# No zeros
near_plus1_OFR_rets_no0s   <- merge(nearby_OFR_rets_no0s, plus1_OFR_rets_no0s, all = TRUE, fill = NA, join = "outer", 
                                    retside = TRUE, retclass = "xts")                
near_plus1_OFR_rets_no0s.df<- as.data.frame(near_plus1_OFR_rets_no0s)

# No Zeros - discovery in distant (lag the plus 1)
# OFRs

plus1_near_OFR_rets_1sec   <- merge(nearby_OFR_rets, plus1_OFR_rets_no0s_1sec, all = TRUE, fill = NA, join = "outer", 
                                    retside = TRUE, retclass = "xts")                
plus1_near_OFR_rets_1sec.df<- as.data.frame(near_plus1_OFR_rets_1sec)

plus1_near_OFR_rets_10sec   <- merge(nearby_OFR_rets, plus1_OFR_rets_no0s_10sec, all = TRUE, fill = NA, join = "outer", 
                                     retside = TRUE, retclass = "xts")                
plus1_near_OFR_rets_10sec.df<- as.data.frame(near_plus1_OFR_rets_10sec)


# Bid-to-OFR
near_plus1_BIDOFR_rets   <- merge(nearby_BID_rets, plus1_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                                  retside = TRUE, retclass = "xts")                
near_plus1_BIDOFR_rets.df<- as.data.frame(near_plus1_BIDOFR_rets)
near_plus1_BIDOFR_rets_1sec   <- merge(nearby_BID_rets_no0s_1sec, plus1_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                                       retside = TRUE, retclass = "xts")                
near_plus1_BIDOFR_rets_1sec.df<- as.data.frame(near_plus1_BIDOFR_rets_1sec)
near_plus1_BIDOFR_rets_10sec   <- merge(nearby_BID_rets_no0s_10sec, plus1_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                                        retside = TRUE, retclass = "xts")                
near_plus1_BIDOFR_rets_10sec.df<- as.data.frame(near_plus1_BIDOFR_rets_10sec)

# No zeros
near_plus1_BIDOFR_rets_no0s   <- merge(nearby_BID_rets_no0s, plus1_OFR_rets_no0s, all = TRUE, fill = NA, join = "outer", 
                                       retside = TRUE, retclass = "xts")                
near_plus1_BIDOFR_rets_no0s.df<- as.data.frame(near_plus1_BIDOFR_rets_no0s)

# OFR-to-Bid
near_plus1_OFRBID_rets   <- merge(nearby_BID_rets, plus1_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                                  retside = TRUE, retclass = "xts")                
near_plus1_OFRBID_rets.df<- as.data.frame(near_plus1_OFRBID_rets)
near_plus1_OFRBID_rets_1sec   <- merge(nearby_BID_rets_no0s_1sec, plus1_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                                       retside = TRUE, retclass = "xts")                
near_plus1_OFRBID_rets_1sec.df<- as.data.frame(near_plus1_OFRBID_rets_1sec)
near_plus1_OFRBID_rets_10sec   <- merge(nearby_BID_rets_no0s_10sec, plus1_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                                        retside = TRUE, retclass = "xts")                
near_plus1_OFRBID_rets_10sec.df<- as.data.frame(near_plus1_OFRBID_rets_10sec)

# No zeros
near_plus1_OFRBID_rets_no0s   <- merge(nearby_BID_rets_no0s, plus1_OFR_rets_no0s, all = TRUE, fill = NA, join = "outer", 
                                       retside = TRUE, retclass = "xts")                
near_plus1_OFRBID_rets_no0s.df<- as.data.frame(near_plus1_OFRBID_rets_no0s)



# In time bins - ten minutes
# Date in the CUMULCORREL1_BID timestamp is irrelevant. We are creating 10 minute bins, in which we place 
# correlations from each day in columns. Last time stamp says 13:19:50, but really it is 13:15:50
# but in the timeSequence function, partial bins are not allowed and 13:09:59 to 13:15:59 is not a full 
# ten minutes.

# BIDS, 'Returns'
ep <- endpoints(align.time(near_plus1_BID_rets, 10*60), 'minutes', k=10)
correl1_rets<- period.apply(align.time(near_plus1_BID_rets, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl1_rets))), 2, substr, 12, 19)
correl1_rets.df <- as.data.frame(correl1_rets)
row.names(correl1_rets.df) <- temp 
colnames(correl1_rets.df) <- c("V1", "V2", "V3", "V4")
correl1_rets.df$TimeBins <- factor(row.names(correl1_rets.df))
correl1_rets.df <- correl1_rets.df[,c("V2", "TimeBins")]
CUMULCORREL1_BID_rets <- merge(CUMULCORREL1_BID_rets, correl1_rets.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus1_BID_rets_1sec, 10*60), 'minutes', k=10)
correl1_rets_1sec<- period.apply(align.time(near_plus1_BID_rets_1sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl1_rets_1sec))), 2, substr, 12, 19)
correl1_rets_1sec.df <- as.data.frame(correl1_rets_1sec)
row.names(correl1_rets_1sec.df) <- temp 
colnames(correl1_rets_1sec.df) <- c("V1", "V2", "V3", "V4")
correl1_rets_1sec.df$TimeBins <- factor(row.names(correl1_rets_1sec.df))
correl1_rets_1sec.df <- correl1_rets_1sec.df[,c("V2", "TimeBins")]
CUMULCORREL1_BID_rets_1sec <- merge(CUMULCORREL1_BID_rets_1sec, correl1_rets_1sec.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus1_BID_rets_10sec, 10*60), 'minutes', k=10)
correl1_rets_10sec<- period.apply(align.time(near_plus1_BID_rets_10sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl1_rets_10sec))), 2, substr, 12, 19)
correl1_rets_10sec.df <- as.data.frame(correl1_rets_10sec)
row.names(correl1_rets_10sec.df) <- temp 
colnames(correl1_rets_10sec.df) <- c("V1", "V2", "V3", "V4")
correl1_rets_10sec.df$TimeBins <- factor(row.names(correl1_rets_10sec.df))
correl1_rets_10sec.df <- correl1_rets_10sec.df[,c("V2", "TimeBins")]
CUMULCORREL1_BID_rets_10sec <- merge(CUMULCORREL1_BID_rets_10sec, correl1_rets_10sec.df, by= "TimeBins", all= TRUE)

##
ep <- endpoints(align.time(plus1_near_BID_rets_1sec, 10*60), 'minutes', k=10)
correl1_rets_1sec<- period.apply(align.time(plus1_near_BID_rets_1sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl1_rets_1sec))), 2, substr, 12, 19)
correl1_rets_1sec.df <- as.data.frame(correl1_rets_1sec)
row.names(correl1_rets_1sec.df) <- temp 
colnames(correl1_rets_1sec.df) <- c("V1", "V2", "V3", "V4")
correl1_rets_1sec.df$TimeBins <- factor(row.names(correl1_rets_1sec.df))
correl1_rets_1sec.df <- correl1_rets_1sec.df[,c("V2", "TimeBins")]
CUMULCORREL1N_BID_rets_1sec <- merge(CUMULCORREL1N_BID_rets_1sec, correl1_rets_1sec.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(plus1_near_BID_rets_10sec, 10*60), 'minutes', k=10)
correl1_rets_10sec<- period.apply(align.time(plus1_near_BID_rets_10sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl1_rets_10sec))), 2, substr, 12, 19)
correl1_rets_10sec.df <- as.data.frame(correl1_rets_10sec)
row.names(correl1_rets_10sec.df) <- temp 
colnames(correl1_rets_10sec.df) <- c("V1", "V2", "V3", "V4")
correl1_rets_10sec.df$TimeBins <- factor(row.names(correl1_rets_10sec.df))
correl1_rets_10sec.df <- correl1_rets_10sec.df[,c("V2", "TimeBins")]
CUMULCORREL1N_BID_rets_10sec <- merge(CUMULCORREL1N_BID_rets_10sec, correl1_rets_10sec.df, by= "TimeBins", all= TRUE)

##

# BIDS, 'Returns' - no zeros
ep <- endpoints(align.time(near_plus1_BID_rets_no0s, 10*60), 'minutes', k=10)
correl1_rets_no0s<- period.apply(align.time(near_plus1_BID_rets_no0s, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl1_rets_no0s))), 2, substr, 12, 19)
correl1_rets_no0s.df <- as.data.frame(correl1_rets_no0s)
row.names(correl1_rets_no0s.df) <- temp 
colnames(correl1_rets_no0s.df) <- c("V1", "V2", "V3", "V4")
correl1_rets_no0s.df$TimeBins <- factor(row.names(correl1_rets_no0s.df))
correl1_rets_no0s.df <- correl1_rets_no0s.df[,c("V2", "TimeBins")]
CUMULCORREL1_BID_rets_no0s <- merge(CUMULCORREL1_BID_rets_no0s, correl1_rets_no0s.df, by= "TimeBins", all= TRUE)

# OFRS, 'Returns'
ep <- endpoints(align.time(near_plus1_OFR_rets, 10*60), 'minutes', k=10)
correl1_rets<- period.apply(align.time(near_plus1_OFR_rets, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl1_rets))), 2, substr, 12, 19)
correl1_rets.df <- as.data.frame(correl1_rets)
row.names(correl1_rets.df) <- temp 
colnames(correl1_rets.df) <- c("V1", "V2", "V3", "V4")
correl1_rets.df$TimeBins <- factor(row.names(correl1_rets.df))
correl1_rets.df <- correl1_rets.df[,c("V2", "TimeBins")]
CUMULCORREL1_OFR_rets <- merge(CUMULCORREL1_OFR_rets, correl1_rets.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus1_OFR_rets_1sec, 10*60), 'minutes', k=10)
correl1_rets_1sec<- period.apply(align.time(near_plus1_OFR_rets_1sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl1_rets_1sec))), 2, substr, 12, 19)
correl1_rets_1sec.df <- as.data.frame(correl1_rets_1sec)
row.names(correl1_rets_1sec.df) <- temp 
colnames(correl1_rets_1sec.df) <- c("V1", "V2", "V3", "V4")
correl1_rets_1sec.df$TimeBins <- factor(row.names(correl1_rets_1sec.df))
correl1_rets_1sec.df <- correl1_rets_1sec.df[,c("V2", "TimeBins")]
CUMULCORREL1_OFR_rets_1sec <- merge(CUMULCORREL1_OFR_rets_1sec, correl1_rets_1sec.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus1_OFR_rets_10sec, 10*60), 'minutes', k=10)
correl1_rets_10sec<- period.apply(align.time(near_plus1_OFR_rets_10sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl1_rets_10sec))), 2, substr, 12, 19)
correl1_rets_10sec.df <- as.data.frame(correl1_rets_10sec)
row.names(correl1_rets_10sec.df) <- temp 
colnames(correl1_rets_10sec.df) <- c("V1", "V2", "V3", "V4")
correl1_rets_10sec.df$TimeBins <- factor(row.names(correl1_rets_10sec.df))
correl1_rets_10sec.df <- correl1_rets_10sec.df[,c("V2", "TimeBins")]
CUMULCORREL1_OFR_rets_10sec <- merge(CUMULCORREL1_OFR_rets_10sec, correl1_rets_10sec.df, by= "TimeBins", all= TRUE)

##
ep <- endpoints(align.time(plus1_near_OFR_rets_1sec, 10*60), 'minutes', k=10)
correl1_rets_1sec<- period.apply(align.time(plus1_near_OFR_rets_1sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl1_rets_1sec))), 2, substr, 12, 19)
correl1_rets_1sec.df <- as.data.frame(correl1_rets_1sec)
row.names(correl1_rets_1sec.df) <- temp 
colnames(correl1_rets_1sec.df) <- c("V1", "V2", "V3", "V4")
correl1_rets_1sec.df$TimeBins <- factor(row.names(correl1_rets_1sec.df))
correl1_rets_1sec.df <- correl1_rets_1sec.df[,c("V2", "TimeBins")]
CUMULCORREL1N_OFR_rets_1sec <- merge(CUMULCORREL1N_OFR_rets_1sec, correl1_rets_1sec.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(plus1_near_OFR_rets_10sec, 10*60), 'minutes', k=10)
correl1_rets_10sec<- period.apply(align.time(plus1_near_OFR_rets_10sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl1_rets_10sec))), 2, substr, 12, 19)
correl1_rets_10sec.df <- as.data.frame(correl1_rets_10sec)
row.names(correl1_rets_10sec.df) <- temp 
colnames(correl1_rets_10sec.df) <- c("V1", "V2", "V3", "V4")
correl1_rets_10sec.df$TimeBins <- factor(row.names(correl1_rets_10sec.df))
correl1_rets_10sec.df <- correl1_rets_10sec.df[,c("V2", "TimeBins")]
CUMULCORREL1N_OFR_rets_10sec <- merge(CUMULCORREL1N_OFR_rets_10sec, correl1_rets_10sec.df, by= "TimeBins", all= TRUE)

##

# OFRS, 'Returns' - no zeros
ep <- endpoints(align.time(near_plus1_OFR_rets_no0s, 10*60), 'minutes', k=10)
correl1_rets_no0s<- period.apply(align.time(near_plus1_OFR_rets_no0s, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl1_rets_no0s))), 2, substr, 12, 19)
correl1_rets_no0s.df <- as.data.frame(correl1_rets_no0s)
row.names(correl1_rets_no0s.df) <- temp 
colnames(correl1_rets_no0s.df) <- c("V1", "V2", "V3", "V4")
correl1_rets_no0s.df$TimeBins <- factor(row.names(correl1_rets_no0s.df))
correl1_rets_no0s.df <- correl1_rets_no0s.df[,c("V2", "TimeBins")]
CUMULCORREL1_OFR_rets_no0s <- merge(CUMULCORREL1_OFR_rets_no0s, correl1_rets_no0s.df, by= "TimeBins", all= TRUE)



# BIDOFRS, 'Returns'
ep <- endpoints(align.time(near_plus1_BIDOFR_rets, 10*60), 'minutes', k=10)
correl1_rets<- period.apply(align.time(near_plus1_BIDOFR_rets, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl1_rets))), 2, substr, 12, 19)
correl1_rets.df <- as.data.frame(correl1_rets)
row.names(correl1_rets.df) <- temp 
colnames(correl1_rets.df) <- c("V1", "V2", "V3", "V4")
correl1_rets.df$TimeBins <- factor(row.names(correl1_rets.df))
correl1_rets.df <- correl1_rets.df[,c("V2", "TimeBins")]
CUMULCORREL1_BIDOFR_rets <- merge(CUMULCORREL1_BIDOFR_rets, correl1_rets.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus1_BIDOFR_rets_1sec, 10*60), 'minutes', k=10)
correl1_rets_1sec<- period.apply(align.time(near_plus1_BIDOFR_rets_1sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl1_rets_1sec))), 2, substr, 12, 19)
correl1_rets_1sec.df <- as.data.frame(correl1_rets_1sec)
row.names(correl1_rets_1sec.df) <- temp 
colnames(correl1_rets_1sec.df) <- c("V1", "V2", "V3", "V4")
correl1_rets_1sec.df$TimeBins <- factor(row.names(correl1_rets_1sec.df))
correl1_rets_1sec.df <- correl1_rets_1sec.df[,c("V2", "TimeBins")]
CUMULCORREL1_BIDOFR_rets_1sec <- merge(CUMULCORREL1_BIDOFR_rets_1sec, correl1_rets_1sec.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus1_BIDOFR_rets_10sec, 10*60), 'minutes', k=10)
correl1_rets_10sec<- period.apply(align.time(near_plus1_BIDOFR_rets_10sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl1_rets_10sec))), 2, substr, 12, 19)
correl1_rets_10sec.df <- as.data.frame(correl1_rets_10sec)
row.names(correl1_rets_10sec.df) <- temp 
colnames(correl1_rets_10sec.df) <- c("V1", "V2", "V3", "V4")
correl1_rets_10sec.df$TimeBins <- factor(row.names(correl1_rets_10sec.df))
correl1_rets_10sec.df <- correl1_rets_10sec.df[,c("V2", "TimeBins")]
CUMULCORREL1_BIDOFR_rets_10sec <- merge(CUMULCORREL1_BIDOFR_rets_10sec, correl1_rets_10sec.df, by= "TimeBins", all= TRUE)

# BIDOFRS, 'Returns' - no zeros
ep <- endpoints(align.time(near_plus1_BIDOFR_rets_no0s, 10*60), 'minutes', k=10)
correl1_rets_no0s<- period.apply(align.time(near_plus1_BIDOFR_rets_no0s, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl1_rets_no0s))), 2, substr, 12, 19)
correl1_rets_no0s.df <- as.data.frame(correl1_rets_no0s)
row.names(correl1_rets_no0s.df) <- temp 
colnames(correl1_rets_no0s.df) <- c("V1", "V2", "V3", "V4")
correl1_rets_no0s.df$TimeBins <- factor(row.names(correl1_rets_no0s.df))
correl1_rets_no0s.df <- correl1_rets_no0s.df[,c("V2", "TimeBins")]
CUMULCORREL1_BIDOFR_rets_no0s <- merge(CUMULCORREL1_BIDOFR_rets_no0s, correl1_rets_no0s.df, by= "TimeBins", all= TRUE)    
# OFRBIDS, 'Returns'
ep <- endpoints(align.time(near_plus1_OFRBID_rets, 10*60), 'minutes', k=10)
correl1_rets<- period.apply(align.time(near_plus1_OFRBID_rets, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl1_rets))), 2, substr, 12, 19)
correl1_rets.df <- as.data.frame(correl1_rets)
row.names(correl1_rets.df) <- temp 
colnames(correl1_rets.df) <- c("V1", "V2", "V3", "V4")
correl1_rets.df$TimeBins <- factor(row.names(correl1_rets.df))
correl1_rets.df <- correl1_rets.df[,c("V2", "TimeBins")]
CUMULCORREL1_OFRBID_rets <- merge(CUMULCORREL1_OFRBID_rets, correl1_rets.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus1_OFRBID_rets_1sec, 10*60), 'minutes', k=10)
correl1_rets_1sec<- period.apply(align.time(near_plus1_OFRBID_rets_1sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl1_rets_1sec))), 2, substr, 12, 19)
correl1_rets_1sec.df <- as.data.frame(correl1_rets_1sec)
row.names(correl1_rets_1sec.df) <- temp 
colnames(correl1_rets_1sec.df) <- c("V1", "V2", "V3", "V4")
correl1_rets_1sec.df$TimeBins <- factor(row.names(correl1_rets_1sec.df))
correl1_rets_1sec.df <- correl1_rets_1sec.df[,c("V2", "TimeBins")]
CUMULCORREL1_OFRBID_rets_1sec <- merge(CUMULCORREL1_OFRBID_rets_1sec, correl1_rets_1sec.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus1_OFRBID_rets_10sec, 10*60), 'minutes', k=10)
correl1_rets_10sec<- period.apply(align.time(near_plus1_OFRBID_rets_10sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl1_rets_10sec))), 2, substr, 12, 19)
correl1_rets_10sec.df <- as.data.frame(correl1_rets_10sec)
row.names(correl1_rets_10sec.df) <- temp 
colnames(correl1_rets_10sec.df) <- c("V1", "V2", "V3", "V4")
correl1_rets_10sec.df$TimeBins <- factor(row.names(correl1_rets_10sec.df))
correl1_rets_10sec.df <- correl1_rets_10sec.df[,c("V2", "TimeBins")]
CUMULCORREL1_OFRBID_rets_10sec <- merge(CUMULCORREL1_OFRBID_rets_10sec, correl1_rets_10sec.df, by= "TimeBins", all= TRUE)

# OFRBIDS, 'Returns' - no zeros
ep <- endpoints(align.time(near_plus1_OFRBID_rets_no0s, 10*60), 'minutes', k=10)
correl1_rets_no0s<- period.apply(align.time(near_plus1_OFRBID_rets_no0s, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl1_rets_no0s))), 2, substr, 12, 19)
correl1_rets_no0s.df <- as.data.frame(correl1_rets_no0s)
row.names(correl1_rets_no0s.df) <- temp 
colnames(correl1_rets_no0s.df) <- c("V1", "V2", "V3", "V4")
correl1_rets_no0s.df$TimeBins <- factor(row.names(correl1_rets_no0s.df))
correl1_rets_no0s.df <- correl1_rets_no0s.df[,c("V2", "TimeBins")]
CUMULCORREL1_OFRBID_rets_no0s <- merge(CUMULCORREL1_OFRBID_rets_no0s, correl1_rets_no0s.df, by= "TimeBins", all= TRUE)


######################################################################################## 

# #Nearby and plus2
# ########################################################################################  

plus2_BID         <- to.period(qplus2$BID, period = 'seconds', k = 1, OHLC = FALSE)
plus2_BID_rets    <- diff.xts(plus2_BID, lag = 1, differences = 1, arithmetic = TRUE, log = TRUE, na.pad = TRUE)
non_zeros         <- index(plus2_BID_rets)[which(plus2_BID_rets != 0)]
p2secs_to_update_BID_rets  <-difftime(non_zeros[1:(length(non_zeros)-1)], non_zeros[2:length(non_zeros)], unit='secs')
plus2_BID_rets_no0s<- subset(plus2_BID_rets, BID !=0)
plus2_BID_rets_1sec<- lag.xts(plus2_BID_rets, k=-1)
plus2_BID_rets_10sec<- lag.xts(plus2_BID_rets, k=-10)

plus2_OFR         <- to.period(qplus2$OFR, period = 'seconds', k = 1, OHLC = FALSE)
plus2_OFR_rets    <- diff.xts(plus2_OFR, lag = 1, differences = 1, arithmetic = TRUE, log = TRUE, na.pad = TRUE)
non_zeros         <- index(plus2_OFR_rets)[which(plus2_OFR_rets != 0)]
p2secs_to_update_OFR_rets  <-difftime(non_zeros[1:(length(non_zeros)-1)], non_zeros[2:length(non_zeros)], unit='secs')
plus2_OFR_rets_no0s<- subset(plus2_OFR_rets, OFR !=0)
plus2_OFR_rets_1sec<- lag.xts(plus2_OFR_rets, k=-1)
plus2_OFR_rets_10sec<- lag.xts(plus2_OFR_rets, k=-10)

# Making contract pairs
# BIDS, Levels
near_plus2_BID        <- merge(nearby_BID, plus2_BID, all = TRUE, fill = NA, join = "outer", retside = TRUE, 
                               retclass = "xts")                
near_plus2_BID.df     <- as.data.frame(near_plus2_BID)

# BIDS, 'Returns'
#Keep zeroes
near_plus2_BID_rets   <- merge(nearby_BID_rets, plus2_BID_rets, all = TRUE, fill = NA, join = "outer", 
                               retside = TRUE, retclass = "xts")                
near_plus2_BID_rets.df<- as.data.frame(near_plus2_BID_rets)
near_plus2_BID_rets_1sec   <- merge(nearby_BID_rets, plus2_BID_rets_1sec, all = TRUE, fill = NA, join = "outer", 
                                    retside = TRUE, retclass = "xts")                
near_plus2_BID_rets_1sec.df<- as.data.frame(near_plus2_BID_rets_1sec)
near_plus2_BID_rets_10sec   <- merge(nearby_BID_rets, plus2_BID_rets_10sec, all = TRUE, fill = NA, join = "outer", 
                                     retside = TRUE, retclass = "xts")                
near_plus2_BID_rets_10sec.df<- as.data.frame(near_plus2_BID_rets_10sec)

# No zeros
near_plus2_BID_rets_no0s   <- merge(nearby_BID_rets_no0s, plus2_BID_rets_no0s, all = TRUE, fill = NA, join = "outer", 
                                    retside = TRUE, retclass = "xts")                
near_plus2_BID_rets_no0s.df<- as.data.frame(near_plus2_BID_rets_no0s)

# OFR, Levels
near_plus2_OFR        <- merge(nearby_OFR, plus2_OFR, all = TRUE, fill = NA, join = "outer", retside = TRUE, 
                               retclass = "xts")                
near_plus2_OFR.df     <- as.data.frame(near_plus2_OFR)

# 'Returns'
near_plus2_OFR_rets   <- merge(nearby_OFR_rets, plus2_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                               retside = TRUE, retclass = "xts")                
near_plus2_OFR_rets.df<- as.data.frame(near_plus2_OFR_rets)
near_plus2_OFR_rets_1sec   <- merge(nearby_OFR_rets, plus2_OFR_rets_1sec, all = TRUE, fill = NA, join = "outer", 
                                    retside = TRUE, retclass = "xts")                
near_plus2_OFR_rets_1sec.df<- as.data.frame(near_plus2_OFR_rets_1sec)
near_plus2_OFR_rets_10sec   <- merge(nearby_OFR_rets, plus2_OFR_rets_10sec, all = TRUE, fill = NA, join = "outer", 
                                     retside = TRUE, retclass = "xts")                
near_plus2_OFR_rets_10sec.df<- as.data.frame(near_plus2_OFR_rets_10sec)

# No zeros
near_plus2_OFR_rets_no0s   <- merge(nearby_OFR_rets_no0s, plus2_OFR_rets_no0s, all = TRUE, fill = NA, join = "outer", 
                                    retside = TRUE, retclass = "xts")                
near_plus2_OFR_rets_no0s.df<- as.data.frame(near_plus2_OFR_rets_no0s)

# Bid-to-OFR
near_plus2_BIDOFR_rets   <- merge(nearby_BID_rets, plus2_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                                  retside = TRUE, retclass = "xts")                
near_plus2_BIDOFR_rets.df<- as.data.frame(near_plus2_BIDOFR_rets)
near_plus2_BIDOFR_rets_1sec   <- merge(nearby_BID_rets_no0s_1sec, plus2_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                                       retside = TRUE, retclass = "xts")                
near_plus2_BIDOFR_rets_1sec.df<- as.data.frame(near_plus2_BIDOFR_rets_1sec)
near_plus2_BIDOFR_rets_10sec   <- merge(nearby_BID_rets_no0s_10sec, plus2_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                                        retside = TRUE, retclass = "xts")                
near_plus2_BIDOFR_rets_10sec.df<- as.data.frame(near_plus2_BIDOFR_rets_10sec)

# No zeros
near_plus2_BIDOFR_rets_no0s   <- merge(nearby_BID_rets_no0s, plus2_OFR_rets_no0s, all = TRUE, fill = NA, join = "outer", 
                                       retside = TRUE, retclass = "xts")                
near_plus2_BIDOFR_rets_no0s.df<- as.data.frame(near_plus2_BIDOFR_rets_no0s)

# OFR-to-Bid
near_plus2_OFRBID_rets   <- merge(nearby_BID_rets, plus2_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                                  retside = TRUE, retclass = "xts")                
near_plus2_OFRBID_rets.df<- as.data.frame(near_plus2_OFRBID_rets)
near_plus2_OFRBID_rets_1sec   <- merge(nearby_BID_rets_no0s_1sec, plus2_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                                       retside = TRUE, retclass = "xts")                
near_plus2_OFRBID_rets_1sec.df<- as.data.frame(near_plus2_OFRBID_rets_1sec)
near_plus2_OFRBID_rets_10sec   <- merge(nearby_BID_rets_no0s_10sec, plus2_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                                        retside = TRUE, retclass = "xts")                
near_plus2_OFRBID_rets_10sec.df<- as.data.frame(near_plus2_OFRBID_rets_10sec)

# No zeros
near_plus2_OFRBID_rets_no0s   <- merge(nearby_BID_rets_no0s, plus2_OFR_rets_no0s, all = TRUE, fill = NA, join = "outer", 
                                       retside = TRUE, retclass = "xts")                
near_plus2_OFRBID_rets_no0s.df<- as.data.frame(near_plus2_OFRBID_rets_no0s)

# In time bins - ten minutes
# Date in the CUMULCORREL1_BID timestamp is irrelevant. We are creating 10 minute bins, in which we place 
# correlations from each day in columns. Last time stamp says 13:19:50, but really it is 13:15:50
# but in the timeSequence function, partial bins are not allowed and 13:09:59 to 13:15:59 is not a full 
# ten minutes.

# BIDS, 'Returns'
ep <- endpoints(align.time(near_plus2_BID_rets, 10*60), 'minutes', k=10)
correl2_rets<- period.apply(align.time(near_plus2_BID_rets, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl2_rets))), 2, substr, 12, 19)
correl2_rets.df <- as.data.frame(correl2_rets)
row.names(correl2_rets.df) <- temp 
colnames(correl2_rets.df) <- c("V1", "V2", "V3", "V4")
correl2_rets.df$TimeBins <- factor(row.names(correl2_rets.df))
correl2_rets.df <- correl2_rets.df[,c("V2", "TimeBins")]
CUMULCORREL2_BID_rets <- merge(CUMULCORREL2_BID_rets, correl2_rets.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus2_BID_rets_1sec, 10*60), 'minutes', k=10)
correl2_rets_1sec<- period.apply(align.time(near_plus2_BID_rets_1sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl2_rets_1sec))), 2, substr, 12, 19)
correl2_rets_1sec.df <- as.data.frame(correl2_rets_1sec)
row.names(correl2_rets_1sec.df) <- temp 
colnames(correl2_rets_1sec.df) <- c("V1", "V2", "V3", "V4")
correl2_rets_1sec.df$TimeBins <- factor(row.names(correl2_rets_1sec.df))
correl2_rets_1sec.df <- correl2_rets_1sec.df[,c("V2", "TimeBins")]
CUMULCORREL2_BID_rets_1sec <- merge(CUMULCORREL2_BID_rets_1sec, correl2_rets_1sec.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus2_BID_rets_10sec, 10*60), 'minutes', k=10)
correl2_rets_10sec<- period.apply(align.time(near_plus2_BID_rets_10sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl2_rets_10sec))), 2, substr, 12, 19)
correl2_rets_10sec.df <- as.data.frame(correl2_rets_10sec)
row.names(correl2_rets_10sec.df) <- temp 
colnames(correl2_rets_10sec.df) <- c("V1", "V2", "V3", "V4")
correl2_rets_10sec.df$TimeBins <- factor(row.names(correl2_rets_10sec.df))
correl2_rets_10sec.df <- correl2_rets_10sec.df[,c("V2", "TimeBins")]
CUMULCORREL2_BID_rets_10sec <- merge(CUMULCORREL2_BID_rets_10sec, correl2_rets_10sec.df, by= "TimeBins", all= TRUE)

# BIDS, 'Returns' - no zeros
ep <- endpoints(align.time(near_plus2_BID_rets_no0s, 10*60), 'minutes', k=10)
correl2_rets_no0s<- period.apply(align.time(near_plus2_BID_rets_no0s, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl2_rets_no0s))), 2, substr, 12, 19)
correl2_rets_no0s.df <- as.data.frame(correl2_rets_no0s)
row.names(correl2_rets_no0s.df) <- temp 
colnames(correl2_rets_no0s.df) <- c("V1", "V2", "V3", "V4")
correl2_rets_no0s.df$TimeBins <- factor(row.names(correl2_rets_no0s.df))
correl2_rets_no0s.df <- correl2_rets_no0s.df[,c("V2", "TimeBins")]
CUMULCORREL2_BID_rets_no0s <- merge(CUMULCORREL2_BID_rets_no0s, correl2_rets_no0s.df, by= "TimeBins", all= TRUE)

# OFRS, 'Returns'
ep <- endpoints(align.time(near_plus2_OFR_rets, 10*60), 'minutes', k=10)
correl2_rets<- period.apply(align.time(near_plus2_OFR_rets, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl2_rets))), 2, substr, 12, 19)
correl2_rets.df <- as.data.frame(correl2_rets)
row.names(correl2_rets.df) <- temp 
colnames(correl2_rets.df) <- c("V1", "V2", "V3", "V4")
correl2_rets.df$TimeBins <- factor(row.names(correl2_rets.df))
correl2_rets.df <- correl2_rets.df[,c("V2", "TimeBins")]
CUMULCORREL2_OFR_rets <- merge(CUMULCORREL2_OFR_rets, correl2_rets.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus2_OFR_rets_1sec, 10*60), 'minutes', k=10)
correl2_rets_1sec<- period.apply(align.time(near_plus2_OFR_rets_1sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl2_rets_1sec))), 2, substr, 12, 19)
correl2_rets_1sec.df <- as.data.frame(correl2_rets_1sec)
row.names(correl2_rets_1sec.df) <- temp 
colnames(correl2_rets_1sec.df) <- c("V1", "V2", "V3", "V4")
correl2_rets_1sec.df$TimeBins <- factor(row.names(correl2_rets_1sec.df))
correl2_rets_1sec.df <- correl2_rets_1sec.df[,c("V2", "TimeBins")]
CUMULCORREL2_OFR_rets_1sec <- merge(CUMULCORREL2_OFR_rets_1sec, correl2_rets_1sec.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus2_OFR_rets_10sec, 10*60), 'minutes', k=10)
correl2_rets_10sec<- period.apply(align.time(near_plus2_OFR_rets_10sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl2_rets_10sec))), 2, substr, 12, 19)
correl2_rets_10sec.df <- as.data.frame(correl2_rets_10sec)
row.names(correl2_rets_10sec.df) <- temp 
colnames(correl2_rets_10sec.df) <- c("V1", "V2", "V3", "V4")
correl2_rets_10sec.df$TimeBins <- factor(row.names(correl2_rets_10sec.df))
correl2_rets_10sec.df <- correl2_rets_10sec.df[,c("V2", "TimeBins")]
CUMULCORREL2_OFR_rets_10sec <- merge(CUMULCORREL2_OFR_rets_10sec, correl2_rets_10sec.df, by= "TimeBins", all= TRUE)

# OFRS, 'Returns' - no zeros
ep <- endpoints(align.time(near_plus2_OFR_rets_no0s, 10*60), 'minutes', k=10)
correl2_rets_no0s<- period.apply(align.time(near_plus2_OFR_rets_no0s, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl2_rets_no0s))), 2, substr, 12, 19)
correl2_rets_no0s.df <- as.data.frame(correl2_rets_no0s)
row.names(correl2_rets_no0s.df) <- temp 
colnames(correl2_rets_no0s.df) <- c("V1", "V2", "V3", "V4")
correl2_rets_no0s.df$TimeBins <- factor(row.names(correl2_rets_no0s.df))
correl2_rets_no0s.df <- correl2_rets_no0s.df[,c("V2", "TimeBins")]
CUMULCORREL2_OFR_rets_no0s <- merge(CUMULCORREL2_OFR_rets_no0s, correl2_rets_no0s.df, by= "TimeBins", all= TRUE)

# BIDOFRS, 'Returns'
ep <- endpoints(align.time(near_plus2_BIDOFR_rets, 10*60), 'minutes', k=10)
correl2_rets<- period.apply(align.time(near_plus2_BIDOFR_rets, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl2_rets))), 2, substr, 12, 19)
correl2_rets.df <- as.data.frame(correl2_rets)
row.names(correl2_rets.df) <- temp 
colnames(correl2_rets.df) <- c("V1", "V2", "V3", "V4")
correl2_rets.df$TimeBins <- factor(row.names(correl2_rets.df))
correl2_rets.df <- correl2_rets.df[,c("V2", "TimeBins")]
CUMULCORREL2_BIDOFR_rets <- merge(CUMULCORREL2_BIDOFR_rets, correl2_rets.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus2_BIDOFR_rets_1sec, 10*60), 'minutes', k=10)
correl2_rets_1sec<- period.apply(align.time(near_plus2_BIDOFR_rets_1sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl2_rets_1sec))), 2, substr, 12, 19)
correl2_rets_1sec.df <- as.data.frame(correl2_rets_1sec)
row.names(correl2_rets_1sec.df) <- temp 
colnames(correl2_rets_1sec.df) <- c("V1", "V2", "V3", "V4")
correl2_rets_1sec.df$TimeBins <- factor(row.names(correl2_rets_1sec.df))
correl2_rets_1sec.df <- correl2_rets_1sec.df[,c("V2", "TimeBins")]
CUMULCORREL2_BIDOFR_rets_1sec <- merge(CUMULCORREL2_BIDOFR_rets_1sec, correl2_rets_1sec.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus2_BIDOFR_rets_10sec, 10*60), 'minutes', k=10)
correl2_rets_10sec<- period.apply(align.time(near_plus2_BIDOFR_rets_10sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl2_rets_10sec))), 2, substr, 12, 19)
correl2_rets_10sec.df <- as.data.frame(correl2_rets_10sec)
row.names(correl2_rets_10sec.df) <- temp 
colnames(correl2_rets_10sec.df) <- c("V1", "V2", "V3", "V4")
correl2_rets_10sec.df$TimeBins <- factor(row.names(correl2_rets_10sec.df))
correl2_rets_10sec.df <- correl2_rets_10sec.df[,c("V2", "TimeBins")]
CUMULCORREL2_BIDOFR_rets_10sec <- merge(CUMULCORREL2_BIDOFR_rets_10sec, correl2_rets_10sec.df, by= "TimeBins", all= TRUE)

# BIDOFRS, 'Returns' - no zeros
ep <- endpoints(align.time(near_plus2_BIDOFR_rets_no0s, 10*60), 'minutes', k=10)
correl2_rets_no0s<- period.apply(align.time(near_plus2_BIDOFR_rets_no0s, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl2_rets_no0s))), 2, substr, 12, 19)
correl2_rets_no0s.df <- as.data.frame(correl2_rets_no0s)
row.names(correl2_rets_no0s.df) <- temp 
colnames(correl2_rets_no0s.df) <- c("V1", "V2", "V3", "V4")
correl2_rets_no0s.df$TimeBins <- factor(row.names(correl2_rets_no0s.df))
correl2_rets_no0s.df <- correl2_rets_no0s.df[,c("V2", "TimeBins")]
CUMULCORREL2_BIDOFR_rets_no0s <- merge(CUMULCORREL2_BIDOFR_rets_no0s, correl2_rets_no0s.df, by= "TimeBins", all= TRUE)    
# OFRBIDS, 'Returns'
ep <- endpoints(align.time(near_plus2_OFRBID_rets, 10*60), 'minutes', k=10)
correl2_rets<- period.apply(align.time(near_plus2_OFRBID_rets, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl2_rets))), 2, substr, 12, 19)
correl2_rets.df <- as.data.frame(correl2_rets)
row.names(correl2_rets.df) <- temp 
colnames(correl2_rets.df) <- c("V1", "V2", "V3", "V4")
correl2_rets.df$TimeBins <- factor(row.names(correl2_rets.df))
correl2_rets.df <- correl2_rets.df[,c("V2", "TimeBins")]
CUMULCORREL2_OFRBID_rets <- merge(CUMULCORREL2_OFRBID_rets, correl2_rets.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus2_OFRBID_rets_1sec, 10*60), 'minutes', k=10)
correl2_rets_1sec<- period.apply(align.time(near_plus2_OFRBID_rets_1sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl2_rets_1sec))), 2, substr, 12, 19)
correl2_rets_1sec.df <- as.data.frame(correl2_rets_1sec)
row.names(correl2_rets_1sec.df) <- temp 
colnames(correl2_rets_1sec.df) <- c("V1", "V2", "V3", "V4")
correl2_rets_1sec.df$TimeBins <- factor(row.names(correl2_rets_1sec.df))
correl2_rets_1sec.df <- correl2_rets_1sec.df[,c("V2", "TimeBins")]
CUMULCORREL2_OFRBID_rets_1sec <- merge(CUMULCORREL2_OFRBID_rets_1sec, correl2_rets_1sec.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus2_OFRBID_rets_10sec, 10*60), 'minutes', k=10)
correl2_rets_10sec<- period.apply(align.time(near_plus2_OFRBID_rets_10sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl2_rets_10sec))), 2, substr, 12, 19)
correl2_rets_10sec.df <- as.data.frame(correl2_rets_10sec)
row.names(correl2_rets_10sec.df) <- temp 
colnames(correl2_rets_10sec.df) <- c("V1", "V2", "V3", "V4")
correl2_rets_10sec.df$TimeBins <- factor(row.names(correl2_rets_10sec.df))
correl2_rets_10sec.df <- correl2_rets_10sec.df[,c("V2", "TimeBins")]
CUMULCORREL2_OFRBID_rets_10sec <- merge(CUMULCORREL2_OFRBID_rets_10sec, correl2_rets_10sec.df, by= "TimeBins", all= TRUE)

# OFRBIDS, 'Returns' - no zeros
ep <- endpoints(align.time(near_plus2_OFRBID_rets_no0s, 10*60), 'minutes', k=10)
correl2_rets_no0s<- period.apply(align.time(near_plus2_OFRBID_rets_no0s, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl2_rets_no0s))), 2, substr, 12, 19)
correl2_rets_no0s.df <- as.data.frame(correl2_rets_no0s)
row.names(correl2_rets_no0s.df) <- temp 
colnames(correl2_rets_no0s.df) <- c("V1", "V2", "V3", "V4")
correl2_rets_no0s.df$TimeBins <- factor(row.names(correl2_rets_no0s.df))
correl2_rets_no0s.df <- correl2_rets_no0s.df[,c("V2", "TimeBins")]
CUMULCORREL2_OFRBID_rets_no0s <- merge(CUMULCORREL2_OFRBID_rets_no0s, correl2_rets_no0s.df, by= "TimeBins", all= TRUE)


# ######################################################################################## 
#  
# 
# #Nearby and plus3
# ########################################################################################  

plus3_BID         <- to.period(qplus3$BID, period = 'seconds', k = 1, OHLC = FALSE)
plus3_BID_rets    <- diff.xts(plus3_BID, lag = 1, differences = 1, arithmetic = TRUE, log = TRUE, na.pad = TRUE)
non_zeros         <- index(plus3_BID_rets)[which(plus3_BID_rets != 0)]
p3secs_to_update_BID_rets  <-difftime(non_zeros[1:(length(non_zeros)-1)], non_zeros[2:length(non_zeros)], unit='secs')
plus3_BID_rets_no0s<- subset(plus3_BID_rets, BID !=0)
plus3_BID_rets_1sec<- lag.xts(plus3_BID_rets, k=-1)
plus3_BID_rets_10sec<- lag.xts(plus3_BID_rets, k=-10)

plus3_OFR         <- to.period(qplus3$OFR, period = 'seconds', k = 1, OHLC = FALSE)
plus3_OFR_rets    <- diff.xts(plus3_OFR, lag = 1, differences = 1, arithmetic = TRUE, log = TRUE, na.pad = TRUE)
non_zeros         <- index(plus3_OFR_rets)[which(plus3_OFR_rets != 0)]
p3secs_to_update_OFR_rets  <-difftime(non_zeros[1:(length(non_zeros)-1)], non_zeros[2:length(non_zeros)], unit='secs')
plus3_OFR_rets_no0s<- subset(plus3_OFR_rets, OFR !=0)
plus3_OFR_rets_1sec<- lag.xts(plus3_OFR_rets, k=-1)
plus3_OFR_rets_10sec<- lag.xts(plus3_OFR_rets, k=-10)

# Making contract pairs
# BIDS, Levels
near_plus3_BID        <- merge(nearby_BID, plus3_BID, all = TRUE, fill = NA, join = "outer", retside = TRUE, 
                               retclass = "xts")                
near_plus3_BID.df     <- as.data.frame(near_plus3_BID)

# BIDS, 'Returns'
#Keep zeroes
near_plus3_BID_rets   <- merge(nearby_BID_rets, plus3_BID_rets, all = TRUE, fill = NA, join = "outer", 
                               retside = TRUE, retclass = "xts")                
near_plus3_BID_rets.df<- as.data.frame(near_plus3_BID_rets)
near_plus3_BID_rets_1sec   <- merge(nearby_BID_rets, plus3_BID_rets_1sec, all = TRUE, fill = NA, join = "outer", 
                                    retside = TRUE, retclass = "xts")                
near_plus3_BID_rets_1sec.df<- as.data.frame(near_plus3_BID_rets_1sec)
near_plus3_BID_rets_10sec   <- merge(nearby_BID_rets, plus3_BID_rets_10sec, all = TRUE, fill = NA, join = "outer", 
                                     retside = TRUE, retclass = "xts")                
near_plus3_BID_rets_10sec.df<- as.data.frame(near_plus3_BID_rets_10sec)

# No zeros
near_plus3_BID_rets_no0s   <- merge(nearby_BID_rets_no0s, plus3_BID_rets_no0s, all = TRUE, fill = NA, join = "outer", 
                                    retside = TRUE, retclass = "xts")                
near_plus3_BID_rets_no0s.df<- as.data.frame(near_plus3_BID_rets_no0s)

# OFR, Levels
near_plus3_OFR        <- merge(nearby_OFR, plus3_OFR, all = TRUE, fill = NA, join = "outer", retside = TRUE, 
                               retclass = "xts")                
near_plus3_OFR.df     <- as.data.frame(near_plus3_OFR)

# 'Returns'
near_plus3_OFR_rets   <- merge(nearby_OFR_rets, plus3_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                               retside = TRUE, retclass = "xts")                
near_plus3_OFR_rets.df<- as.data.frame(near_plus3_OFR_rets)
near_plus3_OFR_rets_1sec   <- merge(nearby_OFR_rets, plus3_OFR_rets_1sec, all = TRUE, fill = NA, join = "outer", 
                                    retside = TRUE, retclass = "xts")                
near_plus3_OFR_rets_1sec.df<- as.data.frame(near_plus3_OFR_rets_1sec)
near_plus3_OFR_rets_10sec   <- merge(nearby_OFR_rets, plus3_OFR_rets_10sec, all = TRUE, fill = NA, join = "outer", 
                                     retside = TRUE, retclass = "xts")                
near_plus3_OFR_rets_10sec.df<- as.data.frame(near_plus3_OFR_rets_10sec)

# No zeros
near_plus3_OFR_rets_no0s   <- merge(nearby_OFR_rets_no0s, plus3_OFR_rets_no0s, all = TRUE, fill = NA, join = "outer", 
                                    retside = TRUE, retclass = "xts")                
near_plus3_OFR_rets_no0s.df<- as.data.frame(near_plus3_OFR_rets_no0s)


# Bid-to-OFR
near_plus3_BIDOFR_rets   <- merge(nearby_BID_rets, plus3_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                                  retside = TRUE, retclass = "xts")                
near_plus3_BIDOFR_rets.df<- as.data.frame(near_plus3_BIDOFR_rets)
near_plus3_BIDOFR_rets_1sec   <- merge(nearby_BID_rets_no0s_1sec, plus3_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                                       retside = TRUE, retclass = "xts")                
near_plus3_BIDOFR_rets_1sec.df<- as.data.frame(near_plus3_BIDOFR_rets_1sec)
near_plus3_BIDOFR_rets_10sec   <- merge(nearby_BID_rets_no0s_10sec, plus3_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                                        retside = TRUE, retclass = "xts")                
near_plus3_BIDOFR_rets_10sec.df<- as.data.frame(near_plus3_BIDOFR_rets_10sec)

# No zeros
near_plus3_BIDOFR_rets_no0s   <- merge(nearby_BID_rets_no0s, plus3_OFR_rets_no0s, all = TRUE, fill = NA, join = "outer", 
                                       retside = TRUE, retclass = "xts")                
near_plus3_BIDOFR_rets_no0s.df<- as.data.frame(near_plus3_BIDOFR_rets_no0s)

# OFR-to-Bid
near_plus3_OFRBID_rets   <- merge(nearby_BID_rets, plus3_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                                  retside = TRUE, retclass = "xts")                
near_plus3_OFRBID_rets.df<- as.data.frame(near_plus3_OFRBID_rets)
near_plus3_OFRBID_rets_1sec   <- merge(nearby_BID_rets_no0s_1sec, plus3_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                                       retside = TRUE, retclass = "xts")                
near_plus3_OFRBID_rets_1sec.df<- as.data.frame(near_plus3_OFRBID_rets_1sec)
near_plus3_OFRBID_rets_10sec   <- merge(nearby_BID_rets_no0s_10sec, plus3_OFR_rets, all = TRUE, fill = NA, join = "outer", 
                                        retside = TRUE, retclass = "xts")                
near_plus3_OFRBID_rets_10sec.df<- as.data.frame(near_plus3_OFRBID_rets_10sec)

# No zeros
near_plus3_OFRBID_rets_no0s   <- merge(nearby_BID_rets_no0s, plus3_OFR_rets_no0s, all = TRUE, fill = NA, join = "outer", 
                                       retside = TRUE, retclass = "xts")                
near_plus3_OFRBID_rets_no0s.df<- as.data.frame(near_plus3_OFRBID_rets_no0s)


# In time bins - ten minutes
# Date in the CUMULCORREL1_BID timestamp is irrelevant. We are creating 10 minute bins, in which we place 
# correlations from each day in columns. Last time stamp says 13:19:50, but really it is 13:15:50
# but in the timeSequence function, partial bins are not allowed and 13:09:59 to 13:15:59 is not a full 
# ten minutes.

# BIDS, 'Returns'
ep <- endpoints(align.time(near_plus3_BID_rets, 10*60), 'minutes', k=10)
correl3_rets<- period.apply(align.time(near_plus3_BID_rets, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl3_rets))), 2, substr, 12, 19)
correl3_rets.df <- as.data.frame(correl3_rets)
row.names(correl3_rets.df) <- temp 
colnames(correl3_rets.df) <- c("V1", "V2", "V3", "V4")
correl3_rets.df$TimeBins <- factor(row.names(correl3_rets.df))
correl3_rets.df <- correl3_rets.df[,c("V2", "TimeBins")]
CUMULCORREL3_BID_rets <- merge(CUMULCORREL3_BID_rets, correl3_rets.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus3_BID_rets_1sec, 10*60), 'minutes', k=10)
correl3_rets_1sec<- period.apply(align.time(near_plus3_BID_rets_1sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl3_rets_1sec))), 2, substr, 12, 19)
correl3_rets_1sec.df <- as.data.frame(correl3_rets_1sec)
row.names(correl3_rets_1sec.df) <- temp 
colnames(correl3_rets_1sec.df) <- c("V1", "V2", "V3", "V4")
correl3_rets_1sec.df$TimeBins <- factor(row.names(correl3_rets_1sec.df))
correl3_rets_1sec.df <- correl3_rets_1sec.df[,c("V2", "TimeBins")]
CUMULCORREL3_BID_rets_1sec <- merge(CUMULCORREL3_BID_rets_1sec, correl3_rets_1sec.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus3_BID_rets_10sec, 10*60), 'minutes', k=10)
correl3_rets_10sec<- period.apply(align.time(near_plus3_BID_rets_10sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl3_rets_10sec))), 2, substr, 12, 19)
correl3_rets_10sec.df <- as.data.frame(correl3_rets_10sec)
row.names(correl3_rets_10sec.df) <- temp 
colnames(correl3_rets_10sec.df) <- c("V1", "V2", "V3", "V4")
correl3_rets_10sec.df$TimeBins <- factor(row.names(correl3_rets_10sec.df))
correl3_rets_10sec.df <- correl3_rets_10sec.df[,c("V2", "TimeBins")]
CUMULCORREL3_BID_rets_10sec <- merge(CUMULCORREL3_BID_rets_10sec, correl3_rets_10sec.df, by= "TimeBins", all= TRUE)

# BIDS, 'Returns' - no zeros
ep <- endpoints(align.time(near_plus3_BID_rets_no0s, 10*60), 'minutes', k=10)
correl3_rets_no0s<- period.apply(align.time(near_plus3_BID_rets_no0s, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl3_rets_no0s))), 2, substr, 12, 19)
correl3_rets_no0s.df <- as.data.frame(correl3_rets_no0s)
row.names(correl3_rets_no0s.df) <- temp 
colnames(correl3_rets_no0s.df) <- c("V1", "V2", "V3", "V4")
correl3_rets_no0s.df$TimeBins <- factor(row.names(correl3_rets_no0s.df))
correl3_rets_no0s.df <- correl3_rets_no0s.df[,c("V2", "TimeBins")]
CUMULCORREL3_BID_rets_no0s <- merge(CUMULCORREL3_BID_rets_no0s, correl3_rets_no0s.df, by= "TimeBins", all= TRUE)

# OFRS, 'Returns'
ep <- endpoints(align.time(near_plus3_OFR_rets, 10*60), 'minutes', k=10)
correl3_rets<- period.apply(align.time(near_plus3_OFR_rets, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl3_rets))), 2, substr, 12, 19)
correl3_rets.df <- as.data.frame(correl3_rets)
row.names(correl3_rets.df) <- temp 
colnames(correl3_rets.df) <- c("V1", "V2", "V3", "V4")
correl3_rets.df$TimeBins <- factor(row.names(correl3_rets.df))
correl3_rets.df <- correl3_rets.df[,c("V2", "TimeBins")]
CUMULCORREL3_OFR_rets <- merge(CUMULCORREL3_OFR_rets, correl3_rets.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus3_OFR_rets_1sec, 10*60), 'minutes', k=10)
correl3_rets_1sec<- period.apply(align.time(near_plus3_OFR_rets_1sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl3_rets_1sec))), 2, substr, 12, 19)
correl3_rets_1sec.df <- as.data.frame(correl3_rets_1sec)
row.names(correl3_rets_1sec.df) <- temp 
colnames(correl3_rets_1sec.df) <- c("V1", "V2", "V3", "V4")
correl3_rets_1sec.df$TimeBins <- factor(row.names(correl3_rets_1sec.df))
correl3_rets_1sec.df <- correl3_rets_1sec.df[,c("V2", "TimeBins")]
CUMULCORREL3_OFR_rets_1sec <- merge(CUMULCORREL3_OFR_rets_1sec, correl3_rets_1sec.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus3_OFR_rets_10sec, 10*60), 'minutes', k=10)
correl3_rets_10sec<- period.apply(align.time(near_plus3_OFR_rets_10sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl3_rets_10sec))), 2, substr, 12, 19)
correl3_rets_10sec.df <- as.data.frame(correl3_rets_10sec)
row.names(correl3_rets_10sec.df) <- temp 
colnames(correl3_rets_10sec.df) <- c("V1", "V2", "V3", "V4")
correl3_rets_10sec.df$TimeBins <- factor(row.names(correl3_rets_10sec.df))
correl3_rets_10sec.df <- correl3_rets_10sec.df[,c("V2", "TimeBins")]
CUMULCORREL3_OFR_rets_10sec <- merge(CUMULCORREL3_OFR_rets_10sec, correl3_rets_10sec.df, by= "TimeBins", all= TRUE)

# OFRS, 'Returns' - no zeros
ep <- endpoints(align.time(near_plus3_OFR_rets_no0s, 10*60), 'minutes', k=10)
correl3_rets_no0s<- period.apply(align.time(near_plus3_OFR_rets_no0s, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl3_rets_no0s))), 2, substr, 12, 19)
correl3_rets_no0s.df <- as.data.frame(correl3_rets_no0s)
row.names(correl3_rets_no0s.df) <- temp 
colnames(correl3_rets_no0s.df) <- c("V1", "V2", "V3", "V4")
correl3_rets_no0s.df$TimeBins <- factor(row.names(correl3_rets_no0s.df))
correl3_rets_no0s.df <- correl3_rets_no0s.df[,c("V2", "TimeBins")]
CUMULCORREL3_OFR_rets_no0s <- merge(CUMULCORREL3_OFR_rets_no0s, correl3_rets_no0s.df, by= "TimeBins", all= TRUE)


# BIDOFRS, 'Returns'
ep <- endpoints(align.time(near_plus3_BIDOFR_rets, 10*60), 'minutes', k=10)
correl3_rets<- period.apply(align.time(near_plus3_BIDOFR_rets, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl3_rets))), 2, substr, 12, 19)
correl3_rets.df <- as.data.frame(correl3_rets)
row.names(correl3_rets.df) <- temp 
colnames(correl3_rets.df) <- c("V1", "V2", "V3", "V4")
correl3_rets.df$TimeBins <- factor(row.names(correl3_rets.df))
correl3_rets.df <- correl3_rets.df[,c("V2", "TimeBins")]
CUMULCORREL3_BIDOFR_rets <- merge(CUMULCORREL3_BIDOFR_rets, correl3_rets.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus3_BIDOFR_rets_1sec, 10*60), 'minutes', k=10)
correl3_rets_1sec<- period.apply(align.time(near_plus3_BIDOFR_rets_1sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl3_rets_1sec))), 2, substr, 12, 19)
correl3_rets_1sec.df <- as.data.frame(correl3_rets_1sec)
row.names(correl3_rets_1sec.df) <- temp 
colnames(correl3_rets_1sec.df) <- c("V1", "V2", "V3", "V4")
correl3_rets_1sec.df$TimeBins <- factor(row.names(correl3_rets_1sec.df))
correl3_rets_1sec.df <- correl3_rets_1sec.df[,c("V2", "TimeBins")]
CUMULCORREL3_BIDOFR_rets_1sec <- merge(CUMULCORREL3_BIDOFR_rets_1sec, correl3_rets_1sec.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus3_BIDOFR_rets_10sec, 10*60), 'minutes', k=10)
correl3_rets_10sec<- period.apply(align.time(near_plus3_BIDOFR_rets_10sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl3_rets_10sec))), 2, substr, 12, 19)
correl3_rets_10sec.df <- as.data.frame(correl3_rets_10sec)
row.names(correl3_rets_10sec.df) <- temp 
colnames(correl3_rets_10sec.df) <- c("V1", "V2", "V3", "V4")
correl3_rets_10sec.df$TimeBins <- factor(row.names(correl3_rets_10sec.df))
correl3_rets_10sec.df <- correl3_rets_10sec.df[,c("V2", "TimeBins")]
CUMULCORREL3_BIDOFR_rets_10sec <- merge(CUMULCORREL3_BIDOFR_rets_10sec, correl3_rets_10sec.df, by= "TimeBins", all= TRUE)

# BIDOFRS, 'Returns' - no zeros
ep <- endpoints(align.time(near_plus3_BIDOFR_rets_no0s, 10*60), 'minutes', k=10)
correl3_rets_no0s<- period.apply(align.time(near_plus3_BIDOFR_rets_no0s, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl3_rets_no0s))), 2, substr, 12, 19)
correl3_rets_no0s.df <- as.data.frame(correl3_rets_no0s)
row.names(correl3_rets_no0s.df) <- temp 
colnames(correl3_rets_no0s.df) <- c("V1", "V2", "V3", "V4")
correl3_rets_no0s.df$TimeBins <- factor(row.names(correl3_rets_no0s.df))
correl3_rets_no0s.df <- correl3_rets_no0s.df[,c("V2", "TimeBins")]
CUMULCORREL3_BIDOFR_rets_no0s <- merge(CUMULCORREL3_BIDOFR_rets_no0s, correl3_rets_no0s.df, by= "TimeBins", all= TRUE)    
# OFRBIDS, 'Returns'
ep <- endpoints(align.time(near_plus3_OFRBID_rets, 10*60), 'minutes', k=10)
correl3_rets<- period.apply(align.time(near_plus3_OFRBID_rets, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl3_rets))), 2, substr, 12, 19)
correl3_rets.df <- as.data.frame(correl3_rets)
row.names(correl3_rets.df) <- temp 
colnames(correl3_rets.df) <- c("V1", "V2", "V3", "V4")
correl3_rets.df$TimeBins <- factor(row.names(correl3_rets.df))
correl3_rets.df <- correl3_rets.df[,c("V2", "TimeBins")]
CUMULCORREL3_OFRBID_rets <- merge(CUMULCORREL3_OFRBID_rets, correl3_rets.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus3_OFRBID_rets_1sec, 10*60), 'minutes', k=10)
correl3_rets_1sec<- period.apply(align.time(near_plus3_OFRBID_rets_1sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl3_rets_1sec))), 2, substr, 12, 19)
correl3_rets_1sec.df <- as.data.frame(correl3_rets_1sec)
row.names(correl3_rets_1sec.df) <- temp 
colnames(correl3_rets_1sec.df) <- c("V1", "V2", "V3", "V4")
correl3_rets_1sec.df$TimeBins <- factor(row.names(correl3_rets_1sec.df))
correl3_rets_1sec.df <- correl3_rets_1sec.df[,c("V2", "TimeBins")]
CUMULCORREL3_OFRBID_rets_1sec <- merge(CUMULCORREL3_OFRBID_rets_1sec, correl3_rets_1sec.df, by= "TimeBins", all= TRUE)

ep <- endpoints(align.time(near_plus3_OFRBID_rets_10sec, 10*60), 'minutes', k=10)
correl3_rets_10sec<- period.apply(align.time(near_plus3_OFRBID_rets_10sec, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl3_rets_10sec))), 2, substr, 12, 19)
correl3_rets_10sec.df <- as.data.frame(correl3_rets_10sec)
row.names(correl3_rets_10sec.df) <- temp 
colnames(correl3_rets_10sec.df) <- c("V1", "V2", "V3", "V4")
correl3_rets_10sec.df$TimeBins <- factor(row.names(correl3_rets_10sec.df))
correl3_rets_10sec.df <- correl3_rets_10sec.df[,c("V2", "TimeBins")]
CUMULCORREL3_OFRBID_rets_10sec <- merge(CUMULCORREL3_OFRBID_rets_10sec, correl3_rets_10sec.df, by= "TimeBins", all= TRUE)

# OFRBIDS, 'Returns' - no zeros
ep <- endpoints(align.time(near_plus3_OFRBID_rets_no0s, 10*60), 'minutes', k=10)
correl3_rets_no0s<- period.apply(align.time(near_plus3_OFRBID_rets_no0s, 10*60), INDEX=ep, FUN=cor, use = "pairwise.complete.obs")
temp <- apply(as.matrix(rownames(as.data.frame(correl3_rets_no0s))), 2, substr, 12, 19)
correl3_rets_no0s.df <- as.data.frame(correl3_rets_no0s)
row.names(correl3_rets_no0s.df) <- temp 
colnames(correl3_rets_no0s.df) <- c("V1", "V2", "V3", "V4")
correl3_rets_no0s.df$TimeBins <- factor(row.names(correl3_rets_no0s.df))
correl3_rets_no0s.df <- correl3_rets_no0s.df[,c("V2", "TimeBins")]
CUMULCORREL3_OFRBID_rets_no0s <- merge(CUMULCORREL3_OFRBID_rets_no0s, correl3_rets_no0s.df, by= "TimeBins", all= TRUE)
# ######################################################################################## 


}

#######################################
# Plotting summaries - keep zeros
  # Bids Contemporaneous Plus1
  CUMULCORREL1_BID_rets$MEANS <- apply(CUMULCORREL1_BID_rets[,2:dim(CUMULCORREL1_BID_rets)[2]], 1, mean, na.rm = TRUE)
  CUMULCORREL1_BID_rets$sdS <- apply(CUMULCORREL1_BID_rets[,2:dim(CUMULCORREL1_BID_rets)[2]], 1, sd, na.rm = TRUE) 
  CUMULCORREL1_BID_rets$contract <- factor("1Deferred")  

  # Bids Contemporaneous Plus2
  CUMULCORREL2_BID_rets$MEANS <- apply(CUMULCORREL2_BID_rets[,2:dim(CUMULCORREL2_BID_rets)[2]], 1, mean, na.rm = TRUE)
  CUMULCORREL2_BID_rets$sdS <- apply(CUMULCORREL2_BID_rets[,2:dim(CUMULCORREL2_BID_rets)[2]], 1, sd, na.rm = TRUE) 
  CUMULCORREL2_BID_rets$contract <- factor("2Deferred")

  # Bids Contemporaneous Plus3
  CUMULCORREL3_BID_rets$MEANS <- apply(CUMULCORREL3_BID_rets[,2:dim(CUMULCORREL3_BID_rets)[2]], 1, mean, na.rm = TRUE)
  CUMULCORREL3_BID_rets$sdS <- apply(CUMULCORREL3_BID_rets[,2:dim(CUMULCORREL3_BID_rets)[2]], 1, sd, na.rm = TRUE) 
  CUMULCORREL3_BID_rets$contract <- factor("3Deferred")

  CUMULCORREL_BID_rets <- rbind(CUMULCORREL1_BID_rets, CUMULCORREL2_BID_rets, CUMULCORREL3_BID_rets)
  
  pd <- position_dodge(0.4)
  MAXES <- min(CUMULCORREL_BID_rets$MEANS - CUMULCORREL_BID_rets$sdS,1)
  MINS <- max(CUMULCORREL_BID_rets$MEANS - CUMULCORREL_BID_rets$sdS,0)
  Bid_plot <- ggplot(CUMULCORREL_BID_rets, aes(TimeBins, MEANS, ymin = MEANS-sdS, 
                                                 ymax = MEANS+sdS, colour=contract, group=contract) ) + 
    geom_errorbar(size=1, position=pd) +
    geom_point(size=4, position=pd) + 
    geom_line(size=0.25, position=pd) +
    ggtitle('Contemporaneous Correlation with Nearby in BIDs') +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45), axis.title.x=element_blank(), 
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major = element_line(colour = "grey")) +
    #scale_colour_grey() + 
    ylab("Correlation") +
  coord_cartesian(ylim = c(-.2, 1.2))+
  scale_y_continuous(minor_breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1.0, 1.1), breaks = c(0, .2, .4, .6, .8, 1., 1.2))
ggsave(file="Bid_plot.png", path='C:/Users/mallorym/Documents/GitHub/BBOBAS', scale=1, height=4, width=8, units="in")



###############################
# OFRs Contemporaneous Plus1
CUMULCORREL1_OFR_rets$MEANS <- apply(CUMULCORREL1_OFR_rets[,2:dim(CUMULCORREL1_OFR_rets)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL1_OFR_rets$sdS <- apply(CUMULCORREL1_OFR_rets[,2:dim(CUMULCORREL1_OFR_rets)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL1_OFR_rets$contract <- factor("1Deferred")  

# OFRs Contemporaneous Plus2
CUMULCORREL2_OFR_rets$MEANS <- apply(CUMULCORREL2_OFR_rets[,2:dim(CUMULCORREL2_OFR_rets)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL2_OFR_rets$sdS <- apply(CUMULCORREL2_OFR_rets[,2:dim(CUMULCORREL2_OFR_rets)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL2_OFR_rets$contract <- factor("2Deferred")

# OFRs Contemporaneous Plus3
CUMULCORREL3_OFR_rets$MEANS <- apply(CUMULCORREL3_OFR_rets[,2:dim(CUMULCORREL3_OFR_rets)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL3_OFR_rets$sdS <- apply(CUMULCORREL3_OFR_rets[,2:dim(CUMULCORREL3_OFR_rets)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL3_OFR_rets$contract <- factor("3Deferred")

CUMULCORREL_OFR_rets <- rbind(CUMULCORREL1_OFR_rets, CUMULCORREL2_OFR_rets, CUMULCORREL3_OFR_rets)

pd <- position_dodge(0.4)
MAXES <- min(CUMULCORREL_OFR_rets$MEANS - CUMULCORREL_OFR_rets$sdS,1)
MINS <- max(CUMULCORREL_OFR_rets$MEANS - CUMULCORREL_OFR_rets$sdS,0)
OFR_plot <- ggplot(CUMULCORREL_OFR_rets, aes(TimeBins, MEANS, ymin = MEANS-sdS, 
                                             ymax = MEANS+sdS, colour=contract, group=contract) ) + 
  geom_errorbar(size=1, position=pd) +
  geom_point(size=4, position=pd) + 
  geom_line(size=0.25, position=pd) +
  ggtitle('Contemporaneous Correlation with Nearby in OFRs') +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45), axis.title.x=element_blank(), 
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major = element_line(colour = "grey")) +
  #scale_colour_grey() + 
  ylab("Correlation")+
  coord_cartesian(ylim = c(-.2, 1.2))+
  scale_y_continuous(minor_breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1.0, 1.1), breaks = c(0, .2, .4, .6, .8, 1., 1.2))
ggsave(file="OFR_plot.png", path='C:/Users/mallorym/Documents/GitHub/BBOBAS', scale=1, height=4, width=8, units="in")
#########################################################################

#######################################
# Plotting summaries - No zeros
# Bids Contemporaneous Plus1
CUMULCORREL1_BID_rets_no0s$MEANS <- apply(CUMULCORREL1_BID_rets_no0s[,2:dim(CUMULCORREL1_BID_rets_no0s)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL1_BID_rets_no0s$sdS <- apply(CUMULCORREL1_BID_rets_no0s[,2:dim(CUMULCORREL1_BID_rets_no0s)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL1_BID_rets_no0s$contract <- factor("1Deferred")  

# Bids Contemporaneous Plus2
CUMULCORREL2_BID_rets_no0s$MEANS <- apply(CUMULCORREL2_BID_rets_no0s[,2:dim(CUMULCORREL2_BID_rets_no0s)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL2_BID_rets_no0s$sdS <- apply(CUMULCORREL2_BID_rets_no0s[,2:dim(CUMULCORREL2_BID_rets_no0s)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL2_BID_rets_no0s$contract <- factor("2Deferred")

# Bids Contemporaneous Plus3
CUMULCORREL3_BID_rets_no0s$MEANS <- apply(CUMULCORREL3_BID_rets_no0s[,2:dim(CUMULCORREL3_BID_rets_no0s)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL3_BID_rets_no0s$sdS <- apply(CUMULCORREL3_BID_rets_no0s[,2:dim(CUMULCORREL3_BID_rets_no0s)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL3_BID_rets_no0s$contract <- factor("3Deferred")

CUMULCORREL_BID_rets_no0s <- rbind(CUMULCORREL1_BID_rets_no0s, CUMULCORREL2_BID_rets_no0s, CUMULCORREL3_BID_rets_no0s)

pd <- position_dodge(0.4)
MAXES <- min(CUMULCORREL_BID_rets_no0s$MEANS - CUMULCORREL_BID_rets_no0s$sdS,1)
MINS <- max(CUMULCORREL_BID_rets_no0s$MEANS - CUMULCORREL_BID_rets_no0s$sdS,0)
Bid_plot_no0s <- ggplot(CUMULCORREL_BID_rets_no0s, aes(TimeBins, MEANS, ymin = MEANS-sdS, 
                                             ymax = MEANS+sdS, colour=contract, group=contract) ) + 
  geom_errorbar(size=1, position=pd) +
  geom_point(size=4, position=pd) + 
  geom_line(size=0.25, position=pd) +
  ggtitle('Contemporaneous Correlation with Nearby in BIDs - Information-Based') +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45), axis.title.x=element_blank(), 
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major = element_line(colour = "grey")) +
  #scale_colour_grey() + 
  ylab("Correlation") +
  coord_cartesian(ylim = c(-.2, 1.2))+
  scale_y_continuous(minor_breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1.0, 1.1), breaks = c(0, .2, .4, .6, .8, 1., 1.2))
ggsave(file="Bid_plot_no0s.png", path='C:/Users/mallorym/Documents/GitHub/BBOBAS', scale=1, height=4, width=8, units="in")

###############################
# OFRs Contemporaneous Plus1
CUMULCORREL1_OFR_rets_no0s$MEANS <- apply(CUMULCORREL1_OFR_rets_no0s[,2:dim(CUMULCORREL1_OFR_rets_no0s)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL1_OFR_rets_no0s$sdS <- apply(CUMULCORREL1_OFR_rets_no0s[,2:dim(CUMULCORREL1_OFR_rets_no0s)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL1_OFR_rets_no0s$contract <- factor("1Deferred")  

# OFRs Contemporaneous Plus2
CUMULCORREL2_OFR_rets_no0s$MEANS <- apply(CUMULCORREL2_OFR_rets_no0s[,2:dim(CUMULCORREL2_OFR_rets_no0s)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL2_OFR_rets_no0s$sdS <- apply(CUMULCORREL2_OFR_rets_no0s[,2:dim(CUMULCORREL2_OFR_rets_no0s)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL2_OFR_rets_no0s$contract <- factor("2Deferred")

# OFRs Contemporaneous Plus3
CUMULCORREL3_OFR_rets_no0s$MEANS <- apply(CUMULCORREL3_OFR_rets_no0s[,2:dim(CUMULCORREL3_OFR_rets_no0s)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL3_OFR_rets_no0s$sdS <- apply(CUMULCORREL3_OFR_rets_no0s[,2:dim(CUMULCORREL3_OFR_rets_no0s)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL3_OFR_rets_no0s$contract <- factor("3Deferred")

CUMULCORREL_OFR_rets_no0s <- rbind(CUMULCORREL1_OFR_rets_no0s, CUMULCORREL2_OFR_rets_no0s, CUMULCORREL3_OFR_rets_no0s)

pd <- position_dodge(0.4)
MAXES <- min(CUMULCORREL_OFR_rets_no0s$MEANS - CUMULCORREL_OFR_rets_no0s$sdS,1)
MINS <- max(CUMULCORREL_OFR_rets_no0s$MEANS - CUMULCORREL_OFR_rets_no0s$sdS,0)
OFR_plot_no0s <- ggplot(CUMULCORREL_OFR_rets_no0s, aes(TimeBins, MEANS, ymin = MEANS-sdS, 
                                             ymax = MEANS+sdS, colour=contract, group=contract) ) + 
  geom_errorbar(size=1, position=pd) +
  geom_point(size=4, position=pd) + 
  geom_line(size=0.25, position=pd) +
  ggtitle('Contemporaneous Correlation with Nearby in OFRs - Information-Based') +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45), axis.title.x=element_blank(), 
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major = element_line(colour = "grey")) +
  #scale_colour_grey() + 
  ylab("Correlation") +
  coord_cartesian(ylim = c(-.2, 1.2))+
  scale_y_continuous(minor_breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1.0, 1.1), breaks = c(0, .2, .4, .6, .8, 1., 1.2))
ggsave(file="OFR_plot_no0s.png", path='C:/Users/mallorym/Documents/GitHub/BBOBAS', scale=1, height=4, width=8, units="in")

#########################################################################
#######################################
# Plotting summaries - Time Lags keep zeros
# Bids Plus1
CUMULCORREL1_BID_rets_1sec$MEANS <- apply(CUMULCORREL1_BID_rets_1sec[,2:dim(CUMULCORREL1_BID_rets_1sec)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL1_BID_rets_1sec$sdS <- apply(CUMULCORREL1_BID_rets_1sec[,2:dim(CUMULCORREL1_BID_rets_1sec)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL1_BID_rets_1sec$contract <- factor("1Deferred")
CUMULCORREL1_BID_rets_1sec$lag <- factor("One Second")

CUMULCORREL1_BID_rets_10sec$MEANS <- apply(CUMULCORREL1_BID_rets_10sec[,2:dim(CUMULCORREL1_BID_rets_10sec)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL1_BID_rets_10sec$sdS <- apply(CUMULCORREL1_BID_rets_10sec[,2:dim(CUMULCORREL1_BID_rets_10sec)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL1_BID_rets_10sec$contract <- factor("1Deferred")  
CUMULCORREL1_BID_rets_10sec$lag <- factor("Ten Seconds")  

# Bids  Plus2
CUMULCORREL2_BID_rets_1sec$MEANS <- apply(CUMULCORREL2_BID_rets_1sec[,2:dim(CUMULCORREL2_BID_rets_1sec)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL2_BID_rets_1sec$sdS <- apply(CUMULCORREL2_BID_rets_1sec[,2:dim(CUMULCORREL2_BID_rets_1sec)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL2_BID_rets_1sec$contract <- factor("2Deferred")

# Bids  Plus3
CUMULCORREL3_BID_rets_1sec$MEANS <- apply(CUMULCORREL3_BID_rets_1sec[,2:dim(CUMULCORREL3_BID_rets_1sec)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL3_BID_rets_1sec$sdS <- apply(CUMULCORREL3_BID_rets_1sec[,2:dim(CUMULCORREL3_BID_rets_1sec)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL3_BID_rets_1sec$contract <- factor("3Deferred")

CUMULCORREL1_BID_rets_no0s$lag <- factor("Contemporaneous")
CUMULCORREL_BID_rets_timelag <- rbind(CUMULCORREL1_BID_rets_no0s, CUMULCORREL1_BID_rets_1sec, CUMULCORREL1_BID_rets_10sec)

pd <- position_dodge(0.4)
MAXES <- min(CUMULCORREL_BID_rets_timelag$MEANS - CUMULCORREL_BID_rets_timelag$sdS,1)
MINS <- max(CUMULCORREL_BID_rets_timelag$MEANS - CUMULCORREL_BID_rets_timelag$sdS,0)
Bid_plot_timelag <- ggplot(CUMULCORREL_BID_rets_timelag, aes(TimeBins, MEANS, ymin = MEANS-sdS, 
                                             ymax = MEANS+sdS, colour=lag, group=lag) ) + 
  geom_errorbar(size=1, position=pd) +
  geom_point(size=4, position=pd) + 
  geom_line(size=0.25, position=pd) +
  ggtitle('Nearby and 1 Deferred Correlation, BID') +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45), axis.title.x=element_blank(), 
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major = element_line(colour = "grey")) +
  #scale_colour_grey() + 
  ylab("Correlation") +
  coord_cartesian(ylim = c(-.2, 1.2))+
  scale_y_continuous(minor_breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1.0, 1.1), breaks = c(0, .2, .4, .6, .8, 1., 1.2))
ggsave(file="Bid_plot_timelag.png", path='C:/Users/mallorym/Documents/GitHub/BBOBAS', scale=1, height=4, width=8, units="in")

###############################
#######################################
# Plotting summaries - Time Lags keep zeros
# Plus1 Bids
CUMULCORREL1N_BID_rets_1sec$MEANS <- apply(CUMULCORREL1N_BID_rets_1sec[,2:dim(CUMULCORREL1N_BID_rets_1sec)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL1N_BID_rets_1sec$sdS <- apply(CUMULCORREL1N_BID_rets_1sec[,2:dim(CUMULCORREL1N_BID_rets_1sec)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL1N_BID_rets_1sec$contract <- factor("1Deferred")
CUMULCORREL1N_BID_rets_1sec$lag <- factor("One Second")

CUMULCORREL1N_BID_rets_10sec$MEANS <- apply(CUMULCORREL1N_BID_rets_10sec[,2:dim(CUMULCORREL1N_BID_rets_10sec)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL1N_BID_rets_10sec$sdS <- apply(CUMULCORREL1N_BID_rets_10sec[,2:dim(CUMULCORREL1N_BID_rets_10sec)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL1N_BID_rets_10sec$contract <- factor("1Deferred")  
CUMULCORREL1N_BID_rets_10sec$lag <- factor("Ten Seconds")  


CUMULCORREL1_BID_rets_no0s$lag <- factor("Contemporaneous")
CUMULCORREL1N_BID_rets_timelag <- rbind(CUMULCORREL1_BID_rets_no0s, CUMULCORREL1N_BID_rets_1sec, CUMULCORREL1N_BID_rets_10sec)

pd <- position_dodge(0.4)
MAXES <- min(CUMULCORREL1N_BID_rets_timelag$MEANS - CUMULCORREL1N_BID_rets_timelag$sdS,1)
MINS <- max(CUMULCORREL1N_BID_rets_timelag$MEANS - CUMULCORREL1N_BID_rets_timelag$sdS,0)
Bid_plot_timelag1N <- ggplot(CUMULCORREL1N_BID_rets_timelag, aes(TimeBins, MEANS, ymin = MEANS-sdS, 
                                                             ymax = MEANS+sdS, colour=lag, group=lag) ) + 
  geom_errorbar(size=1, position=pd) +
  geom_point(size=4, position=pd) + 
  geom_line(size=0.25, position=pd) +
  ggtitle('Nearby and 1 Deferred Correlation, BID') +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45), axis.title.x=element_blank(), 
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major = element_line(colour = "grey")) +
  #scale_colour_grey() + 
  ylab("Correlation") +
  coord_cartesian(ylim = c(-.2, 1.2))+
  scale_y_continuous(minor_breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1.0, 1.1), breaks = c(0, .2, .4, .6, .8, 1., 1.2))
ggsave(file="Bid_plot_timelag1N.png", path='C:/Users/mallorym/Documents/GitHub/BBOBAS', scale=1, height=4, width=8, units="in")

###############################
# OFRs Contemporaneous Plus1
CUMULCORREL1_OFR_rets_1sec$MEANS <- apply(CUMULCORREL1_OFR_rets_1sec[,2:dim(CUMULCORREL1_OFR_rets_1sec)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL1_OFR_rets_1sec$sdS <- apply(CUMULCORREL1_OFR_rets_1sec[,2:dim(CUMULCORREL1_OFR_rets_1sec)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL1_OFR_rets_1sec$contract <- factor("1Deferred")
CUMULCORREL1_OFR_rets_1sec$lag <- factor("One Second")

CUMULCORREL1_OFR_rets_10sec$MEANS <- apply(CUMULCORREL1_OFR_rets_10sec[,2:dim(CUMULCORREL1_OFR_rets_10sec)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL1_OFR_rets_10sec$sdS <- apply(CUMULCORREL1_OFR_rets_10sec[,2:dim(CUMULCORREL1_OFR_rets_10sec)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL1_OFR_rets_10sec$contract <- factor("1Deferred")  
CUMULCORREL1_OFR_rets_10sec$lag <- factor("Ten Seconds")  

# OFRs  Plus2
CUMULCORREL2_OFR_rets_1sec$MEANS <- apply(CUMULCORREL2_OFR_rets_1sec[,2:dim(CUMULCORREL2_OFR_rets_1sec)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL2_OFR_rets_1sec$sdS <- apply(CUMULCORREL2_OFR_rets_1sec[,2:dim(CUMULCORREL2_OFR_rets_1sec)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL2_OFR_rets_1sec$contract <- factor("2Deferred")

# OFRs  Plus3
CUMULCORREL3_OFR_rets_1sec$MEANS <- apply(CUMULCORREL3_OFR_rets_1sec[,2:dim(CUMULCORREL3_OFR_rets_1sec)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL3_OFR_rets_1sec$sdS <- apply(CUMULCORREL3_OFR_rets_1sec[,2:dim(CUMULCORREL3_OFR_rets_1sec)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL3_OFR_rets_1sec$contract <- factor("3Deferred")

CUMULCORREL1_OFR_rets_no0s$lag <- factor("Contemporaneous")
CUMULCORREL_OFR_rets_timelag <- rbind(CUMULCORREL1_OFR_rets_no0s, CUMULCORREL1_OFR_rets_1sec, CUMULCORREL1_OFR_rets_10sec)

pd <- position_dodge(0.4)
MAXES <- min(CUMULCORREL_OFR_rets_timelag$MEANS - CUMULCORREL_OFR_rets_timelag$sdS,1)
MINS <- max(CUMULCORREL_OFR_rets_timelag$MEANS - CUMULCORREL_OFR_rets_timelag$sdS,0)
OFR_plot_timelag <- ggplot(CUMULCORREL_OFR_rets_timelag, aes(TimeBins, MEANS, ymin = MEANS-sdS, 
                                                             ymax = MEANS+sdS, colour=lag, group=lag) ) + 
  geom_errorbar(size=1, position=pd) +
  geom_point(size=4, position=pd) + 
  geom_line(size=0.25, position=pd) +
  ggtitle('Nearby and 1 Deferred Correlations, OFR') +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45), axis.title.x=element_blank(), 
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major = element_line(colour = "grey")) +
  #scale_colour_grey() + 
  ylab("Correlation") +
  coord_cartesian(ylim = c(-.2, 1.2))+
  scale_y_continuous(minor_breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1.0, 1.1), breaks = c(0, .2, .4, .6, .8, 1., 1.2))
ggsave(file="OFR_plot_timelag.png", path='C:/Users/mallorym/Documents/GitHub/BBOBAS', scale=1, height=4, width=8, units="in")
###########################################
###############################
# Plus1 OFRs
CUMULCORREL1N_OFR_rets_1sec$MEANS <- apply(CUMULCORREL1N_OFR_rets_1sec[,2:dim(CUMULCORREL1N_OFR_rets_1sec)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL1N_OFR_rets_1sec$sdS <- apply(CUMULCORREL1N_OFR_rets_1sec[,2:dim(CUMULCORREL1N_OFR_rets_1sec)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL1N_OFR_rets_1sec$contract <- factor("1Deferred")
CUMULCORREL1N_OFR_rets_1sec$lag <- factor("One Second")

CUMULCORREL1N_OFR_rets_10sec$MEANS <- apply(CUMULCORREL1N_OFR_rets_10sec[,2:dim(CUMULCORREL1N_OFR_rets_10sec)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL1N_OFR_rets_10sec$sdS <- apply(CUMULCORREL1N_OFR_rets_10sec[,2:dim(CUMULCORREL1N_OFR_rets_10sec)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL1N_OFR_rets_10sec$contract <- factor("1Deferred")  
CUMULCORREL1N_OFR_rets_10sec$lag <- factor("Ten Seconds")  


CUMULCORREL1_OFR_rets_no0s$lag <- factor("Contemporaneous")
CUMULCORREL1N_OFR_rets_timelag <- rbind(CUMULCORREL1_OFR_rets_no0s, CUMULCORREL1N_OFR_rets_1sec, CUMULCORREL1N_OFR_rets_10sec)

pd <- position_dodge(0.4)
MAXES <- min(CUMULCORREL1N_OFR_rets_timelag$MEANS - CUMULCORREL1N_OFR_rets_timelag$sdS,1)
MINS <- max(CUMULCORREL1N_OFR_rets_timelag$MEANS - CUMULCORREL1N_OFR_rets_timelag$sdS,0)
OFR_plot_timelag1N <- ggplot(CUMULCORREL1N_OFR_rets_timelag, aes(TimeBins, MEANS, ymin = MEANS-sdS, 
                                                                 ymax = MEANS+sdS, colour=lag, group=lag) ) + 
  geom_errorbar(size=1, position=pd) +
  geom_point(size=4, position=pd) + 
  geom_line(size=0.25, position=pd) +
  ggtitle('Nearby and 1 Deferred Correlation, OFR') +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45), axis.title.x=element_blank(), 
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major = element_line(colour = "grey")) +
  #scale_colour_grey() + 
  ylab("Correlation") +
  coord_cartesian(ylim = c(-.2, 1.2))+
  scale_y_continuous(minor_breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1.0, 1.1), breaks = c(0, .2, .4, .6, .8, 1., 1.2))
ggsave(file="OFR_plot_timelag1N.png", path='C:/Users/mallorym/Documents/GitHub/BBOBAS', scale=1, height=4, width=8, units="in")

###########################################
# Plotting summaries - keep zeros
# BIDOFRs Contemporaneous Plus1
CUMULCORREL1_BIDOFR_rets$MEANS <- apply(CUMULCORREL1_BIDOFR_rets[,2:dim(CUMULCORREL1_BIDOFR_rets)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL1_BIDOFR_rets$sdS <- apply(CUMULCORREL1_BIDOFR_rets[,2:dim(CUMULCORREL1_BIDOFR_rets)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL1_BIDOFR_rets$contract <- factor("1Deferred")  

# BIDOFRs Contemporaneous Plus2
CUMULCORREL2_BIDOFR_rets$MEANS <- apply(CUMULCORREL2_BIDOFR_rets[,2:dim(CUMULCORREL2_BIDOFR_rets)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL2_BIDOFR_rets$sdS <- apply(CUMULCORREL2_BIDOFR_rets[,2:dim(CUMULCORREL2_BIDOFR_rets)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL2_BIDOFR_rets$contract <- factor("2Deferred")

# BIDOFRs Contemporaneous Plus3
CUMULCORREL3_BIDOFR_rets$MEANS <- apply(CUMULCORREL3_BIDOFR_rets[,2:dim(CUMULCORREL3_BIDOFR_rets)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL3_BIDOFR_rets$sdS <- apply(CUMULCORREL3_BIDOFR_rets[,2:dim(CUMULCORREL3_BIDOFR_rets)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL3_BIDOFR_rets$contract <- factor("3Deferred")

CUMULCORREL_BIDOFR_rets <- rbind(CUMULCORREL1_BIDOFR_rets, CUMULCORREL2_BIDOFR_rets, CUMULCORREL3_BIDOFR_rets)

pd <- position_dodge(0.4)
MAXES <- min(CUMULCORREL_BIDOFR_rets$MEANS - CUMULCORREL_BIDOFR_rets$sdS,1)
MINS <- max(CUMULCORREL_BIDOFR_rets$MEANS - CUMULCORREL_BIDOFR_rets$sdS,0)
BIDOFR_plot <- ggplot(CUMULCORREL_BIDOFR_rets, aes(TimeBins, MEANS, ymin = MEANS-sdS, 
                                             ymax = MEANS+sdS, colour=contract, group=contract) ) + 
  geom_errorbar(size=1, position=pd) +
  geom_point(size=4, position=pd) + 
  geom_line(size=0.25, position=pd) +
  ggtitle('Contemporaneous Correlation with Nearby in BIDs to OFRs') +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45), axis.title.x=element_blank(), 
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major = element_line(colour = "grey")) +
  #scale_colour_grey() + 
  ylab("Correlation") +
  coord_cartesian(ylim = c(-.2, 1.2))+
  scale_y_continuous(minor_breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1.0, 1.1), breaks = c(0, .2, .4, .6, .8, 1., 1.2))
ggsave(file="BIDOFR_plot.png", path='C:/Users/mallorym/Documents/GitHub/BBOBAS', scale=1, height=4, width=8, units="in")

#########################################################################

#######################################
# Plotting summaries - No zeros
# Bids Contemporaneous Plus1
CUMULCORREL1_BIDOFR_rets_no0s$MEANS <- apply(CUMULCORREL1_BIDOFR_rets_no0s[,2:dim(CUMULCORREL1_BIDOFR_rets_no0s)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL1_BIDOFR_rets_no0s$sdS <- apply(CUMULCORREL1_BIDOFR_rets_no0s[,2:dim(CUMULCORREL1_BIDOFR_rets_no0s)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL1_BIDOFR_rets_no0s$contract <- factor("1Deferred")  

# BIDOFRs Contemporaneous Plus2
CUMULCORREL2_BIDOFR_rets_no0s$MEANS <- apply(CUMULCORREL2_BIDOFR_rets_no0s[,2:dim(CUMULCORREL2_BIDOFR_rets_no0s)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL2_BIDOFR_rets_no0s$sdS <- apply(CUMULCORREL2_BIDOFR_rets_no0s[,2:dim(CUMULCORREL2_BIDOFR_rets_no0s)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL2_BIDOFR_rets_no0s$contract <- factor("2Deferred")

# BIDOFRs Contemporaneous Plus3
CUMULCORREL3_BIDOFR_rets_no0s$MEANS <- apply(CUMULCORREL3_BIDOFR_rets_no0s[,2:dim(CUMULCORREL3_BIDOFR_rets_no0s)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL3_BIDOFR_rets_no0s$sdS <- apply(CUMULCORREL3_BIDOFR_rets_no0s[,2:dim(CUMULCORREL3_BIDOFR_rets_no0s)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL3_BIDOFR_rets_no0s$contract <- factor("3Deferred")

CUMULCORREL_BIDOFR_rets_no0s <- rbind(CUMULCORREL1_BIDOFR_rets_no0s, CUMULCORREL2_BIDOFR_rets_no0s, CUMULCORREL3_BIDOFR_rets_no0s)

pd <- position_dodge(0.4)
MAXES <- min(CUMULCORREL_BIDOFR_rets_no0s$MEANS - CUMULCORREL_BIDOFR_rets_no0s$sdS,1)
MINS <- max(CUMULCORREL_BIDOFR_rets_no0s$MEANS - CUMULCORREL_BIDOFR_rets_no0s$sdS,0)
BIDOFR_plot_no0s <- ggplot(CUMULCORREL_BIDOFR_rets_no0s, aes(TimeBins, MEANS, ymin = MEANS-sdS, 
                                                       ymax = MEANS+sdS, colour=contract, group=contract) ) + 
  geom_errorbar(size=1, position=pd) +
  geom_point(size=4, position=pd) + 
  geom_line(size=0.25, position=pd) +
  ggtitle('Contemporaneous Correlation with Nearby in BID to OFRs - Information-Based') +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45), axis.title.x=element_blank(), 
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major = element_line(colour = "grey")) +
  #scale_colour_grey() + 
  ylab("Correlation") +
  coord_cartesian(ylim = c(-.2, 1.2))+
  scale_y_continuous(minor_breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1.0, 1.1), breaks = c(0, .2, .4, .6, .8, 1., 1.2))
ggsave(file="BIDOFR_plot_no0s.png", path='C:/Users/mallorym/Documents/GitHub/BBOBAS', scale=1, height=4, width=8, units="in")
#########################################################################
#######################################
# Plotting summaries - Time Lags keep zeros
# Bids Plus1
CUMULCORREL1_BIDOFR_rets_1sec$MEANS <- apply(CUMULCORREL1_BIDOFR_rets_1sec[,2:dim(CUMULCORREL1_BIDOFR_rets_1sec)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL1_BIDOFR_rets_1sec$sdS <- apply(CUMULCORREL1_BIDOFR_rets_1sec[,2:dim(CUMULCORREL1_BIDOFR_rets_1sec)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL1_BIDOFR_rets_1sec$contract <- factor("1Deferred")
CUMULCORREL1_BIDOFR_rets_1sec$lag <- factor("One Second")

CUMULCORREL1_BIDOFR_rets_10sec$MEANS <- apply(CUMULCORREL1_BIDOFR_rets_10sec[,2:dim(CUMULCORREL1_BIDOFR_rets_10sec)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL1_BIDOFR_rets_10sec$sdS <- apply(CUMULCORREL1_BIDOFR_rets_10sec[,2:dim(CUMULCORREL1_BIDOFR_rets_10sec)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL1_BIDOFR_rets_10sec$contract <- factor("1Deferred")  
CUMULCORREL1_BIDOFR_rets_10sec$lag <- factor("Ten Seconds")  

# BIDOFRs  Plus2
CUMULCORREL2_BIDOFR_rets_1sec$MEANS <- apply(CUMULCORREL2_BIDOFR_rets_1sec[,2:dim(CUMULCORREL2_BIDOFR_rets_1sec)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL2_BIDOFR_rets_1sec$sdS <- apply(CUMULCORREL2_BIDOFR_rets_1sec[,2:dim(CUMULCORREL2_BIDOFR_rets_1sec)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL2_BIDOFR_rets_1sec$contract <- factor("2Deferred")

# BIDOFRs  Plus3
CUMULCORREL3_BIDOFR_rets_1sec$MEANS <- apply(CUMULCORREL3_BIDOFR_rets_1sec[,2:dim(CUMULCORREL3_BIDOFR_rets_1sec)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL3_BIDOFR_rets_1sec$sdS <- apply(CUMULCORREL3_BIDOFR_rets_1sec[,2:dim(CUMULCORREL3_BIDOFR_rets_1sec)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL3_BIDOFR_rets_1sec$contract <- factor("3Deferred")

CUMULCORREL1_BIDOFR_rets_no0s$lag <- factor("Contemporaneous")
CUMULCORREL_BIDOFR_rets_timelag <- rbind(CUMULCORREL1_BIDOFR_rets_no0s, CUMULCORREL1_BIDOFR_rets_1sec, CUMULCORREL1_BIDOFR_rets_10sec)

pd <- position_dodge(0.4)
MAXES <- min(CUMULCORREL_BIDOFR_rets_timelag$MEANS - CUMULCORREL_BIDOFR_rets_timelag$sdS,1)
MINS <- max(CUMULCORREL_BIDOFR_rets_timelag$MEANS - CUMULCORREL_BIDOFR_rets_timelag$sdS,0)
BIDOFR_plot_timelag <- ggplot(CUMULCORREL_BIDOFR_rets_timelag, aes(TimeBins, MEANS, ymin = MEANS-sdS, 
                                                             ymax = MEANS+sdS, colour=lag, group=lag) ) + 
  geom_errorbar(size=1, position=pd) +
  geom_point(size=4, position=pd) + 
  geom_line(size=0.25, position=pd) +
  ggtitle('Nearby and 1 Deferred Correlations, BID to OFR') +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45), axis.title.x=element_blank(), 
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major = element_line(colour = "grey")) +
  #scale_colour_grey() + 
  ylab("Correlation") +
  coord_cartesian(ylim = c(-.2, 1.2))+
  scale_y_continuous(minor_breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1.0, 1.1), breaks = c(0, .2, .4, .6, .8, 1., 1.2))
ggsave(file="BIDOFR_plot_timelag.png", path='C:/Users/mallorym/Documents/GitHub/BBOBAS', scale=1, height=4, width=8, units="in")

#######################################
# Plotting summaries - Time Lags keep zeros
# OFRBids Plus1
CUMULCORREL1_OFRBID_rets_no0s$MEANS <- apply(CUMULCORREL1_OFRBID_rets_no0s[,2:dim(CUMULCORREL1_OFRBID_rets_no0s)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL1_OFRBID_rets_no0s$sdS <- apply(CUMULCORREL1_OFRBID_rets_no0s[,2:dim(CUMULCORREL1_OFRBID_rets_no0s)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL1_OFRBID_rets_no0s$contract <- factor("1Deferred")

CUMULCORREL1_OFRBID_rets_1sec$MEANS <- apply(CUMULCORREL1_OFRBID_rets_1sec[,2:dim(CUMULCORREL1_OFRBID_rets_1sec)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL1_OFRBID_rets_1sec$sdS <- apply(CUMULCORREL1_OFRBID_rets_1sec[,2:dim(CUMULCORREL1_OFRBID_rets_1sec)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL1_OFRBID_rets_1sec$contract <- factor("1Deferred")
CUMULCORREL1_OFRBID_rets_1sec$lag <- factor("One Second")

CUMULCORREL1_OFRBID_rets_10sec$MEANS <- apply(CUMULCORREL1_OFRBID_rets_10sec[,2:dim(CUMULCORREL1_OFRBID_rets_10sec)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL1_OFRBID_rets_10sec$sdS <- apply(CUMULCORREL1_OFRBID_rets_10sec[,2:dim(CUMULCORREL1_OFRBID_rets_10sec)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL1_OFRBID_rets_10sec$contract <- factor("1Deferred")  
CUMULCORREL1_OFRBID_rets_10sec$lag <- factor("Ten Seconds")  

# OFRBIDs  Plus2
CUMULCORREL2_OFRBID_rets_1sec$MEANS <- apply(CUMULCORREL2_OFRBID_rets_1sec[,2:dim(CUMULCORREL2_OFRBID_rets_1sec)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL2_OFRBID_rets_1sec$sdS <- apply(CUMULCORREL2_OFRBID_rets_1sec[,2:dim(CUMULCORREL2_OFRBID_rets_1sec)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL2_OFRBID_rets_1sec$contract <- factor("2Deferred")

# OFRBIDs  Plus3
CUMULCORREL3_OFRBID_rets_1sec$MEANS <- apply(CUMULCORREL3_OFRBID_rets_1sec[,2:dim(CUMULCORREL3_OFRBID_rets_1sec)[2]], 1, mean, na.rm = TRUE)
CUMULCORREL3_OFRBID_rets_1sec$sdS <- apply(CUMULCORREL3_OFRBID_rets_1sec[,2:dim(CUMULCORREL3_OFRBID_rets_1sec)[2]], 1, sd, na.rm = TRUE) 
CUMULCORREL3_OFRBID_rets_1sec$contract <- factor("3Deferred")

CUMULCORREL1_OFRBID_rets_no0s$lag <- factor("Contemporaneous")
CUMULCORREL1_OFRBID_rets_timelag <- rbind(CUMULCORREL1_OFRBID_rets_no0s, CUMULCORREL1_OFRBID_rets_1sec, CUMULCORREL1_OFRBID_rets_10sec)

pd <- position_dodge(0.4)
MAXES <- min(CUMULCORREL1_OFRBID_rets_timelag$MEANS - CUMULCORREL1_OFRBID_rets_timelag$sdS,1)
MINS <- max(CUMULCORREL1_OFRBID_rets_timelag$MEANS - CUMULCORREL1_OFRBID_rets_timelag$sdS,0)
OFRBID_plot_timelag <- ggplot(CUMULCORREL1_OFRBID_rets_timelag, aes(TimeBins, MEANS, ymin = MEANS-sdS, 
                                                                   ymax = MEANS+sdS, colour=lag, group=lag) ) + 
  geom_errorbar(size=1, position=pd) +
  geom_point(size=4, position=pd) + 
  geom_line(size=0.25, position=pd) +
  ggtitle('Nearby and 1 Deferred Correlations, OFR to BID') +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45), axis.title.x=element_blank(), 
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major = element_line(colour = "grey")) +
  #scale_colour_grey() + 
  ylab("Correlation") +
  coord_cartesian(ylim = c(-.2, 1.2))+
  scale_y_continuous(minor_breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1.0, 1.1), breaks = c(0, .2, .4, .6, .8, 1., 1.2))
ggsave(file="OFRBID_plot_timelag.png", path='C:/Users/mallorym/Documents/GitHub/BBOBAS', scale=1, height=4, width=8, units="in")


###############################
# Bid_plot
# OFR_plot
# Bid_plot_no0s 
# OFR_plot_no0s
# Bid_plot_timelag
# OFR_plot_timelag
#grid.arrange(Bid_plot, OFR_plot, ncol=1)
# 
# grid.arrange(Bid_plot_no0s, OFR_plot_no0s, ncol=1)

nsecs_to_update_BID_rets <- -1*nsecs_to_update_BID_rets
nsecs_to_update_OFR_rets <- -1*nsecs_to_update_OFR_rets
p1secs_to_update_BID_rets <- -1*p1secs_to_update_BID_rets
p1secs_to_update_OFR_rets <- -1*p1secs_to_update_OFR_rets
p2secs_to_update_BID_rets <- -1*p2secs_to_update_BID_rets
p2secs_to_update_OFR_rets <- -1*p2secs_to_update_OFR_rets
p3secs_to_update_BID_rets <- -1*p3secs_to_update_BID_rets
p3secs_to_update_OFR_rets <- -1*p3secs_to_update_OFR_rets

setwd('C:/Users/mallorym/Documents/GitHub/BBOBAS/')
png(filename="nsecs_to_update_BID_rets.png")
hist(as.numeric(nsecs_to_update_BID_rets), 200, main = "Nearby Bid", xlab="Number of Seconds")
dev.off()

png(filename="nsecs_to_update_OFR_rets.png")
hist(as.numeric(nsecs_to_update_OFR_rets), 200, main = "Nearby Offer", xlab="Number of Seconds")
dev.off()

png(filename="p1secs_to_update_BID_rets.png")
hist(as.numeric(p1secs_to_update_BID_rets), 200, main = "1 Deferred Bid", xlab="Number of Seconds")
dev.off()

png(filename="p1secs_to_update_OFR_rets.png")
hist(as.numeric(p1secs_to_update_OFR_rets), 200, main = "1 Deferred Offer", xlab="Number of Seconds")
dev.off()

png(filename="p2secs_to_update_BID_rets.png")
hist(as.numeric(p2secs_to_update_BID_rets), 200, main = "2 Deferred Bid", xlab="Number of Seconds")
dev.off()

png(filename="p2secs_to_update_OFR_rets.png")
p1ofr <- hist(as.numeric(p2secs_to_update_OFR_rets), 200, main = "2 Deferred Offer", xlab="Number of Seconds")
dev.off()

png(filename="p3secs_to_update_BID_rets.png")
hist(as.numeric(p3secs_to_update_BID_rets), 200, main = "3 Deferred Bid", xlab="Number of Seconds")
dev.off()

png(filename="p3secs_to_update_OFR_rets.png")
p1ofr <- hist(as.numeric(p3secs_to_update_OFR_rets), 200, main = "3 Deferred Offer", xlab="Number of Seconds")
dev.off()

##############################################################################
# Check if means are equal in figure 1

p_values              <- matrix(data = NA, nrow = dim(subset(CUMULCORREL_BID_rets, contract == c('1Deferred')))[1], ncol = 3)

for (i in 2:dim(subset(CUMULCORREL_BID_rets, contract == c('1Deferred')))[1]) {
        tresult       <- t.test(as.numeric(subset(CUMULCORREL_BID_rets, TimeBins == c(as.character(CUMULCORREL_BID_rets$TimeBins[i])) & contract == c('1Deferred'))), 
                                as.numeric(subset(CUMULCORREL_BID_rets, TimeBins == c(as.character(CUMULCORREL_BID_rets$TimeBins[i])) & contract == c('2Deferred'))),
                                mu = 0, paired = FALSE, var.equal = FALSE, na.action = na.exclude)
        p_values[i,1] <- tresult$p.value
}

for (i in 2:dim(subset(CUMULCORREL_BID_rets, contract == c('1Deferred')))[1]) {
        tresult       <- t.test(as.numeric(subset(CUMULCORREL_BID_rets, TimeBins == c(as.character(CUMULCORREL_BID_rets$TimeBins[i])) & contract == c('1Deferred'))), 
                                as.numeric(subset(CUMULCORREL_BID_rets, TimeBins == c(as.character(CUMULCORREL_BID_rets$TimeBins[i])) & contract == c('3Deferred'))),
                                mu = 0, paired = FALSE, var.equal = FALSE, na.action = na.exclude)
        p_values[i,2] <- tresult$p.value
  
  
}

for (i in 2:dim(subset(CUMULCORREL_BID_rets, contract == c('1Deferred')))[1]) {
        tresult       <- t.test(as.numeric(subset(CUMULCORREL_BID_rets, TimeBins == c(as.character(CUMULCORREL_BID_rets$TimeBins[i])) & contract == c('2Deferred'))), 
                                as.numeric(subset(CUMULCORREL_BID_rets, TimeBins == c(as.character(CUMULCORREL_BID_rets$TimeBins[i])) & contract == c('3Deferred'))),
                                mu = 0, paired = FALSE, var.equal = FALSE, na.action = na.exclude)
        p_values[i,3] <- tresult$p.value
}

p_values              <- round(p_values, digits = 2)
row.names(p_values)   <- subset(CUMULCORREL_BID_rets, contract == c('1Deferred'))$TimeBins
colnames(p_values)    <- c('Near & 1 - Near & 2', 'Near & 1 - Near & 3','Near & 2 - Near & 3' )
p_values              <- p_values[-1,]
save(p_values, file = 'p_values.Rda')   
rm(p_values)
load('p_values.Rda')

########################################################################################
# Look at historgrams of correlations
CUMULCORREL_BID_rets

for(i in 2:24){

  hist(as.numeric(subset(subset(CUMULCORREL_BID_rets, TimeBins == c(as.character(CUMULCORREL_BID_rets$TimeBins[i])) 
                                    & contract == c('1Deferred')), select = -c(TimeBins, MEANS, sdS, contract))), 
                                    main = paste(as.character(CUMULCORREL_BID_rets$TimeBins[i]), ', Correlations of Nearby and 1 Deferred'), xlab = "Correlation")
dev.copy(png, paste('1Deferred', as.character(i), '.png'))
dev.off()
}

