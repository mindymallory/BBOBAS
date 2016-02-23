# Summary tables 
#This script creates the summary tables for the BBOBAS paper. I'm doing it in a separate script because I recently learned about data.table
#which will make thie process a lot less painful. I think it will be easier than slogging along with the Analysis.R code. 
library(timeDate)
library(data.table)
library(devtools)
library(readr)
#install_github("<ProfMalloryResearch>/<BBOToolkit>", auth_token = Sys.getenv("GITHUB_PAT"))   # My repository under development
library(BBOTools)
library(magrittr)
library(gridExtra)
library(tidyr)
library(ggplot2)

# 'C:/Users/mallorym/BBOCORNDATA/' # Path to data files on my local computer.

################################################################
# Defines the dates of this paper's sample, and removes the required dates.
yearstart <- 2008
yearend <- 2009
#yearend <-  2008
dates <- timeSequence(from = paste(yearstart, "-01-14", sep = ""), 
                      to = paste(yearend, "-12-30", sep = ""))
                      #to = paste(yearend, "-02-05", sep = ""))

# Easier to define two dates indices than to deal with the issue of the missing leading zero in the 2008 and 2009 
# representation of dates. 
yearstart1 <- 2010
yearend1 <- 2011
#yearend1 <-  2010
dates1 <- timeSequence(from = paste(yearstart1, "-01-04", sep = ""), 
                      to = paste(yearend1, "-12-30", sep = ""))
                      #to = paste(yearend1, "-01-20", sep = ""))


# Code below requires dates to be integers, here we change the format
dates <- dates[isBizday(dates,holidayNYSE(yearstart:yearend))]
dates <- as.numeric(format(dates, format = "%y%m%d"))


dates1 <- dates1[isBizday(dates1,holidayNYSE(yearstart1:yearend1))]
dates1 <- as.numeric(format(dates1, format = "%y%m%d"))
dates1 <- subset(dates1, dates1 != 100405)
# Skipped April 5, 2010. There was some kind of quote spoofing algorithm generating a lot of quotes, posting
# and canceling offers at the best offer. Also it appears that trading was halted. Really I skipped it because the
# file was 12 times larger than the typical size and it was taking too long to process. Would make an interesting case
# study to go back and investigate.

# Delete Limit days when there were no quote revisions. Couldn't make all delitions in one line; not sure why not.
dates1 <- subset(dates1, dates1 != c('100112'))# Revision to Crop Production report

dates1 <- subset(dates1, dates1 != c('101008'))
dates1 <- subset(dates1, dates1 != c('111209'))
dates1 <- subset(dates1, dates1 != c('110331')) # Prospective Plantings report (Not included in our report days)
dates1 <- subset(dates1, dates1 != c('110630')) # Planted Acres report 
dates1 <- subset(dates1, dates1 != c('110705')) # Light trade after the 4th holiday. No trades or quotes for '3 deferred' which
# would have been the March 2012 contract.
dates <- subset(dates, dates != c('80707')) # After 4th holiday, Informa came out with larger than WASDE forecast yeild. \
#dates <- subset(dates, dates != c('90102')) 
dates <- subset(dates, dates != c('90103'))
dates <- subset(dates, dates != c('90104'))
#dates <- subset(dates, dates != c('90105'))
#dates <- subset(dates, dates != c('90106'))
#dates <- subset(dates, dates != c('90107'))
#dates <- subset(dates, dates != c('90108'))
#dates <- subset(dates, dates != c('90109'))
dates <- subset(dates, dates != c('90110'))
dates <- subset(dates, dates != c('90111'))
#dates <- subset(dates, dates != c('90112'))
#dates <- subset(dates, dates != c('90113')) # No clue why but these dates are missing from the dataset. 


################################################################
# Loop loads the daily datafiles, does initial processing of prices, and then stores in a list, accum
ptm <- proc.time()
accum <- as.list(NULL)
for(i in 1:length(dates)) {

 DATASET <- as.data.table(bboread(paste0('C:/Users/mallorym/BBOCORNDATA/', 
                                         '2008Jan-2011Dec_txt',"/",
                                         "XCBT_C_FUT_","0", dates[i], ".txt")))
 
 DATASET[, Price := decimalprices(TrPrice)]                          # Convert Price to decimal
 accum[[i]] <- DATASET[, .(Price = mean(Price), .N),                 # Ave daily price, number of ask/bids
                        by = .(TradeDate, DeliveryDate, ASKBID)]
  # accum[[i]] <- DATASET                                             # If you want to keep all the trades and quotes and not
                                                                     # just get average and counts per day
 }
len <- length(accum)
for(i in 1:length(dates1)) {
  DATASET <- as.data.table(bboread(paste0('C:/Users/mallorym/BBOCORNDATA/',
                                          '2008Jan-2011Dec_txt',"/",
                                          "XCBT_C_FUT_", dates1[i], ".txt")))
  DATASET[, Price := decimalprices(TrPrice)]                         # Convert Price to decimal
  accum[[len + i]] <- DATASET[, .(Price = mean(Price), .N),          # Ave daily price, number of ask/bids
                      by = .(TradeDate, DeliveryDate, ASKBID)]
  # accum[[i]] <- DATASET                                             # If you want to keep all the trades and quotes and not
                                                                      # just get average and counts per day
}
proc.time() - ptm
################################################################
# Remove September Contracts and Roll Continuous Series
# This section has to be done with lapply() calls because I couldn't figure out how to 
# take the first 9 rows after binding the list together in one data table

RollDate <- 20                                                         # Day in the Month prior to Delivery when we roll contracts

accum <- RollContracts(accum, r = RollDate)

################################################################
# Tidy up the data.table in preparation for the ggplot2

DT   <- data.table::rbindlist(accum)                                 # Binds elements of the list into one data table 
DT[, TradeDate := datemanip(TradeDate)]                  # Convert date to proper date format


                                                                     
#DT   <- dcast.data.table(DT, TradeDate + DeliveryDate ~ ASKBID,      # Cast DT and give useful names for ggplot2 groups
DT   <- dcast.data.table(DT, TradeDate + Deferreds ~ ASKBID,      # Cast DT and give useful names for ggplot2 groups
                         value.var = c("N", "Price")) %>%
        setnames(c("N_A", "N_B", "N_NA", "Price_A", "Price_B",       # Replace with more useful names
                   "Price_NA"), c("NumberofAsks", "NumberofBids",
                   "NumberofTransactions", "PriceAsk", "PriceBid",
                   "PriceTransaction") ) 



DT[, c("NumberofAsks", "NumberofBids") := .(round(NumberofAsks/2 - NumberofTransactions/2),          # This is an estimate because BBO data duplicates the 
                            round(NumberofBids/2 - NumberofTransactions/2))]         # ask(bid) that was not revised on the same trseqnum prior to 2012

DT   <- data.table::melt(DT, id.vars = c("TradeDate",                # Final melt before ggplot2 
                                         "Deferreds"))
                                         #                     "DeliveryDate", "Deferreds"))

save(DT, file = 'SummaryDT.Rda')

numasksplot <- ggplot(DT[variable %in% c("NumberofAsks")], aes(TradeDate, value, colour=Deferreds, group=Deferreds)) +
  geom_point() +
  #geom_smooth() +
  scale_size_area() +
  ylab("Number of Ask Quotes") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45), axis.title.x=element_blank(), 
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major = element_line(colour = "grey")) 


numbidsplot <- ggplot(DT[variable %in% c("NumberofBids")], aes(TradeDate, value, colour=Deferreds, group=Deferreds)) +
  geom_point() +
  #geom_smooth() +
  scale_size_area() +
  ylab("Number of Bid Quotes") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45), axis.title.x=element_blank(), 
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major = element_line(colour = "grey")) 

numtransplot <- ggplot(DT[variable %in% c("NumberofTransactions")], aes(TradeDate, value, colour=Deferreds, group=Deferreds)) +
  geom_point() +
  #geom_smooth() +
  scale_size_area() +
  ylab("Number of Transactions") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45), axis.title.x=element_blank(), 
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major = element_line(colour = "grey")) 

dailyaveprice <- ggplot(DT[variable %in% c("PriceTransaction")], aes(TradeDate, value, colour=Deferreds, group=Deferreds)) +
  geom_point() +
  #geom_smooth() +
  scale_size_area() +
  ylab("Price (cents/bu") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45), axis.title.x=element_blank(), 
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major = element_line(colour = "grey")) 

grid.arrange(dailyaveprice, numasksplot, numbidsplot, numtransplot, ncol=1)


