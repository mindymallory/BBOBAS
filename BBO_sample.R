# This code preprocesses the CMEGroup BBO data. It transforms the raw data so that
# every timestamp has a bid, bid size, offer, offersize, and spread. Raw data puts 
# ask and bid at same moment on different lines. Then xts objects are created for 
# trades and quotes and written to file for futher analysis later
# Copyright [2015] [Mindy L. Mallory]

library(LaF)
library(timeDate)
library(xts)
library(reshape)
library(dplyr)
library(xtable)
library(highfrequency)
j=1 # This is a lame hack to fix the fact that I didn't rewrite the naming code for the csv files. 

#setwd('C:/Users/mallorym/Dropbox/Market Microstructure Soybean Futures/BBO_sample') #Dropbox
setwd('C:/Users/mallorym/BBOCORNDATA/2010Feb-2011Dec_txt') #Office PC
ptm <- proc.time()



ConvertCornFuturesQuotes <- function(x) {
  # x is a vector or variable that can be coerced to char type
  # it should have 4 characters: hundreds, tens, ones, and 8th of a cent
  # since futures quotes are in cents per bushel and the ticks are 8ths of a cent
  # this function will only work for corn prices < $10/bushel since I did not define
  # a thousandths place.
  
  x     <- strsplit(as.character(x), "")
  h     <- sapply(x, "[[", 1) #Selects the hudreds place
  t     <- sapply(x, "[[", 2) #Selects the tens place
  o     <- sapply(x, "[[", 3) #Selects the ones place
  e     <- sapply(x, "[[", 4) #Selects the 8th of a cent place
  
  #Convert 8th of a cent to decimal
  e     <- as.numeric(e)/8
  price <- as.numeric(paste0(h,t,o))
  price <- price + e
  return(price)
}


#********************************************************************************************************
#********************************************************************************************************
# Define the dates to loop over
yearstart <- 2010
yearend   <- 2010
dates     <- timeSequence(from = paste(yearstart, "-01-29", sep = ""), # Had a bug in the BADPRICES if statement. Restart the loop
                          to = paste(yearend, "-01-29", sep = ""))
# Skipped April 5, 2010. There was some kind of quote spoofing algorithm generating a lot of quotes, posting
# and canceling offers at the best offer. Also it appears that trading was halted. Really I skipped it because the 
# file was 12 times larger than the typical size and it was taking too long to process. Would make an interesting case
# study to go back and investigate. 

# Stopped at 11-01-2011 for now because they changed the format. EX is different for bid ask, whereas before bid and ask 
# got the same EX. Beginning in 2012 the format changes yet again where they are giving large files and only reporting 
# which quote changed. Might not be worth processing nov and dec 2011 for this paper. 


# Code below requires dates to be integers, here we change the format
dates     <- dates[isBizday(dates,holidayNYSE(yearstart:yearend))]
dates     <- as.numeric(format(dates, format = "%y%m%d"))

      # These dates were useful for testing
      # dates <- c("110110", "100126") #Dropbox
      # dates <- "110110"
      # dates <- "100126"
      # dates <- "100112"
      # #January 2010
      # dates1 <-c(c(100104:100108), c(100111:100115), c(100119:100122),c(100125:100129)) 
      # # #February 2010
      #  dates2 <-c(c(100201:100205), c(100208:100212), c(100216:100219),c(100222:100226))
      # # #March 2010
      #  dates3 <- c(c(100301:100305), c(100308:100312), c(100315:100319),c(100322:100326),c(100329:100331))
      # # #January 2011
      #  dates4 <-c(c(110103:110107), c(110110:110114), c(110118:110121),c(110124:110128), c(110131))
      # dates <- c(dates1,dates2,dates3)
#********************************************************************************************************
#********************************************************************************************************

for(i in 1:length(dates)) {

  data <- laf_open_fwf(filename = paste0("XCBT_C_FUT_",dates[i],".txt"),                              
                       column_widths = c(8,6,8,1,3,1,4,5,7,
                                         1,7,1,1,1,1,1,1,
                                         2,1,1,1,1,1,6 ), 
                       column_names = c("TradeDate", "TradeTime", "TradeSeq#", "SessionInd", 
                                        "TickerSym", "FOIInd", "DeliveryDate", "TrQuantity",
                                        "Strike Price", "StrikePrDecLoc", "TrPrice", "TRPRDecLoc", "ASKBID", 
                                        "IndicativeQuote", "MarketQuote", "CloseOpen", 
                                        "OpenException", "PostClose", "CancelCode", "InsertedPrice",
                                        "FastLast", "Cabinet", "Book", "EntryDate"), 
                       
                       column_types = c("integer", "character", "integer", "string", 
                                        "string", "string", "integer", "integer",
                                        "integer", "integer", "integer", "integer", "string",
                                        "categorical", "categorical", "categorical",
                                        "categorical", "categorical", "categorical", "categorical",
                                        "categorical", "categorical", "categorical", "integer"), )
  
  
  begin(data)
  NROWS          <- 10000  # Manage memory by setting manageable block sizes
  DATA           <- next_block(data, nrows = NROWS, columns= c(1,2,3,7,8,11,13))
  CUMULDATA      <- as.data.frame(t(as.numeric(matrix(0,1,8))))
  CUMULTRANS     <- as.data.frame(t(as.numeric(matrix(0,6))))
  CUMULBADPRICES <- as.data.frame(t(as.numeric(matrix(0,7))))
  
while(dim(DATA)[1]>0) {
  DATA           <- rename(DATA, EX=TradeSeq., SYMBOL=DeliveryDate)
  BadPrices      <- subset(DATA, DATA$TrPrice <= 1000)
  DATA           <- subset(DATA, DATA$TrPrice > 1000)
  DATA$TrPrice   <- ConvertCornFuturesQuotes(DATA$TrPrice)
    

  #Build the independent Ask and Bid objects
    ask          <- subset(DATA, ASKBID == "A", select=-c(ASKBID))
    ask          <- rename(ask, OFRSIZ=TrQuantity, OFR=TrPrice) #dplyr #Seems like sometimes rename 
    #ask          <- rename(ask, c(TrQuantity="QOfferedAtAsk", TrPrice="AskPrice")) #reshape #
    
    bid          <- subset(DATA, ASKBID == "B", select=-c(ASKBID))
    bid          <- rename(bid, BIDSIZ=TrQuantity, BID=TrPrice) #dplyr
    #bid          <- rename(bid, c(TrQuantity="QOfferedAtBid", TrPrice="BidPrice")) #reshape #

    
    TRANSACTIONS <- subset(DATA, ASKBID != "A" & ASKBID != "B" , select=-c(ASKBID)) #
    TRANSACTIONS <- rename(TRANSACTIONS, SIZE= TrQuantity, PRICE=TrPrice) #dplyr no rename needed
    #TRANSACTIONS <- rename(TRANSACTIONS, TrQuantity=TransQuantity) #dplyr no rename needed
    #TRANSACTIONS <- rename(TRANSACTIONS, c(TrQuantity="TrQuantity")) #reshape #
    
    #Join ask and bid to replace the original DATA dataframe and get rid of ask, bid dataframes
    DATA         <- merge(ask, bid, by = cbind(names(DATA[,1:4])), join = "outer") 
    
    #Memory Management
    rm(ask) 
    rm(bid)
    
    names(CUMULDATA)      <- names(DATA)
    CUMULDATA             <- rbind(CUMULDATA,DATA)
    
    names(CUMULTRANS)     <- names(TRANSACTIONS)
    CUMULTRANS            <- rbind(CUMULTRANS,TRANSACTIONS)
    
    names(CUMULBADPRICES) <- names(BadPrices)
    CUMULBADPRICES        <- rbind(CUMULBADPRICES,BadPrices)
    
    DATA                  <- next_block(data, nrows = NROWS, columns= c(1,2,3,7,8,11,13))
  }


#Clean up the top row of the saved data
CUMULDATA      <- CUMULDATA[2:dim(CUMULDATA)[1],]
CUMULTRANS     <- CUMULTRANS[2:dim(CUMULTRANS)[1],]
CUMULBADPRICES <- CUMULBADPRICES[2:dim(CUMULBADPRICES)[1],]

j=1 # This is a lame hack to fix the fact that I didn't rewrite the naming code for the csv files.
# Separate out the contracts here
DeliveryDates  <- unique(CUMULDATA$SYMBOL)
DeliveryDates  <- DeliveryDates[order(DeliveryDates)]
write.csv(CUMULDATA, file = paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[j])))
write.csv(CUMULTRANS, file = paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[j])))
if(is.na(CUMULBADPRICES)[1] == FALSE) {write.csv(CUMULBADPRICES, file = paste0('BADPRICES',as.character(dates[i])))}

# Creates an XTS object for every contract's quotes and trades
# Naming convention is 't_date_contract'. for example t_20100126_1003 is the trades on 01-26-2010 for the March10 contract

for (j in 1:length(DeliveryDates)) {
  #The Quotes 
  qtemp      <- subset(CUMULDATA, SYMBOL == DeliveryDates[j])
  times      <- timeDate(paste0(qtemp$TradeDate,qtemp$TradeTime), format = "%Y%m%d%H%M%S")
  temp       <- as.xts(subset(qtemp, select = -c(TradeDate, TradeTime)), order.by = times)
  save(temp, file = paste0('q', '_', as.character(dates[i]), "_", as.character(DeliveryDates[j]), ".rda"))
  
  #The Trades
  ttemp      <- subset(CUMULTRANS, SYMBOL == DeliveryDates[j])
  times      <- timeDate(paste0(ttemp$TradeDate,ttemp$TradeTime), format = "%Y%m%d%H%M%S")
  temp       <- as.xts(subset(ttemp, select = -c(TradeDate, TradeTime)), order.by = times)
  save(temp, file = paste0('t', '_', as.character(dates[i]), "_", as.character(DeliveryDates[j]), ".rda"))
  
  # Makes a record of any bad prices identified
  if(is.na(CUMULBADPRICES)[1] == FALSE) {save(CUMULBADPRICES, file = paste0('BADPRICES',as.character(dates[i]), ".rda"))
  }
  }
save(DeliveryDates, file = paste0('Contracts', as.character(dates[i]),".rda"))
write.csv(DeliveryDates, file = paste0('Contracts', as.character(dates[i])))
}
proc.time() - ptm


  
  rm(CUMULDATA)
  rm(CUMULTRANS)
  rm(CUMULBADPRICES)
  rm(qtemp)
  rm(ttemp)

# Testing for functionality with 'highfrequency' package functions
  # Write the raw xts to file, but this testing ensures that the 
  # highfrequency functions will work
  
  # First, have to use the 'merge' function because some of the highfreqency 
  # functions crash if you try to pass an xts object with multiple
  # rows with the same timestamp
  q_20100129_1003 <- mergeQuotesSameTimestamp(q_20100129_1003)
  t_20100129_1003 <- mergeTradesSameTimestamp(t_20100129_1003)

  # Aggregation over time
  agg_q <- aggregateQuotes(q_20100129_1003, on='minutes', k=5)
  agg_t <- aggregateTrades(t_20100129_1003, on='minutes', k=5)

  # Matching trades and quotes
  mtq   <- matchTradesQuotes(t_20100129_1003,q_20100129_1003)

  # Get trade direction (useful for caculating PIN, e.g.,)
  gtd   <- getTradeDirection(mtq)

  # Some liquidity measures
  qs    <- tqLiquidity(mtq,t_20100126_1003,q_20100126_1003, type = "qs")
  pi    <- tqLiquidity(mtq,t_20100126_1003,q_20100126_1003, type = "price_impact")

  
# Testing basic xts functionality
  plot(t_20100126_1003$PRICE)
  plot(t_20100126_1003$PRICE["20100126 09:29:30/20100126 13:15:30"])

# Save xts object and load it back into the workspace
  save(mtq, file = "mtq.rda")
  rm(mtq)
  load('mtq.rda')



write.csv(SummaryTableCum, "SummaryJan2010.csv")
write.csv(CUMULDATA, 'CUMULDATA.csv')
write.csv(CUMULTRANS, 'CUMULTRANS.csv')





# #Define as xts object (time series package)
# times <- timeDate(paste0(CUMULDATA$TradeDate,CUMULDATA$TradeTime), format = "%Y%m%d%H%M%S")
# CUMULDATA <- as.xts(subset(CUMULDATA, select = -c(TradeDate, TradeTime)), order.by = times)
#timest <- timeDate(paste0(CUMULTRANS$TradeDate,CUMULTRANS$TradeTime), format = "%Y%m%d%H%M%S")
#CUMULTRANS <- as.xts(subset(CUMULTRANS, select = -c(TradeDate, TradeTime)), order.by = timest)
# 
# head(temp)
# temp <- to.period(temp$TrPrice, period = "minutes", k = 1, OHLC=TRUE)
# plot(temp["2011-01-10 09:29:00/2011-01-10 13:17:00"], main = "Outright Transaction Prices")
# #BASpread["2011-01-10 09:25:00/2011-01-10 10:35:00"]


