#This code preprocesses the CMEGroup BBO data. It transforms the raw data so that
#every timestamp has a bid, bid size, offer, offersize, and spread. Raw data puts 
#ask and bid at same moment on different lines.
#BBO  contains electronic trades only and no spreads.

library(LaF)
library(timeDate)
library(xts)
library(reshape)
library(dplyr)
library(xtable)

#setwd('C:/Users/mallorym/Dropbox/Market Microstructure Soybean Futures/BBO_sample') #Dropbox
setwd('C:/Users/mallorym/BBOCORNDATA/2010Feb-2011Dec_txt') #Office PC
ptm <- proc.time()
#Set period to aggregate to, comment out to allow origianal (and often multiple)
#quotes per second. Accepts "seconds", "minutes", etc

ConvertCornFuturesQuotes <- function(x){
  #x is a vector or variable that can be coerced to char type
  # it should have 4 characters: hundreds, tens, ones, and 8th of a cent
  # since futures quotes are in cents per bushel and the ticks are 8ths of a cent
  # this function will only work for corn prices < $10/bushel since I did not define
  # a thousandths place.
  
  x <- strsplit(as.character(x), "")
  h <- sapply(x, "[[", 1) #Selects the hudreds place
  t <- sapply(x, "[[", 2) #Selects the tens place
  o <- sapply(x, "[[", 3) #Selects the ones place
  e <- sapply(x, "[[", 4) #Selects the 8th of a cent place
  
  #Convert 8th of a cent to decimal
  e <- as.numeric(e)/8
  price <- as.numeric(paste0(h,t,o))
  price <- price + e
  return(price)
}
PERIOD = "seconds"
# dates <- c("110110", "100126") #Dropbox
# dates <- "110110"
# dates <- "100126"
# dates <- "100112"
#January 2010
dates1 <-c(c(100104:100108), c(100111:100115), c(100119:100122),c(100125:100129)) 
# #February 2010
 dates2 <-c(c(100201:100205), c(100208:100212), c(100216:100219),c(100222:100226))
# #March 2010
 dates3 <- c(c(100301:100305), c(100308:100312), c(100315:100319),c(100322:100326),c(100329:100331))
# #January 2011
 dates4 <-c(c(110103:110107), c(110110:110114), c(110118:110121),c(110124:110128), c(110131))
dates <- c(dates1,dates2,dates3,dates4)
i=1
for(i in 1:length(dates)){


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
                                        "categorical", "categorical", "categorical", "integer"), 
  )
  
  begin(data)
 # goto(data, 200014)
  
  NROWS = 50000 #manage memory by setting manageable block sizes
  DATA <- next_block(data, nrows = NROWS, columns= c(1,2,3,7,8,11,13))
  CUMULDATA <- as.data.frame(t(as.numeric(matrix(0,1,10))))
  CUMULTRANS <- as.data.frame(t(as.numeric(matrix(0,6))))
  CUMULBADPRICES <- as.data.frame(t(as.numeric(matrix(0,7))))
  
  while(dim(DATA)[1]>0){
    BadPrices <- subset(DATA, DATA$TrPrice < 1000)
    DATA <- subset(DATA, DATA$TrPrice > 1000)
    DATA$TrPrice <- ConvertCornFuturesQuotes(DATA$TrPrice)
    
    #Build the independent Ask and Bid objects
    ask <- subset(DATA, ASKBID == "A", select=-c(ASKBID))
    ask <- rename(ask, QOfferedAtAsk=TrQuantity, AskPrice=TrPrice) #dplyr #Seems like sometimes rename 
    #ask <- rename(ask, c(TrQuantity="QOfferedAtAsk", TrPrice="AskPrice")) #reshape #
    
    bid <- subset(DATA, ASKBID == "B", select=-c(ASKBID))
    bid <- rename(bid, QOfferedAtBid=TrQuantity, BidPrice=TrPrice) #dplyr
    #bid <- rename(bid, c(TrQuantity="QOfferedAtBid", TrPrice="BidPrice")) #reshape #
    
    TRANSACTIONS <- subset(DATA, ASKBID != "A" & ASKBID != "B" , select=-c(ASKBID)) #
    #TRANSACTIONS <- rename(TRANSACTIONS, TrQuantity=TransQuantity) #dplyr no rename needed
    #TRANSACTIONS <- rename(TRANSACTIONS, c(TrQuantity="TrQuantity")) #reshape #
    
    #Join ask and bid to replace the original DATA dataframe and get rid of ask, bid dataframes
    DATA <- merge(ask, bid, by = cbind(names(DATA[,1:4])), join = "outer") 
    
    ##this merge of askbids and transactions is not correct. See duplicated trseq#'s
    #DATAM <- merge(DATA, transactions, by = cbind(names(DATA[,1:2]),"DeliveryDate"),  join = "outer") 
    
    #Memory Management
    rm(ask) 
    rm(bid)
    
    #Add BAS and BAS Midpoint
    DATA$BidAskMid <- (DATA$AskPrice + DATA$BidPrice)/2
    DATA$BASpread <- DATA$AskPrice - DATA$BidPrice
    
    names(CUMULDATA) <- names(DATA)
    CUMULDATA <- rbind(CUMULDATA,DATA)
    
    names(CUMULTRANS) <- names(TRANSACTIONS)
    CUMULTRANS <- rbind(CUMULTRANS,TRANSACTIONS)
    
    names(CUMULBADPRICES) <- names(BadPrices)
    CUMULBADPRICES <- rbind(CUMULBADPRICES,BadPrices)
    
    DATA <- next_block(data, nrows = NROWS, columns= c(1,2,3,7,8,11,13))
  }
  
  
  
  
#   temp <- subset(CUMULDATA, DeliveryDate == 1003)
#   temp <- subset(CUMULTRANS, DeliveryDate == 1003)
#   sum(temp$TrQuantity)
  
  Contracts <- group_by(CUMULTRANS, DeliveryDate)
  BAS <- group_by(CUMULDATA, DeliveryDate)
  
  SummaryTable <- merge(summarize(Contracts, Volume = sum(TrQuantity)), 
                        summarize(BAS, BAS = mean(BASpread), MidPrice = mean(BidAskMid)), 
                        by = 'DeliveryDate', join = "outer")
  SummaryTable[1,2] <- dates[i] 
  if(i!=1) {
    SummaryTableCum <- rbind(SummaryTableCum,SummaryTable)
    } else 
      SummaryTableCum <- SummaryTable #merge(SummaryTableCum, SummaryTable, by = 'DeliveryDate', join = "outer")

}
proc.time() - ptm

write.csv(SummaryTableCum, "SummaryJan2010.csv")
write.csv(CUMULDATA, 'CUMULDATA.csv')
write.csv(CUMULTRANS, 'CUMULTRANS.csv')

#Here is another edit
#trying for rsh authentication

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

