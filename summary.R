# Summary tables 
#This script creates the summary tables for the BBOBAS paper. I'm doing it in a separate script because I recently learned about data.table
#which will make thie process a lot less painful. I think it will be easier than slogging along with the Analysis.R code. 
# library(timeDate)
# library(data.table)
# library(devtools)
# library(readr)
# install_github("<ProfMalloryResearch>/<BBOToolkit>", auth_token = Sys.getenv("GITHUB_PAT"))   # My repository under development
# library(BBOTools)
#library(magrittr)

# 'C:/Users/mallorym/BBOCORNDATA/' # Path to data files on my local computer.

# Defines the dates of this paper's sample, and removes the required dates.
yearstart <- 2008
yearend <- 2008
#yearend <-  2008
dates <- timeSequence(from = paste(yearstart, "-01-14", sep = ""), 
                      #to = paste(yearend, "-11-04", sep = ""))
                      to = paste(yearend, "-01-16", sep = ""))



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

DATASET <- data.table(t(c(1:8)))
colnames(DATASET) <- c("TradeDate" ,"TradeTime", "TradeSeqNum", "DeliveryDate", "TrQuantity", "TrPrice", "ASKBID", "EntryDate")

start <- c(1, 9, 15, 28, 32, 45, 53, 65)
end   <- c(8, 14, 22, 31, 36, 51, 53, 70)
for(i in 1:length(dates)) {
#   temp <- as.data.table(bboread(paste0('C:/Users/mallorym/BBOCORNDATA/', '2008Jan-2010Jan_txt',
#                                        "/","XCBT_C_FUT_","0", dates[i], ".txt")))
#   DATASET <- rbind(DATASET,temp)
  
  temp <- as.data.table(bboread(paste0('C:/Users/mallorym/BBOCORNDATA/', '2008Jan-2010Jan_txt',
                                       "/","XCBT_C_FUT_","0", dates[i], ".txt")))
  
  DATASET <- rbind(DATASET,temp)
}


# Notes to try

DATASET[, .N, by=.(TradeDate, ASKBID)]  # Close, could write one day at a time. 


DATASET[.(TradeTime > 92959 && TradeTime < 131500, TradeTime] 

# Trying to work with time as a facator 
Times <- as.ITime(timeSequence(from = "2010-01-04 00:00:00", to = "2010-01-04 23:59:59", by = '1 sec'))
hour(Times)


# This works!!!!  # Might be faster to manipulate the string to 2001-02-03 format and convert directly to 
# posixlt format so that we only do one conversion and not two
datatimes <- timeDate(paste0(as.character(DATASET$TradeDate), as.character(DATASET$TradeTime), format = "%Y%m%d%H%M%S"))
datetimes <- 
  paste0(as.character(DATASET$TradeDate), as.character(DATASET$TradeTime)) %>%
  timeDate(format = "%Y%m%d%H%M%S") %>%
  as.data.table()

tttttt<- as.POSIXlt(datetimes)
tttttt$min    # Can use this to build ten minute bin factors
bins <- round(tttttt$min/10)*10
# Now subset for 9:13 hour and paste tttttt$hour and bins together.

makebins <- function(x) {    # contribute to the IDateTime package rather than make this wrapper. 
          datetimes <- paste0(as.character(x$TradeDate), as.character(x$TradeTime)) %>%
          timeDate(format = "%Y%m%d%H%M%S") %>%
          as.POSIXlt()
  
  bins <- round(datetimes$min/10)*10 
  
  bins <- paste0(datetimes$hour, ":", bins)    # Use data.table features to subset hour[ i = 9 through 13 ] quickly. 
  
    return(bins)
  
}

# Write function to manipulate character string 'TradeDate' to an unambiguous time format. Need a function so 
# that operation can be vectorized on the large dataset.


dates <- 
  paste0(as.character(DATASET$TradeDate), as.character(DATASET$TradeTime)) %>%
  timeDate(format = "%Y%m%d%H%M%S") %>%    
  as.IDate()


# Manipulate Date string into Unambiguous format. 


DATA[, datemanip(DATA[, 1:2, with=FALSE])]  # Just edit to do assignment


