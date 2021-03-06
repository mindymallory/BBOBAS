
Nearby and Deferred Quotes: What They Tell us About Linkages and Adjustments to Information
========================================================
author: Mindy Mallory, Phil Garcia, and Teresa Serra
date: April 20, 2015
transition: none
University of Illinois  
   
NCCC 134 Applied Commodity Price Analysis, Forecasting, and Market Risk Management



# ```{r, echo=FALSE, cache=TRUE}
# setwd('C:/Users/mallorym/Documents/GitHub/BBOBAS')
# source('C:/Users/mallorym/Documents/GitHub/BBOBAS/Analysis.R')
# ```

========================================================


Overview
========================================================
incremental: true

Compute correlation in percent changes of the best bid (offer) of the nearby and deferred contracts

Data
- Corn Best Bid and Offer (BBO) data set from CME Group 
- Records every change to the top of the order book (Bid, Bid size, Offer, Offer Size) & transactions (On Globex)
- Create the 'top of the book' from the BBO data (time-stamped to the second)
- Aggregate to the second. 
- 01/04/2010 to 11/04/2011 (We have 2008-2009 and 2012-2013 to process and incorporate)
 + Excluded most days with a limit price move. 

Objectives
========================================================
incremental: true
Computing correlation between percent changes of nearby and deferred Bids (Offers) we gain an understanding how quickly and efficiently information is incorporated along the forward curve
- Suggestive of the role liquidity trades verses information based trades play on the forward curve
- Determine whether these patterns are different on USDA report days (arrivals of important information).
 + Crop Production, WASDE, Grain Stocks

Objectives
========================================================
- Contemporaneous Correlations
 + Bid-to-Bid
 + Offer-to-Offer
 + Bid-to-Ask
 + Offer-to-Bid  

- Time Lagged Correlation (1 second, 10 seconds)
 + Bid-to-Bid
 + Offer-to-Offer
 + Bid-to-Offer
 + Offer-to-Bid


Contemporaneous Correlation of Quotes in the Nearby and Deferred Contracts
========================================================
incremental: true
- Calculated correlations for log changes of Bids (OFRs) in Nearby and:
  + One contract deferred
  + Two contracts deferred
  + Three contracts deferred
- Need to consider the effect of long periods of no revisions on the correlations
  + Both presented 
  
Contemporaneous Correlation
========================================================
![](\Bid_plot.png)
***
- For each day, divide into 24 ten minute sub-samples with observations time-stamped to the second
 + Calculate the correlations in each sub-sample and record
- Figure displays Means and 1 St Dev error bars. 

Contemporaneous Correlation
========================================================
![](\Bid_plot.png)

Contemporaneous Correlation
========================================================
![](\OFR_plot.png)


Contemporaneous Correlation II
========================================================
![](\Bid_plot_no0s.png)

Contemporaneous Correlation II
========================================================
![](\OFR_plot_no0s.png)


Hypothesis: Liquidity Activity vs Information-Based Activity
========================================================
incremental: true
- 'Keep Zeros' Includes many cases where the nearby's TOB was revised 
but the deferred's TOB was not (and vice versa)
- Whether this was a revision of a limit 
order or a transaction, the rest of the forward curve did not respond
- Making or taking liquidity - no information
 + Generates 'low correlation' activity
 
 Hypothesis: Liquidity Activity vs Information-Based Activity
========================================================
incremental: true
- 'No Zeros', by contrast, only includes cases where the TOB of the nearby
and deferred were revised simultaneously (as far as we can tell after aggregating to the second)
- These cases generate nearly perfect correlation. 
- Information?
- This interpretation is consistent with the correlation between the nearby and the 1 deferred 
being lowest in the 'keep zeros' case 
 + Relative to the other deferreds, the 1 deferred contract has the highest volume and (presumably) purely 
 liquidity-based trading
 
USDA Report Days?
========================================================
- Recall that in our sub-sample (2010-2011), reports were released at 8:30am.
 + Before the start of the day trading session

Contemporaneous Correlation
========================================================
![](\Bid_plot_report.png)
report days

Contemporaneous Correlation
========================================================
![](\OFR_plot_report.png)
report days

Contemporaneous Correlation II
========================================================
![](\Bid_plot_no0s_report.png)
report days

Contemporaneous Correlation II
========================================================
![](\OFR_plot_no0s_report.png)
report days

USDA Report Days?
========================================================
- Very similar pattern as a typical day, with a slightly elevated correlation 
at the market open (indicating a higher than usual proportion of information based trading?)
 + This was surprising to us. 
 + Interested to see 2012-2013 when reports are released at 11:00am. 

How Long Does the Correlation Persist?
========================================================
- Lag nearby by 1 second (10 seconds) and calculate the same correlations with the 1 deferred contract

Contemporaneous and Time Lagged Correlations
========================================================
![](\Bid_plot_timelag.png)

Contemporaneous and Time Lagged Correlations
========================================================
![](\OFR_plot_timelag.png)

Correlation Gone Immediately (1 sec)
========================================================
Possible causes?
- HFT? 
 + Anecdotally speaking, HFT did not seem that prevalent (at least not a lot of quote spoofing or pinging)
- Spread Trading
 + An order to the spread book affects both the nearby and deferred simultaneously

Contemporaneous and Time Lagged Correlations
========================================================
![](\Bid_plot_timelag_report.png)
report days


Contemporaneous and Time Lagged Correlations
========================================================
![](\OFR_plot_timelag_report.png)
report days

If Its Spread Traders...
=======================================================
Check Correlation of Bid-to-OFR and OFR-to-Bid
- Recall previous was Bid-to-Bid and OFR-to-OFR

Contemporaneous Correlation
========================================================
![](\BIDOFR_plot.png)

Contemporaneous Correlation
========================================================
![](\OFRBID_plot.png)

Contemporaneous Correlation II
========================================================
![](\BIDOFR_plot_no0s.png)

Contemporaneous and Time Lagged Correlations
========================================================
![](\BIDOFR_plot_timelag.png)

Implications 
========================================================
- Nearby and deferred contract quotes appear to be highly correlated, even to the second
- Indicating an efficiency in the transmission of information up the forward curve
- Correlation does not persist past one second, indicating information has been fully incorporated

Future Research
========================================================
- More detailed quantification of the possible presence of HFT
- Apply the Probability of Informed Trader (PIN) model to the corn market
to test the information hypothesis (Easley and O'Hara 1987)
- Of course, extend to more markets. 


Anecdotes about the Presence of HFT
========================================================

Number of Seconds to a Revision in Bid/Ask
========================================================
![](nsecs_to_update_BID_rets.png)

***
![](nsecs_to_update_OFR_rets.png)


Number of Seconds to a Revision in Bid/Ask
========================================================

![](\p1secs_to_update_BID_rets.png)

***
![](\p1secs_to_update_OFR_rets.png)

Number of Seconds to a Revision in Bid/Ask
========================================================

![](\p2secs_to_update_BID_rets.png)

***
![](\p2secs_to_update_OFR_rets.png)

Number of Seconds to a Revision in Bid/Ask
========================================================

![](\p3secs_to_update_BID_rets.png)

***
![](\p3secs_to_update_OFR_rets.png)
