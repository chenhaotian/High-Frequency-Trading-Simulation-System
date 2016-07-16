library(HFT)

## a unrealistic demo strategy
DemoStrategy <- function(){
    ## identify current transaction's direction
    ## BSI return a length-two vetor representing the amount initiated by buyer and seller
    bsi <- BSI(lastprice=lastprice,bid1 = preorderbook$buybook$price[1],ask1 = preorderbook$sellbook$price[1],volume = volume)
    ## bid-ask-spread
    spread <- orderbook$sellbook$price[1]-orderbook$buybook$price[1]
    if( spread>0.01 & bsi[2]<20 & S("TF1603",longopen.non)){
        ## profit margin is big, seller initiated amount is small, and there is no long open order in queue.
        timeoutsubmission(instrumentid="TF1603",direction = 1,orderid = randomid(5),
                          price = orderbook$buybook$price[1],hands = 1,
                          action = "open",timeoutsleep = 10)
    }
    else if(spread>0.01 & bsi[1]<20 & S("TF1603",shortopen.non)){
        ## profit margin is big, buyer initiated amount is small, and there is no short open order in queue.
        timeoutsubmission(instrumentid="TF1603",direction = -1,orderid = randomid(5),
                          price = orderbook$sellbook$price[1],hands = 1,
                          action = "open",timeoutsleep = 10)
    }
    ## close all open positions.
    chasecloseall("TF1603",chasesleep = 1)
}

## start simuation
HFTsimulator(stg = DemoStrategy,        #strategy function
             instrumentids = "TF1603",  #security id(s)
             datalist = TFtaq,          #TAQ data
             formatlist = TFformat,     #TAQ data format
             verboselimitpriors = TRUE) #record all invermediate infornation

## source from github
source_url("https://raw.githubusercontent.com/chenhaotian/High-Frequency-Trading-Simulation-System/master/miscellaneous.r")

## am
res <- tradesummary(TFtaq,"TF1603",starttime = "09:15:00.000",endtime = "11:30:00.000")
## pm
res <- tradesummary(TFtaq,"TF1603",starttime = "13:00:00.000",endtime = "15:15:00.000")

## check details of some specific orders.
checklimit(instrumentdata = TFtaq,orderid = res$traded$orderid[81])
checklimit(instrumentdata = TFtaq,orderid = res$traded$orderid[79])
checklimit(instrumentdata = TFtaq,orderid = res$traded$orderid[20])
