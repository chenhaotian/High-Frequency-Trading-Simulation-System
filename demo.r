library(HFT)

## a unrealistic demo strategy
DemoStrategy <- function(){
    bsi <- BSI(lastprice=lastprice,bid1 = preorderbook$buybook$price[1],ask1 = preorderbook$sellbook$price[1],volume = volume) # BSI return a length-two vetor representing the amount initiated by buyer and seller
    spread <- orderbook$sellbook$price[1]-orderbook$buybook$price[1] # bid-ask-spread
    if( spread>0.01 & bsi[2]<20 & S("TF1603",longopen.non)){
        ## profit margin is big, seller initiated amount is small, and there is no long open order in queue.
        timeoutsubmission(instrumentid="TF1603",direction = 1,orderid = randomid(5),
                          price = orderbook$buybook$price[1],hands = 1,
                          action = "open",timeoutsleep = 10) #submit a long open order, canceled it if no execution in 10 seconds.
    }
    else if(spread>0.01 & bsi[1]<20 & S("TF1603",shortopen.non)){
        ## profit margin is big, buyer initiated amount is small, and there is no short open order in queue.
        timeoutsubmission(instrumentid="TF1603",direction = -1,orderid = randomid(5),
                          price = orderbook$sellbook$price[1],hands = 1,
                          action = "open",timeoutsleep = 10) #submit a short open order, canceled it if no execution in 10 seconds.
    }
    chasecloseall("TF1603",chasesleep = 1) # close all open positions.
}

## start simulation
res1 <- HFTsimulator(stg = DemoStrategy,        #strategy function
                     instrumentids = "TF1603",  #security id(s)
                     datalist = TFtaq,          #TAQ data
                     formatlist = TFformat)     #TAQ data format


## source from github
library(devtools)
source_url("https://raw.githubusercontent.com/chenhaotian/High-Frequency-Trading-Simulation-System/master/miscellaneous.r")

## am
## res2 <- tradesummary(TFtaq,"TF1603",starttime = "09:15:00.000",endtime = "11:30:00.000")
## pm
res2 <- tradesummary(TFtaq,"TF1603",starttime = "13:00:00.000",endtime = "15:15:00.000") #summary plot

## check details of some specific orders.
checklimit(instrumentdata = TFtaq,orderid = res2$traded$orderid[81]) #check the 81st traded limit order's life experience
checklimit(instrumentdata = TFtaq,orderid = res2$traded$orderid[79])
checklimit(instrumentdata = TFtaq,orderid = res2$traded$orderid[20])
