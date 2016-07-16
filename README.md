HFT, A high-frequency trading simulation package in R.
=======
The goal of HFT is to make it easy to write and test high-frequency trading strategies.

This package provides a simulated environment with most of the realworld operating rules. It also offers varies of order submitting, order flow tracking and summarization functions to simplify strategy writting process. After the simualtion, all intermediate information can be easily fethed and analyzed.

---
### Installation
```R
library(devtools)
install_github("chenhaotian/High-Frequency-Trading-Simulation-System")
```
---
### Getting Started

Let's first show how the simulator works by a demo market-making strategy:

Assume a market-maker believe that the order flow of 6/16 treasury future contract(TF1606) has a strong positive auto-correlation. To avoid risk exposure and increas profit margin, the market-maker plan to place a buy/sell limit order at the bid1/ask1 price whenever there is a relatively large spread and a small seller/buyer initiated transaction amount. All open orders will be canceled if they haven't been executed for 10 seconds. The strategy should be written as follow:

```R
## a unrealistic demo strategy
DemoStrategy <- function(){
    bsi <- BSI(lastprice=lastprice,bid1 = preorderbook$buybook$price[1],ask1 = preorderbook$sellbook$price[1],volume = volume) # BSI return a length-two vetor representing the amounts initiated by buyer and seller
    spread <- orderbook$sellbook$price[1]-orderbook$buybook$price[1] # bid-ask-spread
    if( spread>0.01 & bsi[2]<20 & S("TF1603",longopen.non)){
        ## profit margin is big, seller initiated amount is small and there is no long open order in queue.
        timeoutsubmission(instrumentid="TF1603",direction = 1,orderid = randomid(5),
                          price = orderbook$buybook$price[1],hands = 1,
                          action = "open",timeoutsleep = 10)
    }
    else if(spread>0.01 & bsi[1]<20 & S("TF1603",shortopen.non)){
        ## profit margin is big, buyer initiated amount is small and there is no short open order in queue.
        timeoutsubmission(instrumentid="TF1603",direction = -1,orderid = randomid(5),
                          price = orderbook$sellbook$price[1],hands = 1,
                          action = "open",timeoutsleep = 10)
    }
    chasecloseall("TF1603",chasesleep = 1) # close all open positions.
}
```
Then pass the strategy to the simulator with `HFTsimulator()`:

```R
## start simuation
HFTsimulator(stg = DemoStrategy,        #strategy function
             instrumentids = "TF1603",  #security id(s)
             datalist = TFtaq,          #TAQ data
             formatlist = TFformat,     #TAQ data format
             verboselimitpriors = TRUE) #record all invermediate infornation
```


The general usages is:
```
HFTsimulator(stg = strategy function,
             instrumentids = securities identifiers,
             datalist = TAQ data,
             formatlist = TAQ data format)
```
