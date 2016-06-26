library(HFT)

TF1603  <- function(OMAX){

    spread <- orderbook$sellbook$price[1]-orderbook$buybook$price[1]

    ## long-------------------------------------
    ## 1.no holdings, no open orders
    if(S("TF1603",longopen.non) & S("TF1603",longholdings.non)){
        multisubmission(instrumentid="TF1603",direction = 1, price = orderbook$buybook$price[-1],hands = 1,action = "open")
    }
    else if(S("TF1603",longopen.exist) & S("TF1603",longholdings.non)){
        ## spread---------
        if(max(S("TF1603",longopen)$price)==orderbook$buybook$price[1] & spread<0.01){
            hp <- max(S("TF1603",longopen)$price)
            cancelall(direction = 1,action = "open",pricemin = hp,pricemax = hp)
        }
        ## spread---------
        
        ## 2.no holdings, open orders exist, orderbook move inversely
        if(S("TF1603",longopen.exist)){ #previous step may have canceled all openorders
            if(max(S("TF1603",longopen)$price)<orderbook$buybook$price[2]){
                if(nrow(S("TF1603",longopen))>=OMAX){
                    cancelall(direction = 1,action = "open",pricemax = min(S("TF1603",longopen)$price))
                }
                ordersubmission(instrumentid = "TF1603",orderid = randomid(5),direction = 1, price = orderbook$buybook$price[2],hands = 1,action = "open")
            }
        }
    }
    else if(S("TF1603",longholdings.exist) & S("TF1603",shortclose.non)){
        ## 3.holdings exist, no close orders
        PRICE <- meanopen("TF1603","long")
        PRICE <- round((PRICE+0.005)-PRICE%%0.005,3)
        timeoutchasesubmission("TF1603",orderid = randomid(5),direction = -1,price = PRICE,hands=querycapital(instrumentids = "TF1603")$totallongholdings,action = "close",timeoutsleep = 1,chasesleep = 1)
    }
    else if(S("TF1603",longholdings.exist) & S("TF1603",shortclose.exist)){
        ## 4.holdings exist, close orders exist
        unclosed <- querycapital(instrumentids = "TF1603")$totallongholdings-sum(S("TF1603",shortclose)$hands)
        if(unclosed>0){
            PRICE <- meanopen("TF1603","long")
            PRICE <- round((PRICE+0.005)-PRICE%%0.005,3)
            timeoutchasesubmission("TF1603",orderid = randomid(5),direction = -1,price = PRICE,hands=unclosed,action = "close",timeoutsleep = 1,chasesleep = 1)
        }
    }

    ## short-------------------------------------
    ## 1.no holdings, no open orders
    if(S("TF1603",shortopen.non) & S("TF1603",shortholdings.non)){
        multisubmission(instrumentid="TF1603",direction = -1, price = orderbook$sellbook$price[-1],hands = 1,action = "open")
    }
    else if(S("TF1603",shortopen.exist) & S("TF1603",shortholdings.non)){
        ## spread---------
        if(min(S("TF1603",shortopen)$price)==orderbook$sellbook$price[1] & spread<0.01){
            hp <- min(S("TF1603",shortopen)$price)
            cancelall(direction = -1,action = "open",pricemin = hp,pricemax = hp)
        }
        ## spread---------
        
        ## 2.no holdings, open orders exist, orderbook move inversely
        if(S("TF1603",shortopen.exist)){
            ## previous step may have canceled all openorders
            if(min(S("TF1603",shortopen)$price)>orderbook$sellbook$price[2]){
                if(nrow(S("TF1603",shortopen))>=OMAX){
                    cancelall(direction = -1,action = "open",pricemin = max(S("TF1603",shortopen)$price))
                }
                ordersubmission(instrumentid = "TF1603",orderid = randomid(5),direction = -1, price = orderbook$sellbook$price[2],hands = 1,action = "open")
            }
        }
    }
    else if(S("TF1603",shortholdings.exist) & S("TF1603",longclose.non)){
        ## 3.holdings exist, no close orders
        PRICE <- meanopen("TF1603","short")
        PRICE <- round((PRICE-0.005)-PRICE%%0.005,3)
        timeoutchasesubmission("TF1603",orderid = randomid(5),direction = 1,price = PRICE,hands=abs(querycapital(instrumentids = "TF1603")$totalshortholdings),action = "close",timeoutsleep = 1,chasesleep = 1)
    }
    else if(S("TF1603",shortholdings.exist) & S("TF1603",longclose.exist)){
        ## 4.holdings exist, close orders exist
        unclosed <- abs(querycapital(instrumentids = "TF1603")$totalshortholdings)-sum(S("TF1603",longclose)$hands)
        if(unclosed>0){
            PRICE <- meanopen("TF1603","short")
            PRICE <- round((PRICE-0.005)-PRICE%%0.005,3)
            timeoutchasesubmission("TF1603",orderid = randomid(5),direction = 1,price = PRICE,hands=unclosed,action = "close",timeoutsleep = 1,chasesleep = 1)
        }
    }

    return()
    
}

HFTsimulator(stg = TF1603,OMAX=2,instrumentids = "TF1603",datalist = TFtaq,formatlist = TFformat,verboselimitpriors = TRUE)


## source from github
source_url("https://raw.githubusercontent.com/chenhaotian/High-Frequency-Trading-Simulation-System/master/miscellaneous.r")

res <- tradesummary(TFtaq,"TF1603",endtime = "11:30:00.000")

checklimit(instrumentdata = TFtaq,orderid = res$traded$orderid[81])
checklimit(instrumentdata = TFtaq,orderid = res$traded$orderid[79])
checklimit(instrumentdata = TFtaq,orderid = res$traded$orderid[20])


