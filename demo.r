
source("R/utils.r")
source("R/hftsimulator.r")
load("~/R/Instruments/Qutke/dataforDM")

formatlist <- list(pbuyhands = seq(from = 32,by = 1,length.out = 5),
                     pbuyprice = seq(from = 22,by = 1,length.out = 5),
                     psellhands = seq(from = 37,by = 1,length.out = 5),
                     psellprice = seq(from = 27,by = 1,length.out = 5),
                     ptradetime = 2,plastprice = 4,pvolume = 12,ppresettleprice=8,
                     fee = c(long=0,short=0,closetoday=0,closepreday=0),
                     closeprior = "today",
                     timeformat = "%Y-%m-%d %H:%M:%OS",
                     multiplier = 10000)
instrumentids <- "TF1603"

anomalperiod <- function(HFdata){
    hms <- strftime(strptime(HFdata$TDATETIME,format = "%Y-%m-%d %H:%M:%S"),"%H:%M:%S")
    idx <- (hms>="09:20:00" & hms <="11:30:00") | (hms>="13:00:00" & hms<="15:15:00")
    return(HFdata[idx,])
}
out2 <- anomalperiod(out2)
out2$DATE <- gsub(" .*$","",out2$TDATETIME)

datalist <- subset(out2,DATE=="2015-12-25")

realtime=NULL,writeholding=FALSE,tc=FALSE,IMLAZY = TRUE,septraded = FALSE,unclosed = TRUE

TF1603  <- function(OMAX){

    spread <- orderbook$sellbook$price[1]-orderbook$buybook$price[1]

    ## long-------------------------------------
    ## 1.no holdings, no open orders
    if(TF1603.longopen.non & TF1603.longholdings.non){
        multisubmission(instrumentid="TF1603",direction = 1, price = orderbook$buybook$price[-1],hands = 1,action = "open")
    }
    else if( TF1603.longopen.exist & TF1603.longholdings.non){
        ## spread---------
        if(max(TF1603.longopen%$%"price")==orderbook$buybook$price[1] & spread<0.01){
            hp <- max(TF1603.longopen%$%"price")
            cancelall(direction = 1,action = "open",pricemin = hp,pricemax = hp)
        }
        ## spread---------
        
        ## 2.no holdings, open orders exist, orderbook move inversely
        if(eval(TF1603.longopen.exist)){ #previous step may have canceled all openorders
            if(max(TF1603.longopen%$%"price")<orderbook$buybook$price[2]){
                if(nrow(eval(TF1603.longopen))>=OMAX){
                    cancelall(direction = 1,action = "open",pricemax = min(TF1603.longopen%$%"price"))
                }
                ordersubmission(instrumentid = "TF1603",orderid = randomid(5),direction = 1, price = orderbook$buybook$price[2],hands = 1,action = "open")
            }
        }
    }
    else if(TF1603.longholdings.exist & TF1603.shortclose.non){
        ## 3.holdings exist, no close orders
        PRICE <- meanopen("TF1603","long")
        PRICE <- round((PRICE+0.005)-PRICE%%0.005,3)
        timeoutchasesubmission("TF1603",orderid = randomid(5),direction = -1,price = PRICE,hands=querycapital(instrumentids = "TF1603")$totallongholdings,action = "close",timeoutsleep = 1,chasesleep = 1)
    }
    else if(TF1603.longholdings.exist & TF1603.shortclose.exist){
        ## 4.holdings exist, close orders exist
        unclosed <- querycapital(instrumentids = "TF1603")$totallongholdings-sum(eval(TF1603.shortclose)$hands)
        if(unclosed>0){
            PRICE <- meanopen("TF1603","long")
            PRICE <- round((PRICE+0.005)-PRICE%%0.005,3)
            timeoutchasesubmission("TF1603",orderid = randomid(5),direction = -1,price = PRICE,hands=unclosed,action = "close",timeoutsleep = 1,chasesleep = 1)
        }
    }

    ## short-------------------------------------
    ## 1.no holdings, no open orders
    if(TF1603.shortopen.non & TF1603.shortholdings.non){
        multisubmission(instrumentid="TF1603",direction = -1, price = orderbook$sellbook$price[-1],hands = 1,action = "open")
    }
    else if( TF1603.shortopen.exist & TF1603.shortholdings.non){
        ## spread---------
        if(min(TF1603.shortopen%$%"price")==orderbook$sellbook$price[1] & spread<0.01){
            hp <- min(TF1603.shortopen%$%"price")
            cancelall(direction = -1,action = "open",pricemin = hp,pricemax = hp)
        }
        ## spread---------
        
        ## 2.no holdings, open orders exist, orderbook move inversely
        if(eval(TF1603.shortopen.exist)){
            ## previous step may have canceled all openorders
            if(min(TF1603.shortopen%$%"price")>orderbook$sellbook$price[2]){
                if(nrow(eval(TF1603.shortopen))>=OMAX){
                    cancelall(direction = -1,action = "open",pricemin = max(TF1603.shortopen%$%"price"))
                }
                ordersubmission(instrumentid = "TF1603",orderid = randomid(5),direction = -1, price = orderbook$sellbook$price[2],hands = 1,action = "open")
            }
        }
    }
    else if(TF1603.shortholdings.exist & TF1603.longclose.non){
        ## 3.holdings exist, no close orders
        PRICE <- meanopen("TF1603","short")
        PRICE <- round((PRICE-0.005)-PRICE%%0.005,3)
        timeoutchasesubmission("TF1603",orderid = randomid(5),direction = 1,price = PRICE,hands=abs(querycapital(instrumentids = "TF1603")$totalshortholdings),action = "close",timeoutsleep = 1,chasesleep = 1)
    }
    else if(TF1603.shortholdings.exist & TF1603.longclose.exist){
        ## 4.holdings exist, close orders exist
        unclosed <- abs(querycapital(instrumentids = "TF1603")$totalshortholdings)-sum(eval(TF1603.longclose)$hands)
        if(unclosed>0){
            PRICE <- meanopen("TF1603","short")
            PRICE <- round((PRICE-0.005)-PRICE%%0.005,3)
            timeoutchasesubmission("TF1603",orderid = randomid(5),direction = 1,price = PRICE,hands=unclosed,action = "close",timeoutsleep = 1,chasesleep = 1)
        }
    }

    return()
    
}

HFTsimulator(stg = TF1603,OMAX=2,instrumentids = instrumentids,datalist = datalist,formatlist = formatlist,IMLAZY = TRUE)

source("miscellaneous.r")

res <- tradesummary(datalist,"TF1603",endtime = "11:30:00.000")

checklimit(instrumentdata = datalist,orderid = res$traded$orderid[81])
