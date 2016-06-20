.extractinfo <- function(info=c("tradetime","lastprice","volume","orderbook","HMOS","presettleprice"),EXdata,ptradetime,plastprice,pvolume,pbuyhands,pbuyprice,psellhands,psellprice,ppresettleprice,timeformat="%Y-%m-%d %H:%M:%OS"){
    match.arg(info,choices = c("tradetime","lastprice","volume","orderbook","HMOS","presettleprice"))
    
    return(
        switch(info,
               tradetime=strftime(strptime(TRADETIME,timeformat),format = "%Y-%m-%d %H:%M:%OS"),
               lastprice=round(as.numeric(EXdata[plastprice]),5),
               volume=round(as.numeric(EXdata[pvolume]),5),
               orderbook=list(
                   buybook=data.frame(hands=round(as.numeric(EXdata[pbuyhands]),5),
                       price=round(as.numeric(EXdata[pbuyprice]),5)),
                   sellbook=data.frame(hands=round(as.numeric(EXdata[psellhands]),5),
                       price=round(as.numeric(EXdata[psellprice]),5))
                   ),
               HMOS=paste("1970-01-01",strftime(strptime(TRADETIME,timeformat),format = "%H:%M:%OS")),
               presettleprice=round(as.numeric(EXdata[ppresettleprice]),5)
               )
        )
}

.DEFMACRO <- function(..., expr){
    expr <- substitute(expr)
    a <- substitute(list(...))[-1]
    nn <- names(a)
    if (is.null(nn)) 
        nn <- rep("", length(a))
    for (i in 1:length(a)) {
        if (nn[i] == "") {
            nn[i] <- paste(a[[i]])
            msg <- paste(a[[i]], "not supplied")
            a[[i]] <- substitute(stop(foo), list(foo = msg))
        }
        if (nn[i] == "DOTS") {
            nn[i] <- "..."
            a[[i]] <- formals(function(...) {
            })[[1]]
        }
    }
    names(a) <- nn
    a <- as.list(a)
    ff <- eval(substitute(function() {
        tmp <- substitute(body)
        eval(tmp, parent.frame())
    }, list(body = expr)))
    formals(ff) <- a
    mm <- match.call()
    mm$expr <- NULL
    mm[[1]] <- as.name("macro")
    attr(ff, "source") <- c(deparse(mm), deparse(expr))
    ff
}


.capchange <- .DEFMACRO(TODAY,TOTAL,HANDS,COMMISSION,expr={
    ## cashchange <- (-1)*direction*HANDS*tradeprice-HANDS*tradeprice*COMMISSION
    idx <- .GlobalEnv$tradingstates$capital$instrumentid==instrumentid
    ## initialize new instrument
    if(!any(idx)){
        .GlobalEnv$tradingstates$capital <- rbind(.GlobalEnv$tradingstates$capital,data.frame(instrumentid=instrumentid,longholdingstoday=0,shortholdingstoday=0,longholdingspreday=0,shortholdingspreday=0,totallongholdings=0,totalshortholdings=0,cash=0,stringsAsFactors=FALSE))
        idx <- nrow(.GlobalEnv$tradingstates$capital)
    }
    handschange <- HANDS*direction
    trans <- handschange*tradeprice*(-1)*multiplier
    cost <- cost + HANDS*tradeprice*COMMISSION*multiplier
    .GlobalEnv$tradingstates$capital$cash[idx] <- .GlobalEnv$tradingstates$capital$cash[idx]+trans-cost
    .GlobalEnv$tradingstates$capital$TODAY[idx] <- .GlobalEnv$tradingstates$capital$TODAY[idx]+handschange
    .GlobalEnv$tradingstates$capital$TOTAL[idx] <- .GlobalEnv$tradingstates$capital$TOTAL[idx]+handschange
    ## capital calculation needs prices of many different instruments......
})

.updatecapital <- function(instrumentid,direction,hands,action,tradeprice,fee,closeprior="today",multiplier=10000){
    
    ## cost of current transaction
    cost <- 0
    idx <- .GlobalEnv$tradingstates$capital$instrumentid==instrumentid
    if(action=="close"){
        if(closeprior=="today"){
            if(direction==-1){
                ## close long, direction==-1!!!!!!!!!
                ## longholdings>=0
                if(hands<=.GlobalEnv$tradingstates$capital$longholdingstoday[idx]){
                    .capchange(longholdingstoday,totallongholdings,
                              hands,fee["closetoday"])
                }
                else{
                    close1 <- .GlobalEnv$tradingstates$capital$longholdingstoday[idx]
                    .capchange(longholdingstoday,totallongholdings,
                              close1,fee["closetoday"])
                    close2 <- hands-close1
                    .capchange(longholdingspreday,totallongholdings,
                              close2,fee["closepreday"])
                }
            }
            else{
                ## close short, direction==1!!!!!!!!!
                ## shortholdings<=0!!!!!!
                if(hands<=(-.GlobalEnv$tradingstates$capital$shortholdingstoday[idx])){
                    .capchange(shortholdingstoday,totalshortholdings,
                              hands,fee["closetoday"])
                }
                else{
                    close1 <- (-.GlobalEnv$tradingstates$capital$shortholdingstoday[idx])
                    .capchange(shortholdingstoday,totalshortholdings,
                              close1,fee["closetoday"])
                    close2 <- hands-close1
                    .capchange(shortholdingspreday,totalshortholdings,
                              close2,fee["closepreday"])
                }
            }
        }
        else{
            ## closeprior=="preday"
            if(direction==-1){
                ## close long, direction==-1!!!!!!!!!
                ## longholdings>=0
                if(hands<=.GlobalEnv$tradingstates$capital$longholdingspreday[idx]){
                    .capchange(longholdingspreday,totallongholdings,
                              hands,fee["closepreday"])
                }
                else{
                    close1 <- .GlobalEnv$tradingstates$capital$longholdingspreday[idx]
                    .capchange(longholdingspreday,totallongholdings,
                              close1,fee["closepreday"])
                    close2 <- hands-close1
                    .capchange(longholdingstoday,totallongholdings,
                              close2,fee["closetoday"])
                }
            }
            else{
                ## close short, direction==1!!!!!!!!!
                ## shortholdings<=0!!!!!!
                if(hands<=(-.GlobalEnv$tradingstates$capital$shortholdingspreday[idx])){
                    .capchange(shortholdingspreday,totalshortholdings,
                              hands,fee["closepreday"])
                }
                else{
                    close1 <- (-.GlobalEnv$tradingstates$capital$shortholdingspreday[idx])
                    .capchange(shortholdingspreday,totalshortholdings,
                              close1,fee["closepreday"])
                    close2 <- hands-close1
                    .capchange(shortholdingstoday,totalshortholdings,
                              close2,fee["closetoday"])
                }
            }
        }
    }
    else if(action=="open"){
        if(direction==1){
            .capchange(longholdingstoday,totallongholdings,
                      hands,fee["long"])
        }
        else{
            .capchange(shortholdingstoday,totalshortholdings,
                      hands,fee["short"])
        }
    }
    else if(action=="closetoday"){
        if(direction==-1){
            ## close long, direction==-1!!!!!!!!!
            .capchange(longholdingstoday,totallongholdings,
                      hands,fee["closetoday"])
        }
        else{
            ## close short, direction==1!!!!!!!!!
            .capchange(shortholdingstoday,totalshortholdings,
                      hands,fee["closetoday"])
        }
    }
    else{
        ## action=="closepreday"
        if(direction==-1){
            ## close long, direction==-1!!!!!!!!!
            .capchange(longholdingspreday,totallongholdings,
                      hands,fee["closepreday"])
        }
        else{
            ## close short, direction==1!!!!!!!!!
            .capchange(shortholdingspreday,totalshortholdings,
                      hands,fee["closepreday"])
        }
    }
    ## return transaction cost
    return(cost)
}

## record traded orders' history
.writetraded <- function(instrumentid,orderid,action,direction,tradehands,tradeprice){
    ## write memory then return
    if(.GlobalEnv$tradingstates$septraded){
        if(action=="open"){
            if(direction==1){
                .GlobalEnv$tradingstates$longopen <- rbind(
                    .GlobalEnv$tradingstates$longopen,
                    data.frame(
                        instrumentid=instrumentid,orderid=orderid,
                        tradehands=tradehands,
                        tradeprice=tradeprice,
                        stringsAsFactors = FALSE)
                )
            }
            else{
                ## direction==-1
                .GlobalEnv$tradingstates$shortopen <- rbind(
                    .GlobalEnv$tradingstates$shortopen,
                    data.frame(
                        instrumentid=instrumentid,orderid=orderid,
                        tradehands=tradehands,
                        tradeprice=tradeprice,
                        stringsAsFactors = FALSE)
                )
            }
        }
        else{
            ## action==close
            if(direction==1){
                .GlobalEnv$tradingstates$longclose <- rbind(
                    .GlobalEnv$tradingstates$longclose,
                    data.frame(
                        instrumentid=instrumentid,orderid=orderid,
                        tradehands=tradehands,
                        tradeprice=tradeprice,
                        stringsAsFactors = FALSE)
                )
            }
            else{
                ## direction==-1
                .GlobalEnv$tradingstates$shortclose <- rbind(
                    .GlobalEnv$tradingstates$shortclose,
                    data.frame(
                        instrumentid=instrumentid,orderid=orderid,
                        tradehands=tradehands,
                        tradeprice=tradeprice,
                        stringsAsFactors = FALSE)
                )
            }
        }
        return()
    }
    else{
        return()
    }
}

## involve mean open price calculation, must be executed before .trackunclosed()!!!!!!!!!!! 
.trackclosed <- function(instrumentid,action,direction,tradehands,tradeprice,multiplier){
    if(!.GlobalEnv$tradingstates$closed){
        return()
    }
    if(action=="close"){
        if(direction==1){
            ## short holding
            MEANOPEN <- meanopen(instrumentid,"short")
        }
        else{
            MEANOPEN <- meanopen(instrumentid,"long")
        }
        .GlobalEnv$tradingstates$closedtracker$cash[.GlobalEnv$tradingstates$closedtracker$instrumentid==instrumentid] <- .GlobalEnv$tradingstates$closedtracker$cash[.GlobalEnv$tradingstates$closedtracker$instrumentid==instrumentid]+(MEANOPEN-tradeprice)*tradehands*direction*multiplier
    }
    return()
}

## .trackunclosed open orders, use the same format as .writetraded
.trackunclosed <- function(instrumentid,orderid,action,direction,tradehands,tradeprice){
    if(!.GlobalEnv$tradingstates$unclosed){
        return()
    }
    
    if(action=="open"){
        if(direction==1){
            .GlobalEnv$tradingstates$unclosedlong <- rbind(
                .GlobalEnv$tradingstates$unclosedlong,
                data.frame(
                    instrumentid=instrumentid,orderid=orderid,
                    action="open",direction=1,
                    tradehands=tradehands,
                    tradeprice=tradeprice,
                    stringsAsFactors = FALSE)
            )
        }
        else{
            ## direction==-1
            .GlobalEnv$tradingstates$unclosedshort <- rbind(
                .GlobalEnv$tradingstates$unclosedshort,
                data.frame(
                    instrumentid=instrumentid,orderid=orderid,
                    action="open",direction=-1,
                    tradehands=tradehands,
                    tradeprice=tradeprice,
                    stringsAsFactors = FALSE)
            )
        }
    }
    else{
        ## action==close
        if(direction==1){
            OPEN <- .GlobalEnv$tradingstates$unclosedshort[.GlobalEnv$tradingstates$unclosedshort$instrumentid==instrumentid,]
            cumopen <- cumsum(OPEN$tradehands)
            remained <- cumopen-tradehands
            L <- nrow(OPEN)
            ## all have been closed
            if(all(remained<=0)){
                .GlobalEnv$tradingstates$unclosedshort <- rbind(.GlobalEnv$tradingstates$unclosedshort[.GlobalEnv$tradingstates$unclosedshort$instrumentid!=instrumentid,],OPEN[-(1:L),])
                return()
            }
            idx <- which(remained>0)[1]
            OPEN$tradehands[idx] <- remained[idx]
            .GlobalEnv$tradingstates$unclosedshort <- rbind(.GlobalEnv$tradingstates$unclosedshort[.GlobalEnv$tradingstates$unclosedshort$instrumentid!=instrumentid,],OPEN[idx:L,])
        }
        else{
            ## direction==-1
            OPEN <- .GlobalEnv$tradingstates$unclosedlong[.GlobalEnv$tradingstates$unclosedlong$instrumentid==instrumentid,]
            cumopen <- cumsum(OPEN$tradehands)
            remained <- cumopen-tradehands
            L <- nrow(OPEN)
            ## all have been closed
            if(all(remained<=0)){
                .GlobalEnv$tradingstates$unclosedlong <- rbind(.GlobalEnv$tradingstates$unclosedlong[.GlobalEnv$tradingstates$unclosedlong$instrumentid!=instrumentid,],OPEN[-(1:L),])
                return()
            }
            idx <- which(remained>0)[1]
            OPEN$tradehands[idx] <- remained[idx]
            .GlobalEnv$tradingstates$unclosedlong <- rbind(.GlobalEnv$tradingstates$unclosedlong[.GlobalEnv$tradingstates$unclosedlong$instrumentid!=instrumentid,],OPEN[idx:L,])
        }
    }
    return()
}

.writeorderhistory <- function(instrumentid,orderid,direction,hands,price,tradeprice,status,action,cost,tradetime=.GlobalEnv$tradingstates$currenttradetime){
    ## write memory then return
    .GlobalEnv$tradingstates$orderhistory <- rbind(
        .GlobalEnv$tradingstates$orderhistory,
        data.frame(
            instrumentid=instrumentid,orderid=orderid,
            direction=direction,price=price,
            hands=hands,action=action,
            tradetime=tradetime,
            tradeprice=tradeprice,
            cost=cost,status=status,
            initialhands=ifelse(action=="cancel",0,.GlobalEnv$tradingstates$orders$initialhands[.GlobalEnv$tradingstates$orders$orderid==orderid]),
            stringsAsFactors = FALSE)
    )
    return()
}

.writecapitalhistory <- function(instrumentid,tradeprice,tradehands,cost,tradetime=.GlobalEnv$tradingstates$currenttradetime){
    
    .GlobalEnv$tradingstates$capitalhistory <- rbind(
        .GlobalEnv$tradingstates$capitalhistory,
        cbind(
            .GlobalEnv$tradingstates$capital[.GlobalEnv$tradingstates$capital$instrumentid==instrumentid,],
            data.frame(
                tradetime=tradetime,
                tradeprice=tradeprice,tradehands=tradehands,cost=cost,
                stringsAsFactors=FALSE)
        )
    )
    return()
}


.eatbook <- function(instrumentid,market,book,fee,closeprior="today",multiplier){
    ## stop condition
    if(nrow(book)==0)
        return(market)
    if(book$hands[1]>=market$hands){    #eat market hands
        cost <- .updatecapital(instrumentid,market$direction,market$hands,market$action,book$price[1],fee,closeprior,multiplier)
        ## write history
        .writeorderhistory(instrumentid,market$orderid,market$direction,0,market$price,tradeprice=book$price[1],status=0,action=market$action,cost)
        .writecapitalhistory(instrumentid,tradeprice=book$price[1],tradehands=market$hands,cost)
        .writetraded(instrumentid,market$orderid,market$action,market$direction,market$hands,book$price[1])
        .trackclosed(instrumentid,market$action,market$direction,market$hands,book$price[1],multiplier)
        .trackunclosed(instrumentid,market$orderid,market$action,market$direction,market$hands,book$price[1])
        return(market[-1,])
    }
    else{                               #eat book
        ## match case
        cost <- .updatecapital(instrumentid,market$direction,book$hands[1],market$action,book$price[1],fee,closeprior,multiplier)
        market$hands <- market$hands-book$hands[1]
        ## write history
        .writeorderhistory(instrumentid,market$orderid,market$direction,market$hands,market$price,tradeprice=book$price[1],status=1,action=market$action,cost)
        .writecapitalhistory(instrumentid,tradeprice=book$price[1],tradehands=book$hands[1],cost)
        .writetraded(instrumentid,market$orderid,market$action,market$direction,book$hands[1],book$price[1])
        .trackclosed(instrumentid,market$action,market$direction,book$hands[1],book$price[1],multiplier)
        .trackunclosed(instrumentid,market$orderid,market$action,market$direction,book$hands[1],book$price[1])

        book <- book[-1,]
        ## recursion
        .eatbook(instrumentid,market,book,fee,closeprior,multiplier=multiplier)
    }
}

.eatpath <- function(instrumentid,limit,remained,fee,closeprior="today",multiplier){
    if(all(remained<=0))
        return(limit)
    idx <- which(remained>0)
    executed <- rep(0,length(remained))
    executed[idx] <- pmin(limit$hands[idx],remained[idx])
    limit$hands[idx] <- limit$hands[idx]-executed[idx]
    
    for(id in idx){
        cost <- .updatecapital(instrumentid = instrumentid,direction = limit$direction[id],hands = executed[id],action = limit$action[id],tradeprice = limit$price[id],fee=fee,closeprior = closeprior,multiplier=multiplier)
        .writeorderhistory(instrumentid,
                          orderid = limit$orderid[id],
                          direction = limit$direction[id],
                          hands = limit$hands[id],
                          price = limit$price[id],
                          tradeprice = limit$price[id],
                          status=ifelse(limit$hands[id]==0,0,1),
                          action=limit$action[id],cost=cost)
        .writecapitalhistory(instrumentid,tradeprice=limit$price[id],tradehands=executed[id],cost)
        .writetraded(instrumentid,limit$orderid[id],limit$action[id],limit$direction[id],executed[id],limit$price[id])
        .trackclosed(instrumentid,limit$action[id],limit$direction[id],executed[id],limit$price[id],multiplier)
        .trackunclosed(instrumentid,limit$orderid[id],limit$action[id],limit$direction[id],executed[id],limit$price[id])
    }
    
    ## limit$hands[idx] <- limit$hands[idx]-executed[idx]
    return(limit[limit$hands!=0,])
}

.eatprior <- function(book,volume){
    if(nrow(book)==0 | volume==0)
        return(list(
            book=data.frame(price=numeric(),hands=numeric(),stringsAsFactors = FALSE),
            volume=volume))
    else{
        if(book$hands[1]>volume){
            book$hands[1] <- book$hands[1]-volume
            return(list(book=book,volume=0))
        }
        else if(book$hands[1]==volume){
            return(list(book=book[-1,],volume=0))
        }
        else{ #book$hands[1]<volume
            return(.eatprior(book[-1,],volume-book$hands[1]))
        }
    }
}

.eatpriors <- function(limit,lastprice,volume,direction,preorderbook){
    if(direction==1){
        remained <- vapply(limit$orderid,function(id){
            idx <- limit$orderid==id
            ## price condition not met
            if(limit$price[idx]<lastprice){
                return(0)
            }else{
                ## eat high priority orders in preorderbook
                marketremained <- volume-sum(preorderbook$buybook$hands[preorderbook$buybook$price>limit$price[idx]])
                if(marketremained<=0){
                    return(0)
                }
                else{
                    ## eat prior limit orders
                    if(nrow(.GlobalEnv$tradingstates$limitprior[[id]])==0){
                        return(marketremained)
                    }else{
                        priorreamined <- marketremained-.GlobalEnv$tradingstates$limitprior[[id]]$hands
                        if(priorreamined>=0){
                            .GlobalEnv$tradingstates$limitprior[[id]] <- data.frame(hands=numeric(),price=numeric(),stringsAsFactors=FALSE)
                            return(priorreamined)
                        }else{
                            .GlobalEnv$tradingstates$limitprior[[id]]$hands <- -priorreamined
                            return(0)
                        }
                    }
                    
                }
            }
        },FUN.VALUE = 1)
    }
    else{
        ## direction==-1
        remained <- vapply(limit$orderid,function(id){
            idx <- limit$orderid==id
            ## price condition not met
            if(limit$price[idx]>lastprice){
                return(0)
            }else{
                ## eat high priority orders in preorderbook
                marketremained <- volume-sum(preorderbook$sellbook$hands[preorderbook$sellbook$price<limit$price[idx]])
                if(marketremained<=0){
                    return(0)
                }else{
                    ## eat prior limit orders
                    if(nrow(.GlobalEnv$tradingstates$limitprior[[id]])==0){
                        return(marketremained)
                    }else{
                        priorreamined <- marketremained-.GlobalEnv$tradingstates$limitprior[[id]]$hands
                        if(priorreamined>=0){
                            .GlobalEnv$tradingstates$limitprior[[id]] <- data.frame(hands=numeric(),price=numeric(),stringsAsFactors=FALSE)
                            return(priorreamined)
                        }else{
                            .GlobalEnv$tradingstates$limitprior[[id]]$hands <- -priorreamined
                            return(0)
                        }
                    }
                }
            }

        },FUN.VALUE = 1)
    }
    return(remained)
}

.cancledetector <- function(limit,book,direction){
    if(direction==1)
        dumped <- vapply(limit$orderid,function(id){
            ## no prior orders
            if(nrow(.GlobalEnv$tradingstates$limitprior[[id]])==0){
                return(1)
            }
            ## nothing change
            if(.GlobalEnv$tradingstates$limitprior[[id]]$price<min(book$price)){
                return(1)
            }
            else{
                change <- .GlobalEnv$tradingstates$limitprior[[id]]
                currenthands <- book$hands[match(change$price,book$price)]
                ## currenthands might be NA
                change$hands <- min(change$hands,ifelse(is.na(currenthands),0,currenthands))
                .GlobalEnv$tradingstates$limitprior[[id]] <- change[change$hands!=0,]
                return(1)
            }
        },FUN.VALUE = 1)
    else
        dumped <- vapply(limit$orderid,function(id){
            ## no prior orders
            if(nrow(.GlobalEnv$tradingstates$limitprior[[id]])==0){
                return(1)
            }
            if(.GlobalEnv$tradingstates$limitprior[[id]]$price>max(book$price)){
                ## nothing change
                return(1)
            }
            else{
                change <- .GlobalEnv$tradingstates$limitprior[[id]]
                currenthands <- book$hands[match(change$price,book$price)]
                ## currenthands might be NA
                change$hands <- min(change$hands,ifelse(is.na(currenthands),0,currenthands))
                .GlobalEnv$tradingstates$limitprior[[id]] <- change[change$hands!=0,]
                return(1)
            }
        },FUN.VALUE = 1)
    return()
}

.updateinstrument <- function(instrumentid,lastprice,volume,orderbook,preorderbook,fee,closeprior="today",multiplier){
    currentinstrument <- .GlobalEnv$tradingstates$orders[.GlobalEnv$tradingstates$orders$instrumentid==instrumentid,]
    
    if(nrow(currentinstrument)==0){
        return()
    }
    ## market order ----------------------
    ## at most two rows, long and short
    market <- currentinstrument[currentinstrument$price==0,]
    if(nrow(market)!=0){
        longopen <- market[market$direction==1&market$action=="open",]
        if(nrow(longopen)>0){
            longopen <- .eatbook(instrumentid,longopen,orderbook$sellbook,fee,closeprior,multiplier)
        }
        longclose <- market[market$direction==1&market$action=="close",]
        if(nrow(longclose)>0){
            longclose <- .eatbook(instrumentid,longclose,orderbook$sellbook,fee,closeprior,multiplier)
        }
        longclosetoday <- market[market$direction==1&market$action=="closetoday",]
        if(nrow(longclosetoday)>0){
            longclosetoday <- .eatbook(instrumentid,longclosetoday,orderbook$sellbook,fee,closeprior,multiplier)
        }
        longclosepreday <- market[market$direction==1&market$action=="closetoday",]
        if(nrow(longclosepreday)>0){
            longclosepreday <- .eatbook(instrumentid,longclosepreday,orderbook$sellbook,fee,closeprior,multiplier)
        }
        
        shortopen <- market[market$direction==-1&market$action=="open",]
        if(nrow(shortopen)>0){
            shortopen <- .eatbook(instrumentid,shortopen,orderbook$buybook,fee,closeprior,multiplier)
        }
        shortclose <- market[market$direction==-1&market$action=="close",]
        if(nrow(shortclose)>0){
            shortclose <- .eatbook(instrumentid,shortclose,orderbook$buybook,fee,closeprior,multiplier)
        }
        shortclosetoday <- market[market$direction==-1&market$action=="closetoday",]
        if(nrow(shortclosetoday)>0){
            shortclosetoday <- .eatbook(instrumentid,shortclosetoday,orderbook$buybook,fee,closeprior,multiplier)
        }
        shortclosepreday <- market[market$direction==-1&market$action=="closetoday",]
        if(nrow(shortclosepreday)>0){
            shortclosepreday <- .eatbook(instrumentid,shortclosepreday,orderbook$buybook,fee,closeprior,multiplier)
        }
        market <- rbind(longopen,longclose,longclosetoday,longclosepreday,shortopen,shortclose,shortclosetoday,shortclosepreday)
    }
    
    ## limit order ----------------------
    LIMIT <- currentinstrument[currentinstrument$price!=0,]
    if(nrow(LIMIT)!=0){
        ## sell initiated?
        mid <- (preorderbook$buybook$price[1]+preorderbook$sellbook$price[1])/2
        if(lastprice>mid+0.0000001){
            SI <- FALSE
        }
        else if(lastprice<mid-0.0000001){
            SI <- TRUE
        }
        else{
            SI <- ifelse(runif(1)>0.5,TRUE,FALSE)
        }
        
        longlimit <- LIMIT[LIMIT$direction==1,]
        if(nrow(longlimit)>0){
            ## IMPORTANT: affect mean open price
            longlimit <- longlimit[order(longlimit$price,decreasing = TRUE),]
            if( (volume==0) | (!SI))
                .cancledetector(longlimit,orderbook$buybook,direction=1)                #volume==0 | (!SI)
            if(SI & volume>0.0000001){
                ## eat prior limit orders first
                remained <- .eatpriors(limit=longlimit,lastprice=lastprice,volume = volume,direction=1,preorderbook=preorderbook)
                ## then eat our limit orders
                longlimit <- .eatpath(instrumentid = instrumentid,limit=longlimit,remained = remained,fee = fee,closeprior = closeprior,multiplier = multiplier)
            }
        }
        
        shortlimit <- LIMIT[LIMIT$direction==-1,]
        if(nrow(shortlimit)>0){
            shortlimit <- shortlimit[order(shortlimit$price,decreasing = FALSE),]
            if( (volume==0) | SI)
                .cancledetector(shortlimit,orderbook$sellbook,direction=-1)                #volume==0 | SI
            if((!SI) & volume>0.0000001){
                remained <- .eatpriors(limit=shortlimit,lastprice=lastprice,volume = volume,direction=-1,preorderbook=preorderbook)
                shortlimit <- .eatpath(instrumentid = instrumentid,limit=shortlimit,remained = remained,fee = fee,closeprior = closeprior,multiplier = multiplier)
            }
        }
        
        LIMIT <- rbind(longlimit,shortlimit)
    }
    
    ## combine remaining orders
    .GlobalEnv$tradingstates$orders <- rbind(market,LIMIT,.GlobalEnv$tradingstates$orders[.GlobalEnv$tradingstates$orders$instrumentid!=instrumentid,])
    
    return()
}

.priororders <- function(mostrecentorderbook,orderid,direction,price){
    if(direction==1){
        ## if all idx are FALSE, mostrecentorderbook$buybook[idx,] will be a data.frame with zero row
        .GlobalEnv$tradingstates$limitprior[[orderid]] <- mostrecentorderbook$buybook[mostrecentorderbook$buybook$price==price,]
    }
    else{
        ## if all idx are FALSE, mostrecentorderbook$sellbook[idx,] will be a data.frame with zero row
        .GlobalEnv$tradingstates$limitprior[[orderid]] <- mostrecentorderbook$sellbook[mostrecentorderbook$sellbook$price==price,]
    }
    return()
}

.sucker <- .DEFMACRO(LONGHOLDINGS,SHORTHOLDINGS,expr = {
    vol <- abs(hands)
    if(direction==-1){
        ## close long, hold>0, untrade<0
        hold <- sum(.GlobalEnv$tradingstates$capital$LONGHOLDINGS[.GlobalEnv$tradingstates$capital$instrumentid==instrumentid])
        nethold <- hold+untrade
        if( (hold==0) | direction==sign(nethold) |
           vol>abs(hold) | vol>abs(nethold) |
           (any(currentinstrument$price==0&currentinstrument$direction==direction&currentinstrument$action%in%c("close",action)) & price==0) ){
            .writeorderhistory(instrumentid,orderid,direction,hands,price,tradeprice=0,status=6,action,cost=0)
            stop(6)
        }
    }
    else{
        ## close short, hold<0, untrade>0
        hold <- sum(.GlobalEnv$tradingstates$capital$SHORTHOLDINGS[.GlobalEnv$tradingstates$capital$instrumentid==instrumentid])
        nethold <- hold+untrade
        if( (hold==0) | direction==sign(nethold) |
           vol>abs(hold) | vol>abs(nethold) |
           (any(currentinstrument$price==0&currentinstrument$direction==direction&currentinstrument$action%in%c("close",action)) & price==0) ){
            .writeorderhistory(instrumentid,orderid,direction,hands,price,tradeprice=0,status=6,action,cost=0)
            stop(6)
        }
    }
})

## detect timeout orders, must be executed before .orderchaser
.timeoutdetector <- function(tradetime=.GlobalEnv$tradingstates$currenttradetime){
    if(!any(.GlobalEnv$tradingstates$orders$timeoutlist)){
        return()
    }
    timeoutidx <- .GlobalEnv$tradingstates$orders$timeoutlist &
        as.numeric(difftime(tradetime,.GlobalEnv$tradingstates$orders$submitstart),unit="secs")>=.GlobalEnv$tradingstates$orders$timeoutsleep
    ## timeout, chase
    chaseidx <- timeoutidx & .GlobalEnv$tradingstates$orders$timeoutchase
    ## timeout, don't chase
    timeoutidx <- timeoutidx & (!.GlobalEnv$tradingstates$orders$timeoutchase)
    ## chase
    if(any(chaseidx)){
        .GlobalEnv$tradingstates$orders$chaselist[chaseidx] <- TRUE
    }
    ## cancel
    if(any(timeoutidx)){
        cancelall(orderid = .GlobalEnv$tradingstates$orders[timeoutidx])
    }
    return()
}

## support for multiple insstruments
.chasedetector <- function(orders){
    mostrecentorderbook <- list()
    ## get all related order books
    for(instrumentid in unique(orders$instrumentid)){
        mostrecentorderbook[[instrumentid]] <- INSTRUMENT$orderbook[[instrumentid]]
    }
    ## return logical vector indicating wether to chase
    return(
    vapply(X=1:nrow(orders),FUN = function(i){
        return(
        (orders$direction[i]==1 & orders$price[i]!= mostrecentorderbook[[orders$instrumentid[i]]]$buybook$price[1]) |
            (orders$direction[i]==-1 & orders$price[i]!= mostrecentorderbook[[orders$instrumentid[i]]]$sellbook$price[1])
        )
    },FUN.VALUE = TRUE)
    )
}

## chase bid1 or ask1 price
.orderchaser <- function(tradetime=.GlobalEnv$tradingstates$currenttradetime){
    if(!any(.GlobalEnv$tradingstates$orders$chaselist)){
        return()
    }
    
    ## exceed idle time
    idx <- .GlobalEnv$tradingstates$orders$chaselist &
        as.numeric(difftime(tradetime,.GlobalEnv$tradingstates$orders$submitstart),unit="secs")>=.GlobalEnv$tradingstates$orders$chasesleep
    if(!any(idx)){
        return()
    }
    ## timeout orders
    orders <- .GlobalEnv$tradingstates$orders[idx,]
    chaseidx <- .chasedetector(orders)
    if(!any(chaseidx)){
        return()
    }
    orders <- orders[chaseidx,]
    cancelall(orderid = orders$orderid)
    ## automatically submit bid1 or ask1 price when price=NULL
    multisubmission(instrumentid = orders$instrumentid,direction = orders$direction,price=NULL,hands = orders$hands,action = orders$action,chaselist = TRUE,chasesleep=orders$chasesleep)
}

.lazyfunctions <- function(){
    ## setOperators("%c%","%&%","%!%","%==%","%$%")
    setOperators("%c%","%$%")
    ## concatenate stings
    `%c%` <- function(x,y){
        return(paste(as.character(x),as.character(y),sep=""))
    }
    assign("%c%",value = `%c%`,envir = .GlobalEnv)
    ## logicals
    `&` <- function(x,y){
        if(is.expression(x)){
            x <- eval(x)
            x <- ifelse(is.logical(x),x,NA)
        }
        if(is.expression(y)){
            y <- eval(y)
            y <- ifelse(is.logical(y),y,NA)
        }
        .Primitive("&")(x,y)
    }
    assign("&",value = `&`,envir = .GlobalEnv)
    `|` <- function(x,y){
        if(is.expression(x)){
            x <- eval(x)
            x <- ifelse(is.logical(x),x,NA)
        }
        if(is.expression(y)){
            y <- eval(y)
            y <- ifelse(is.logical(y),y,NA)
        }
        .Primitive("|")(x,y)
    }
    assign("|",value = `|`,envir = .GlobalEnv)
    `==` <- function(x,y){
        if(is.expression(x)){
            x <- eval(x)
            ## x <- ifelse(is.logical(x),x,NA)
        }
        if(is.expression(y)){
            y <- eval(y)
            ## y <- ifelse(is.logical(y),y,NA)
        }
        .Primitive("==")(x,y)
    }
    assign("==",value = `==`,envir = .GlobalEnv)
    `!=` <- function(x,y){
        if(is.expression(x)){
            x <- eval(x)
            ## x <- ifelse(is.logical(x),x,NA)
        }
        if(is.expression(y)){
            y <- eval(y)
            ## y <- ifelse(is.logical(y),y,NA)
        }
        .Primitive("!=")(x,y)
    }
    assign("!=",value = `!=`,envir = .GlobalEnv)

    isTRUE <- function(x){
        if(is.expression(x)){
            x <- eval(x)
            x <- ifelse(is.logical(x),x,NA)
        }
        identical(TRUE,x)
    }
    assign("isTRUE",value = `isTRUE`,envir = .GlobalEnv)
    ## use with caution!!!
    `!` <- function(y){
        if(is.expression(y)){
            y <- eval(y)
            y <- ifelse(is.logical(y),y,NA)
        }
        .Primitive("!")(y)
    }
    assign("!",value = `!`,envir = .GlobalEnv)
    
    ANY <- function(...,na.rm=FALSE){
        base:::any(vapply(list(...),function(par){
            if(is.expression(par)){
                par <- eval(par)
                if(!is.logical(par))
                    return(NA)
                else
                    return(par)
            }else{
                if(is.logical(par))
                    return(par)
                else
                    return(NA)
            }
        },FUN.VALUE = TRUE),na.rm = na.rm)
    }
    assign("ANY",value = ANY,envir = .GlobalEnv)
    ALL <- function(...,na.rm=FALSE){
        base:::all(vapply(list(...),function(par){
            if(is.expression(par)){
                par <- eval(par)
                if(!is.logical(par))
                    return(NA)
                else
                    return(par)
            }else{
                if(is.logical(par))
                    return(par)
                else
                    return(NA)
            }
        },FUN.VALUE = TRUE),na.rm = na.rm)
    }
    assign("ALL",value = ALL,envir = .GlobalEnv)    
    ## y must be specified.
    ## example: shortopen%$%"orderid"
    `%$%` <- function(x,y){
        if(is.expression(x))
            x <- eval(x)
        x[[y]]
    }
    assign("%$%",value = `%$%`,envir = .GlobalEnv)
    
}

.furtherlazyfunctions <- function(){
    ## do not support + - * \
    
    ## !!!! may cause trouble!!!!!!!!!!!!!!!!!!!!
    `>=` <- function(x,y){
        if(is.expression(x)){
            x <- eval(x)
        }
        if(is.expression(y)){
            y <- eval(y)
        }
        .Primitive(">=")(x,y)
    }
    assign(">=",value = `>=`,envir = .GlobalEnv)
    `>` <- function(x,y){
        if(is.expression(x)){
            x <- eval(x)
        }
        if(is.expression(y)){
            y <- eval(y)
        }
        .Primitive(">")(x,y)
    }
    assign(">",value = `>`,envir = .GlobalEnv)
    `<=` <- function(x,y){
        if(is.expression(x)){
            x <- eval(x)
        }
        if(is.expression(y)){
            y <- eval(y)
        }
        .Primitive("<=")(x,y)
    }
    assign("<=",value = `<=`,envir = .GlobalEnv)
    `<` <- function(x,y){
        if(is.expression(x)){
            x <- eval(x)
        }
        if(is.expression(y)){
            y <- eval(y)
        }
        .Primitive("<")(x,y)
    }
    assign("<",value = `<`,envir = .GlobalEnv)
    ## !!!! may cause trouble!!!!!!!!!!!!!!!!!!!!
}

.lazyexpressions <- function(instrumentid,ninstruments=NULL,type="specific"){
    match.arg(type,c("specific","general"))
    if(type=="general"){
        if(is.null(ninstruments)){
            stop("ninstruments can't be NULL when type=general!")
        }
        prefix <- paste("instrument",ninstruments,sep = "")
    }
    else if(type=="specific"){
        prefix <- instrumentid
    }
    
    ## orders
    orders.non <- parse(text="nrow(.GlobalEnv$tradingstates$orders[.GlobalEnv$tradingstates$orders$instrumentid==\""%c%instrumentid%c%"\",])==0")
    orders.exist <- parse(text="nrow(.GlobalEnv$tradingstates$orders[.GlobalEnv$tradingstates$orders$instrumentid==\""%c%instrumentid%c%"\",])!=0")
    assign(prefix%c%".orders.non",orders.non,envir = .GlobalEnv)
    assign(prefix%c%".orders.exist",orders.exist,envir = .GlobalEnv)
    ## longopen
    longopen <- parse(text=".GlobalEnv$tradingstates$orders[.GlobalEnv$tradingstates$orders$action==\"open\" & .GlobalEnv$tradingstates$orders$direction==1 &"%c%".GlobalEnv$tradingstates$orders$instrumentid==\""%c%instrumentid%c%"\",]")
    assign(prefix%c%".longopen",longopen,envir = .GlobalEnv)
    longopen.non <- parse(text="nrow(.GlobalEnv$tradingstates$orders[.GlobalEnv$tradingstates$orders$action==\"open\" & .GlobalEnv$tradingstates$orders$direction==1 &"%c%".GlobalEnv$tradingstates$orders$instrumentid==\""%c%instrumentid%c%"\",])==0")
    assign(prefix%c%".longopen.non",longopen.non,envir = .GlobalEnv)
    longopen.exist <- parse(text="nrow(.GlobalEnv$tradingstates$orders[.GlobalEnv$tradingstates$orders$action==\"open\" & .GlobalEnv$tradingstates$orders$direction==1 &"%c%".GlobalEnv$tradingstates$orders$instrumentid==\""%c%instrumentid%c%"\",])!=0")
    assign(prefix%c%".longopen.exist",longopen.exist,envir = .GlobalEnv)
    ## shortopen
    shortopen <- parse(text=".GlobalEnv$tradingstates$orders[.GlobalEnv$tradingstates$orders$action==\"open\"&.GlobalEnv$tradingstates$orders$direction==-1 &"%c%".GlobalEnv$tradingstates$orders$instrumentid==\""%c%instrumentid%c%"\",]")
    assign(prefix%c%".shortopen",shortopen,envir = .GlobalEnv)
    shortopen.non <- parse(text="nrow(.GlobalEnv$tradingstates$orders[.GlobalEnv$tradingstates$orders$action==\"open\"&.GlobalEnv$tradingstates$orders$direction==-1 &"%c%".GlobalEnv$tradingstates$orders$instrumentid==\""%c%instrumentid%c%"\",])==0")
    assign(prefix%c%".shortopen.non",shortopen.non,envir = .GlobalEnv)
    shortopen.exist <- parse(text="nrow(.GlobalEnv$tradingstates$orders[.GlobalEnv$tradingstates$orders$action==\"open\"&.GlobalEnv$tradingstates$orders$direction==-1 &"%c%".GlobalEnv$tradingstates$orders$instrumentid==\""%c%instrumentid%c%"\",])!=0")
    assign(prefix%c%".shortopen.exist",shortopen.exist,envir = .GlobalEnv)
    ## longclose
    longclose <- parse(text=".GlobalEnv$tradingstates$orders[.GlobalEnv$tradingstates$orders$action==\"close\"&.GlobalEnv$tradingstates$orders$direction==1 &"%c%".GlobalEnv$tradingstates$orders$instrumentid==\""%c%instrumentid%c%"\",]")
    assign(prefix%c%".longclose",longclose,envir = .GlobalEnv)
    longclose.non <- parse(text="nrow(.GlobalEnv$tradingstates$orders[.GlobalEnv$tradingstates$orders$action==\"close\"&.GlobalEnv$tradingstates$orders$direction==1 &"%c%".GlobalEnv$tradingstates$orders$instrumentid==\""%c%instrumentid%c%"\",])==0")
    assign(prefix%c%".longclose.non",longclose.non,envir = .GlobalEnv)
    longclose.exist <- parse(text="nrow(.GlobalEnv$tradingstates$orders[.GlobalEnv$tradingstates$orders$action==\"close\"&.GlobalEnv$tradingstates$orders$direction==1 &"%c%".GlobalEnv$tradingstates$orders$instrumentid==\""%c%instrumentid%c%"\",])!=0")
    assign(prefix%c%".longclose.exist",longclose.exist,envir = .GlobalEnv)
    ## shortclose
    shortclose <- parse(text=".GlobalEnv$tradingstates$orders[.GlobalEnv$tradingstates$orders$action==\"close\"&.GlobalEnv$tradingstates$orders$direction==-1 &"%c%".GlobalEnv$tradingstates$orders$instrumentid==\""%c%instrumentid%c%"\",]")
    assign(prefix%c%".shortclose",shortclose,envir=.GlobalEnv)
    shortclose.non <- parse(text="nrow(.GlobalEnv$tradingstates$orders[.GlobalEnv$tradingstates$orders$action==\"close\"&.GlobalEnv$tradingstates$orders$direction==-1 &"%c%".GlobalEnv$tradingstates$orders$instrumentid==\""%c%instrumentid%c%"\",])==0")
    assign(prefix%c%".shortclose.non",shortclose.non,envir=.GlobalEnv)
    shortclose.exist <- parse(text="nrow(.GlobalEnv$tradingstates$orders[.GlobalEnv$tradingstates$orders$action==\"close\"&.GlobalEnv$tradingstates$orders$direction==-1 &"%c%".GlobalEnv$tradingstates$orders$instrumentid==\""%c%instrumentid%c%"\",])!=0")
    assign(prefix%c%".shortclose.exist",shortclose.exist,envir=.GlobalEnv)
    ## holdings
    holdings.exist <- parse(text=".GlobalEnv$tradingstates$capital$totallongholdings[.GlobalEnv$tradingstates$capital$instrumentid==\""%c%instrumentid%c%"\"] >0 | .GlobalEnv$tradingstates$capital$totalshortholdings"%c% "[.GlobalEnv$tradingstates$capital$instrumentid==\""%c%instrumentid%c%"\"]"%c%"<0")
    assign(prefix%c%".holdings.exist",holdings.exist,envir=.GlobalEnv)
    holdings.non <- parse(text=".GlobalEnv$tradingstates$capital$totallongholdings[.GlobalEnv$tradingstates$capital$instrumentid==\""%c%instrumentid%c%"\"] ==0 & .GlobalEnv$tradingstates$capital$totalshortholdings"%c% "[.GlobalEnv$tradingstates$capital$instrumentid==\""%c%instrumentid%c%"\"]"%c%"==0")
    assign(prefix%c%".holdings.non",holdings.non,envir=.GlobalEnv)
    ## longholdings
    longholdings.exist <- parse(text=".GlobalEnv$tradingstates$capital$totallongholdings"%c% "[.GlobalEnv$tradingstates$capital$instrumentid==\""%c%instrumentid%c%"\"]"%c%">0")
    assign(prefix%c%".longholdings.exist",longholdings.exist,envir=.GlobalEnv)
    longholdings.non <- parse(text=".GlobalEnv$tradingstates$capital$totallongholdings"%c% "[.GlobalEnv$tradingstates$capital$instrumentid==\""%c%instrumentid%c%"\"]"%c%"==0")
    assign(prefix%c%".longholdings.non",longholdings.non,envir=.GlobalEnv)
    ## shortholdings
    shortholdings.exist <- parse(text=".GlobalEnv$tradingstates$capital$totalshortholdings"%c% "[.GlobalEnv$tradingstates$capital$instrumentid==\""%c%instrumentid%c%"\"]"%c%"<0")
    assign(prefix%c%".shortholdings.exist",shortholdings.exist,envir=.GlobalEnv)
    shortholdings.non <- parse(text=".GlobalEnv$tradingstates$capital$totalshortholdings"%c% "[.GlobalEnv$tradingstates$capital$instrumentid==\""%c%instrumentid%c%"\"]"%c%"==0")
    assign(prefix%c%".shortholdings.non",shortholdings.non,envir=.GlobalEnv)
}

.cancelorders <- function(orders){
    if(nrow(orders)>0){
        for(i in seq_along(orders$orderid)){
            ordersubmission(instrumentid = orders$instrumentid[i],orderid = orders$orderid[i],action = "cancel")
        }
    }
    return()
}

.tradecenter <- function(instrumentid,tradetime=.GlobalEnv$tradingstates$currenttradetime){
    if(!.GlobalEnv$tradingstates$tc){return()}
    
    if(.GlobalEnv$tradingstates$justchanged[instrumentid] | as.numeric(difftime(tradetime,.GlobalEnv$tradingstates$lastchange[instrumentid]),unit="secs")>=.GlobalEnv$tradingstates$Sleep){
        .GlobalEnv$tradingstates$justchanged[instrumentid] <- FALSE
        .GlobalEnv$tradingstates$lastchange[instrumentid] <- tradetime
        
        ## get orderbook
        orderbook <- INSTRUMENT$orderbook[[instrumentid]]
        if(is.null(orderbook)){return()}
        longholding <- .GlobalEnv$tradingstates$th$longholding[.GlobalEnv$tradingstates$th$instrumentid==instrumentid]
        shortholding <- .GlobalEnv$tradingstates$th$shortholding[.GlobalEnv$tradingstates$th$instrumentid==instrumentid]
        
        currentinstrument <- .GlobalEnv$tradingstates$capital[.GlobalEnv$tradingstates$capital$instrumentid==instrumentid,]
        currentorder <- .GlobalEnv$tradingstates$orders[.GlobalEnv$tradingstates$orders$instrumentid==instrumentid,]


        ## long holdings
        longclose <- currentorder[(currentorder$action=="close"&currentorder$direction==-1),]
        longopen <- currentorder[(currentorder$action=="open"&currentorder$direction==1),]

        ## short holdings
        shortclose <- currentorder[(currentorder$action=="close"&currentorder$direction==1),]
        shortopen <- currentorder[(currentorder$action=="open"&currentorder$direction==-1),]
        
        ## operations on long holdings
        if(!is.null(longholding)){
            if(currentinstrument$totallongholdings<longholding){
                .cancelorders(longclose)
                .cancelorders(longopen[longopen$price!=orderbook$buybook$price[1],])
                if(sum(longopen$hands[longopen$price==orderbook$buybook$price[1]])>longholding-currentinstrument$totallongholdings){
                    .cancelorders(longopen[longopen$price==orderbook$buybook$price[1],])
                    ordersubmission(instrumentid,orderid = randomid(5),direction = 1,
                                    price = orderbook$buybook$price[1],
                                    hands = longholding-currentinstrument$totallongholdings,
                                    action = "open")
                }
                else if(sum(longopen$hands[longopen$price==orderbook$buybook$price[1]])<longholding-currentinstrument$totallongholdings){
                    ordersubmission(instrumentid,orderid = randomid(5),direction = 1,
                                    price = orderbook$buybook$price[1],
                                    hands = longholding-currentinstrument$totallongholdings-sum(longopen$hands[longopen$price==orderbook$buybook$price[1]]),
                                    action = "open")
                }
            }
            else if(currentinstrument$totallongholdings==longholding){
                .cancelorders(longclose)
                .cancelorders(longopen)
            }
            else{
                ## currentinstrument$totallongholdings>longholding
                .cancelorders(longopen)
                .cancelorders(longclose[longclose$price!=orderbook$sellbook$price[1],])
                if(sum(longclose$hands[longclose$price==orderbook$sellbook$price[1]])>currentinstrument$totallongholdings-longholding){
                    .cancelorders(longclose[longclose$price==orderbook$sellbook$price[1],])
                    ordersubmission(instrumentid,orderid = randomid(5),direction = -1,
                                    price = orderbook$sellbook$price[1],
                                    hands = currentinstrument$totallongholdings-longholding,
                                    action = "close")
                }
                else if(sum(longclose$hands[longclose$price==orderbook$sellbook$price[1]])<currentinstrument$totallongholdings-longholding){
                    ordersubmission(instrumentid,orderid = randomid(5),direction = -1,
                                    price = orderbook$sellbook$price[1],
                                    hands = currentinstrument$totallongholdings-longholding-sum(longclose$hands[longclose$price==orderbook$sellbook$price[1]]),
                                    action = "close")
                }
            }
        }
        
        ## operations on short holdings
        if(!is.null(shortholding)){
            if(currentinstrument$totalshortholdings>shortholding){
                .cancelorders(shortclose)
                .cancelorders(shortopen[shortopen$price!=orderbook$sellbook$price[1],])
                if(sum(shortopen$hands[shortopen$price==orderbook$sellbook$price[1]])>currentinstrument$totalshortholdings-shortholding){
                    .cancelorders(shortopen[shortopen$price==orderbook$sellbook$price[1],])
                    ordersubmission(instrumentid,orderid = randomid(5),direction = -1,
                                    price = orderbook$sellbook$price[1],
                                    hands = currentinstrument$totalshortholdings-shortholding,
                                    action = "open")
                }
                else if(sum(shortopen$hands[shortopen$price==orderbook$sellbook$price[1]])<currentinstrument$totalshortholdings-shortholding){
                    ordersubmission(instrumentid,orderid = randomid(5),direction = -1,
                                    price = orderbook$sellbook$price[1],
                                    hands = currentinstrument$totalshortholdings-shortholding-sum(shortopen$hands[shortopen$price==orderbook$sellbook$price[1]]),
                                    action = "open")
                }
            }
            else if(currentinstrument$totalshortholdings==shortholding){
                .cancelorders(shortclose)
                .cancelorders(shortopen)
            }
            else{
                ## currentinstrument$totalshortholdings<shortholding
                .cancelorders(shortopen)
                .cancelorders(shortclose[shortclose$price!=orderbook$buybook$price[1],])
                if(sum(shortclose$hands[shortclose$price==orderbook$buybook$price[1]])>shortholding-currentinstrument$totalshortholdings){
                    .cancelorders(shortclose[shortclose$price==orderbook$buybook$price[1],])
                    ordersubmission(instrumentid,orderid = randomid(5),direction = 1,
                                    price = orderbook$buybook$price[1],
                                    hands = shortholding-currentinstrument$totalshortholdings,
                                    action = "close")
                }
                else if(sum(shortclose$hands[shortclose$price==orderbook$buybook$price[1]])<shortholding-currentinstrument$totalshortholdings){
                    ordersubmission(instrumentid,orderid = randomid(5),direction = 1,
                                    price = orderbook$buybook$price[1],
                                    hands = shortholding-currentinstrument$totalshortholdings-sum(shortclose$hands[shortclose$price==orderbook$buybook$price[1]]),
                                    action = "close")
                }
            }
        }
        
    }
        
    return()
}

## record all prior limit orders' informations.
.verboselimitpriors <- function(tradetime=.GlobalEnv$tradingstates$currenttradetime){
    .GlobalEnv$tradingstates$verbosepriors[[tradetime]] <- .GlobalEnv$tradingstates$limitprior
}

## extract tradetime, lastprice, orderbook, preorderbook and volume from current data flow. update queuing orders and capital state.
.CFEupdate <- .DEFMACRO(DATA,INSTRUMENTID,expr = {
    DATA <- unlist(strsplit(paste(DATA,collapse = ","),split = ","))
    ## extract information
    tradetime <- .extractinfo("tradetime",DATA,ptradetime=INSTRUMENT$ptradetime[[INSTRUMENTID]],timeformat=INSTRUMENT$timeformat[[INSTRUMENTID]])
    ## keep tracking most recent tradetime IMPORTANT
    .GlobalEnv$tradingstates$currenttradetime <- tradetime
    ## interdaily trading-----------------------------------
    if(.GlobalEnv$tradingstates$interdaily){
        ## reset instrument trading start indicator
        .GlobalEnv$tradingstates$startoftheday[INSTRUMENTID] <- FALSE
        HMOS <- .extractinfo("HMOS",DATA,ptradetime=INSTRUMENT$ptradetime[[INSTRUMENTID]],timeformat=INSTRUMENT$timeformat[[INSTRUMENTID]])
        INSTRUMENT$current[[INSTRUMENTID]] <- ifelse(HMOS<=INSTRUMENT$endoftheday[[INSTRUMENTID]],as.numeric(difftime(HMOS,"1970-01-01 00:00:00.000",units = "secs")+INSTRUMENT$tomidnight[[INSTRUMENTID]]),as.numeric(difftime(HMOS,INSTRUMENT$endoftheday[[INSTRUMENTID]],units = "secs")))
        ## new day condition
        if(INSTRUMENT$current[[INSTRUMENTID]]<INSTRUMENT$pre[[INSTRUMENTID]]){
            ## instrument trading start indicator
            .GlobalEnv$tradingstates$startoftheday[INSTRUMENTID] <- TRUE
            ## reset total volume and orderbook
            INSTRUMENT$pretotalvolume <- INSTRUMENT$pretotalvolume[names(INSTRUMENT$pretotalvolume)!=INSTRUMENTID]
            INSTRUMENT$preorderbook <- INSTRUMENT$preorderbook[names(INSTRUMENT$preorderbook)!=INSTRUMENTID]
            IDX <- .GlobalEnv$tradingstates$capital$instrumentid==INSTRUMENTID
            ## move holdings to preholdins
            .GlobalEnv$tradingstates$capital[IDX,c("longholdingspreday","shortholdingspreday")] <- .GlobalEnv$tradingstates$capital[IDX,c("longholdingspreday","shortholdingspreday")]+.GlobalEnv$tradingstates$capital[IDX,c("longholdingstoday","shortholdingstoday")]
            .GlobalEnv$tradingstates$capital[IDX,c("longholdingstoday","shortholdingstoday")] <- c(0,0)
            ## INSTRUMENT$newday[[INSTRUMENTID]] <- FALSE
        }
        INSTRUMENT$pre[[INSTRUMENTID]] <- INSTRUMENT$current[[INSTRUMENTID]]
    }
    ## interdaily trading-----------------------------------
    lastprice <- .extractinfo("lastprice",DATA,plastprice=INSTRUMENT$plastprice[[INSTRUMENTID]])
    INSTRUMENT$lastprice[[INSTRUMENTID]] <- lastprice
    totalvolume <- .extractinfo("volume",DATA,pvolume=INSTRUMENT$pvolume[[INSTRUMENTID]])
    if(! INSTRUMENTID%in%names(INSTRUMENT$pretotalvolume) ){
        INSTRUMENT$pretotalvolume[[INSTRUMENTID]] <- totalvolume
    }
    volume <- totalvolume-INSTRUMENT$pretotalvolume[[INSTRUMENTID]]
    orderbook <- .extractinfo("orderbook",DATA,pbuyhands=INSTRUMENT$pbuyhands[[INSTRUMENTID]],pbuyprice=INSTRUMENT$pbuyprice[[INSTRUMENTID]],psellhands=INSTRUMENT$psellhands[[INSTRUMENTID]],psellprice=INSTRUMENT$psellprice[[INSTRUMENTID]])
    if(! INSTRUMENTID%in%names(INSTRUMENT$preorderbook) ){
        INSTRUMENT$preorderbook[[INSTRUMENTID]] <- orderbook
    }
    INSTRUMENT$orderbook[[INSTRUMENTID]] <- orderbook
    preorderbook <- INSTRUMENT$preorderbook[[INSTRUMENTID]] #might be useful
    
    ## fill settle price for pre unclosed!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if(any(.GlobalEnv$tradingstates$unclosedsettleprice)){
        if(.GlobalEnv$tradingstates$unclosedsettleprice[INSTRUMENTID]){
            presettleprice <- .extractinfo("presettleprice",DATA,ppresettleprice = INSTRUMENT$ppresettleprice[[INSTRUMENTID]])
            idxlong <- .GlobalEnv$tradingstates$unclosedlong$instrumentid==INSTRUMENTID&.GlobalEnv$tradingstates$unclosedlong$tradeprice==0
            if(any(idxlong)){
                .GlobalEnv$tradingstates$unclosedlong$tradeprice[idxlong] <- presettleprice
            }
            idxshort <- .GlobalEnv$tradingstates$unclosedshort$instrumentid==INSTRUMENTID&.GlobalEnv$tradingstates$unclosedshort$tradeprice==0
            if(any(idxshort)){
                .GlobalEnv$tradingstates$unclosedshort$tradeprice[idxshort] <- presettleprice
            }
            .GlobalEnv$tradingstates$unclosedsettleprice[INSTRUMENTID] <- FALSE
        }
    }
    ## fill settle price for pre unclosed!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    ## update states
    .updateinstrument(instrumentid=INSTRUMENTID,lastprice,volume,orderbook,INSTRUMENT$preorderbook[[INSTRUMENTID]],INSTRUMENT$fee[[INSTRUMENTID]],INSTRUMENT$closeprior[[INSTRUMENTID]],multiplier=INSTRUMENT$multiplier[[INSTRUMENTID]])
    ## save as previous values
    INSTRUMENT$pretotalvolume[[INSTRUMENTID]] <- totalvolume
    INSTRUMENT$preorderbook[[INSTRUMENTID]] <- orderbook
    ## some automatic functions
    .timeoutdetector()
    .orderchaser()
    .tradecenter(INSTRUMENTID)
})
