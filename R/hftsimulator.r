
options(digits.secs=DIGITSSECS)
options(stringsAsFactors = STRINGSASFACTORS)


.tradingstates <- new.env(hash = TRUE)
.tradingstates$orders <- data.frame(
    instrumentid=character(),
    orderid=character(),direction=numeric(),
    price=numeric(),hands=numeric(),
    action=character(),
    initialhands=numeric(),
    timeoutlist=logical(),          #wether to check timeout
    timeoutchase=logical(),         #wether to chase after timeout
    timeoutsleep=numeric(),          #length of timeout,in secs
    chaselist=logical(),            #wether to chase
    chasesleep=numeric(),           #length of chase sleep time,secs
    submitstart=character(),        #chase or timeout start time
    stringsAsFactors=FALSE)
.tradingstates$limitprior <- NULL    #high prior limit orders
.tradingstates$capital <- data.frame(
    instrumentid=character(),
    longholdingstoday=numeric(), shortholdingstoday=numeric(),
    longholdingspreday=numeric(),shortholdingspreday=numeric(),
    totallongholdings=numeric(),totalshortholdings=numeric(),
    cash=numeric(),stringsAsFactors=FALSE
)
.tradingstates$tc <- logical(1)              #trade center?
## target holding of trade center
.tradingstates$th <- data.frame(instrumentid=character(),longholding=numeric(),
                               shortholding=numeric(),stringsAsFactors = FALSE)

## write history to memory
.tradingstates$orderhistory <- data.frame(
    instrumentid=character(),orderid=character(),
    direction=numeric(),price=numeric(),
    hands=numeric(),action=character(),
    tradetime=character(),tradeprice=numeric(),
    cost=numeric(),status=numeric(),
    initialhands=numeric(),
    stringsAsFactors = FALSE)
.tradingstates$capitalhistory <- data.frame(
    instrumentid=character(),
    longholdingstoday=numeric(), shortholdingstoday=numeric(),
    longholdingspreday=numeric(),shortholdingspreday=numeric(),
    totallongholdings=numeric(),totalshortholdings=numeric(),
    cash=numeric(),tradetime=character(),
    tradeprice=numeric(),tradehands=numeric(),cost=numeric(),
    stringsAsFactors=FALSE)

## save seprated traded order history when septraded=TRUE
.tradingstates$septraded <- logical(1)
.tradingstates$longopen <- data.frame(
    instrumentid=character(),orderid=character(),
    action=character(),
    direction=numeric(),
    tradehands=numeric(),
    tradeprice=numeric(),
    stringsAsFactors = FALSE)
.tradingstates$shortclose <- .tradingstates$longopen
.tradingstates$shortopen <- .tradingstates$longopen
.tradingstates$shortclose <- .tradingstates$longopen

## current time
.tradingstates$currenttradetime <- character()

## interdaily or not
.tradingstates$interdaily <- logical(1)
.tradingstates$startoftheday <- logical(1)

## verbose
.tradingstates$verbosepriors <- NULL

## trade center invoke tag and sleep recorder
.tradingstates$justchanged <- NULL
.tradingstates$lastchange <- NULL
.tradingstates$Sleep <- numeric(1)

## instrument-closeprofit tracker
.tradingstates$closed <- logical(1)
.tradingstates$closedtracker <- data.frame(instrumentid=character(),cash=numeric(),stringsAsFactors=FALSE)

## track unclosed orders
.tradingstates$unclosed <- logical(1)
.tradingstates$unclosedlong <- .tradingstates$longopen
.tradingstates$unclosedshort <- .tradingstates$longopen
.tradingstates$unclosedsettleprice <- logical(1)


.INSTRUMENT  <- new.env(hash = TRUE)

.INSTRUMENT$instrumentid <- list()

.INSTRUMENT$pbuyhands <- list()
.INSTRUMENT$pbuyprice <- list()
.INSTRUMENT$psellhands <- list()
.INSTRUMENT$psellprice <- list()
.INSTRUMENT$ptradetime <- list()
.INSTRUMENT$plastprice <- list()
.INSTRUMENT$pvolume <- list()
.INSTRUMENT$ppresettleprice <- list()

.INSTRUMENT$pbuyhands <- list()
.INSTRUMENT$pbuyprice <- list()
## sellbook:
.INSTRUMENT$psellhands <- list()
.INSTRUMENT$psellprice <- list()

.INSTRUMENT$ptradetime <- list()
.INSTRUMENT$plastprice <- list()
.INSTRUMENT$pvolume <- list()
.INSTRUMENT$ppresettleprice <- list()


## temp variables and user specified parameters
.INSTRUMENT$pretotalvolume <- list()
.INSTRUMENT$orderbook <- list()
.INSTRUMENT$preorderbook <- list()
.INSTRUMENT$lastprice <- list()    #holdings profit

.INSTRUMENT$fee <- list()
.INSTRUMENT$closeprior <- list()
.INSTRUMENT$fee <- list()
.INSTRUMENT$closeprior <- list()

## time format
.INSTRUMENT$timeformat <- list()

## end of the day
.INSTRUMENT$endoftheday <- list()
.INSTRUMENT$tomidnight <- list()

## face value per hand
.INSTRUMENT$multiplier <- list()

## parameters for interdaily trading
.INSTRUMENT$pre <- list()
.INSTRUMENT$current <- list()


##' queryorder
##'
##' queryorder
##' @title queryorder
##' @param orderid orderid to be queried, return all orders if orderid=NULL
##' @return order
##' @author Chen
##'
queryorder <- function(orderids=NULL){
    if(is.null(orderids))
        return(.tradingstates$orders)
    else
        return(.tradingstates$orders[.tradingstates$orders$orderid%in%orderids,])
}

##' querycapital
##' rycapital
##'
##' querycapital
##' @title querycapital
##' @param instrumentids instrumentids to be queried, return all orders if instrumentids=NULL
##' @return instruments
##' @author Chen
##'
querycapital <- function(instrumentids=NULL){
    if(!is.null(instrumentids))
        return(subset(.tradingstates$capital,instrumentid%in%instrumentids))
    else
        return(.tradingstates$capital)
}

##' @title ordersubmission
##'  take one of the following order actions, including open, close, closetoday, closepreday and cancel.
##' @details ordersubmission submit an order specified by the user, it also take some additional actions after the submission. For example, if set timeoutlist=TRUE and timeoutsleep=1, the simulator will first submit an order and cancel it if it hasn't been executed in the next second.
##' @seealso \link{multisubmission} \link{timeoutchasesubmission} \link{timeoutsubmission} \link{chasesubmission}
##' @param instrumentid character, instrument identifier
##' @param orderid character, specifying an unique order id, can be generated by randomid()
##' @param direction integer, specifying trading direction. 1 for long, -1 for short.
##' @param price numeric, specifiying order pirce.NOTE: when price=0, ordersubmission() will submit a market order; when price=NULL, ordersubmission() will take the corresponding bid1 or ask1 price as submitted price.
##' @param hands integer, specifying amount to be submitted.
##' @param action character, specifying submit action, action can take value from one of "open","close","closetoday","closepreday" and "cancel". amout submitted in action='close' can not be greater than the sum of current holdings and queuing open hands.
##' @param timeoutlist logical, specyfing wether to give current order a timeout interval, the length of the interval is specified by timeoutsleep. if the order hasn't been executed after a time interval greater than timeoutsleep, the order will be canceled.
##' @param timeoutsleep numeric, specifying the timeout length in seconds.
##' @param chaselist logical, specifying wether to put this order to auto-chase list. if the order hasn' been executed for a time inverval longer than chasesleep, the simulator will cancel this order(if needed), then submit a new one with the sampe hands and a price equal to the bid1/ask1 price. the simulator will repeat this action until the original submitted amount is executed.
##' @param chasesleep numeric, specifying the time interval between each execution check. In seconds.
##' @param 
##' @return order status
##' @export
##'
ordersubmission <- function(instrumentid="TF1603",orderid=NULL,direction=1,price=0,hands=1,action="open",timeoutlist=FALSE,timeoutchase=FALSE,timeoutsleep=1,chaselist=FALSE,chasesleep=1){

    tradetime=.tradingstates$currenttradetime
    
    match.arg(action,choices = c("open","close","closetoday","closepreday","cancel"))
    if(is.null(orderid) | is.null(instrumentid)){
        stop("orderid and instrumentid can not be NULL!")
    }

    ## cancel order
    if(action=="cancel"){
        canceledorder <- .tradingstates$orders[.tradingstates$orders$orderid==orderid,]
        .tradingstates$orders <- .tradingstates$orders[.tradingstates$orders$orderid!=orderid,]
        .writeorderhistory(instrumentid,orderid,canceledorder$direction,canceledorder$hands,canceledorder$price,tradeprice=0,status=5,action,cost=0)
        return(5)
    }
    
    if(any(c(hands%%1!=0, hands<=0, isTRUE(price<0) , !(direction%in%c(-1,1))))){
        stop("illegal parameter values!")
    }

    ## special requirements when action!=cancel
    ## get most recent orderbook
    mostrecentorderbook <- .INSTRUMENT$orderbook[[instrumentid]]
    ## submist bid1 or ask1 when price=NULL
    if(is.null(price)){
        price <- ifelse(direction==1,mostrecentorderbook$buybook$price[1],mostrecentorderbook$sellbook$price[1])
    }
    
    ## tmp file, used to update order state
    orders <- .tradingstates$orders
    currentinstrument <- orders[orders$instrumentid==instrumentid,]
    if(orderid%in%currentinstrument$orderid){
        stop("orderid already exists!")
    }
    if(action=="open"){
        ## only one market order is allowed in each position
        if(any(currentinstrument$price==0&currentinstrument$direction==direction&currentinstrument$action=="open") & price==0){
            .writeorderhistory(instrumentid,orderid,direction,hands,price,tradeprice=0,status=6,action,cost=0)
            stop(6)
        }
        orders <- rbind(orders,data.frame(instrumentid=instrumentid,orderid=orderid,direction=direction,price=price,hands=hands,action=action,initialhands=hands,timeoutlist=timeoutlist,timeoutchase=timeoutchase,timeoutsleep=timeoutsleep,chaselist=chaselist,chasesleep=chasesleep,submitstart=tradetime,stringsAsFactors=FALSE))
        ## save prior orders
        if(price>0){
            .priororders(mostrecentorderbook = mostrecentorderbook,orderid = orderid,direction = direction,price=price)
        }
        .tradingstates$orders <- orders
        .writeorderhistory(instrumentid,orderid,direction,hands,price,tradeprice=0,status=3,action,cost=0)
        return(3)
    }
    else if(action=="close"){
        ## untrade closes
        untrade <- sum(currentinstrument$hands[currentinstrument$direction==direction&currentinstrument$action%in%c("close","closepreday","closetoday")])*direction #untrade(long)<0, untrade(short)>0
        .sucker(totallongholdings,totalshortholdings)

        orders <- rbind(orders,data.frame(instrumentid=instrumentid,orderid=orderid,direction=direction,price=price,hands=hands,action=action,initialhands=hands,timeoutlist=timeoutlist,timeoutchase=timeoutchase,timeoutsleep=timeoutsleep,chaselist=chaselist,chasesleep=chasesleep,submitstart=tradetime,stringsAsFactors=FALSE))
        
        if(price>0)
            .priororders(mostrecentorderbook = mostrecentorderbook,orderid = orderid,direction = direction,price=price)

        .tradingstates$orders <- orders
        .writeorderhistory(instrumentid,orderid,direction,hands,price,tradeprice=0,status=3,action,cost=0)
        return(3)
    }
    else if(action=="closetoday"){
        ## untrade closes
        untrade <- sum(currentinstrument$hands[currentinstrument$direction==direction&currentinstrument$action%in%c("close","closetoday")])*direction
        .sucker(longholdingstoday,shortholdingstoday)

        orders <- rbind(orders,data.frame(instrumentid=instrumentid,orderid=orderid,direction=direction,price=price,hands=hands,action=action,initialhands=hands,timeoutlist=timeoutlist,timeoutchase=timeoutchase,timeoutsleep=timeoutsleep,chaselist=chaselist,chasesleep=chasesleep,submitstart=tradetime,stringsAsFactors=FALSE))
        if(price>0)
            .priororders(mostrecentorderbook = mostrecentorderbook,orderid = orderid,direction = direction,price=price)

        .tradingstates$orders <- orders
        .writeorderhistory(instrumentid,orderid,direction,hands,price,tradeprice=0,status=3,action,cost=0)
        return(3)
    }
    else{
        ## closepreday
        ## untrade closes
        untrade <- sum(currentinstrument$hands[currentinstrument$direction==direction&currentinstrument$action%in%c("close","closepreday")])*direction
        .sucker(longholdingspreday,shortholdingspreday)

        orders <- rbind(orders,data.frame(instrumentid=instrumentid,orderid=orderid,direction=direction,price=price,hands=hands,action=action,initialhands=hands,timeoutlist=timeoutlist,timeoutchase=timeoutchase,timeoutsleep=timeoutsleep,chaselist=chaselist,chasesleep=chasesleep,submitstart=tradetime,stringsAsFactors=FALSE))
        if(price>0)
            .priororders(mostrecentorderbook = mostrecentorderbook,orderid = orderid,direction = direction,price=price)

        .tradingstates$orders <- orders
        .writeorderhistory(instrumentid,orderid,direction,hands,price,tradeprice=0,status=3,action,cost=0)
        return(3)
    }
}

##' multisubmission
##' 
##'  submit multiple orders, a simple wrapper of ordersubmission(). instrumentid, direction, price, hands and action must be of length one or the same length with the number of orders; orderid must be of length zero or the same length with the number of orders!
##'
##' @seealso \link{ordersubmission} \link{timeoutchasesubmission} \link{timeoutsubmission} \link{chasesubmission}
##' @param instrumentid character, instrument identifier
##' @param orderid character, if length(orderid)==0 (default), multisubmission will generate a random id for each order
##' @param direction integer, specifying trading direction. 1 for long, -1 for short.
##' @param price numeric, specifiying order pirce.default NULL. NOTE: when price=0, ordersubmission() will submit a market order; when price=NULL, ordersubmission() will take the corresponding bid1 or ask1 price as order price.
##' @param hands integer, specifying hands to be submitted.
##' @param action character, action can take value from one of "open","close","closetoday","closepreday" and "cancel". hands submitted in action='close' can not be greater than the sum of current holdings and queuing open hands.
##' @return order status
##' @export
##' @examples
##'\dontrun{
##' ## submit leng(orderbook$buybook$price[-1]) long open orders in TF1512.
##' multisubmission(instrumentid="TF1512",orderid=NULL,direction=1,price=orderbook$buybook$price[-1],hands=1,action='open')
##' }
##' @author Chen
##'
multisubmission <- function(instrumentid="qtid",orderid=NULL,direction=1,price=NULL,hands=1,action="open",timeoutlist=FALSE,timeoutchase=FALSE,timeoutsleep=1,chaselist=FALSE,chasesleep=1){
    ## multiple orders
    tryCatch(expr={
        ## special effects when price=NULL
        if(is.null(price)){
            if(length(orderid)==0){
                orders <- data.frame(instrumentid=instrumentid,direction=direction,hands=hands,action=action,timeoutlist=timeoutlist,timeoutchase=timeoutchase,timeoutsleep=timeoutsleep,chaselist=chaselist,chasesleep=chasesleep,stringsAsFactors = FALSE)
                orderids <- NULL
                for(i in 1:nrow(orders)){orderids <- c(orderids,randomid(5))}
                orders$orderid <- orderids
            }
            else{
                orders <- data.frame(instrumentid=instrumentid,orderid=orderid,direction=direction,hands=hands,action=action,timeoutlist=timeoutlist,timeoutchase=timeoutchase,timeoutsleep=timeoutsleep,chaselist=chaselist,chasesleep=chasesleep,stringsAsFactors = FALSE)
            }
        }
        else{
            ## price is not null
            if(length(orderid)==0){
                orders <- data.frame(instrumentid=instrumentid,direction=direction,price=price,hands=hands,action=action,timeoutlist=timeoutlist,timeoutchase=timeoutchase,timeoutsleep=timeoutsleep,chaselist=chaselist,chasesleep=chasesleep,stringsAsFactors = FALSE)
                orderids <- NULL
                for(i in 1:nrow(orders)){orderids <- c(orderids,randomid(5))}
                orders$orderid <- orderids
            }
            else{
                orders <- data.frame(instrumentid=instrumentid,orderid=orderid,direction=direction,price=price,hands=hands,action=action,timeoutlist=timeoutlist,timeoutchase=timeoutchase,timeoutsleep=timeoutsleep,chaselist=chaselist,chasesleep=chasesleep,stringsAsFactors = FALSE)
            }
        }
    },
             warning=function(w){stop("instrumentid, direction, price, hands action timeoutlist, timeoutchase, timeoutsleep, chaselist and chasesleep must be of length one or the same length with the number of orders!! orderid must be of length zero or the same length with the number of orders!")},
             error=function(e){stop("instrumentid, direction, price, hands action timeoutlist, timeoutchase, timeoutsleep, chaselist and chasesleep must be of length one or the same length with the number of orders!! orderid must be of length zero or the same length with the number of orders!")})
    
    for(i in 1:nrow(orders)){
        ordersubmission(instrumentid = orders$instrumentid[i],
                        orderid = orders$orderid[i],direction = orders$direction[i],
                        price=orders$price[i],hands = orders$hands[i],action = orders$action[i],
                        timeoutlist=orders$timeoutlist[i],
                        timeoutchase=orders$timeoutchase[i],
                        timeoutsleep=orders$timeoutsleep[i],
                        chaselist=orders$chaselist[i],
                        chasesleep=orders$chasesleep[i])
    }
    return()
}

##' timeoutsubmission
##' 
##'  submit an order with timeout checking. The order will be canceled when it hasn't been executed for a duration longer than 'timeoutsleep'
##'
##' @seealso \link{multisubmission} \link{timeoutchasesubmission} \link{ordersubmission} \link{chasesubmission}
##' @param instrumentid character, instrument identifier
##' @param orderid character, unique order id, can be generated by randomid()
##' @param direction integer, specifying trading direction. 1 for long, -1 for short.
##' @param price numeric, specifiying order pirce.NOTE: when price=0, ordersubmission() will submit a market order; when price=NULL, ordersubmission() will take the corresponding bid1 or ask1 price as order price.
##' @param hands integer, specifying hands to be submitted.
##' @param action character, action can take value from one of "open","close","closetoday","closepreday" and "cancel". hands submitted in action='close' can not be greater than the sum of current holdings and queuing open hands.
##' @param timeoutsleep numeric, specifying time out length, seconds
##' @return order status
##' @export
##' @author Chen
##'
timeoutsubmission <- function(instrumentid="qtid",orderid=NULL,direction=1,price=0,hands=1,action="open",timeoutsleep=1){
    if(missing(timeoutsleep)){
        warning("'timeoutsleep' not found! set to 1")
    }
    ordersubmission(instrumentid=instrumentid,
                    orderid=orderid,
                    direction=direction,
                    price=price,hands=hands,
                    action=action,
                    timeoutlist=TRUE,
                    timeoutsleep=timeoutsleep)
    return()
}

##' chasesubmission
##' 
##'  chase bid1 or ask1. after every 'chasesleep' seconds, order chaser will check wether current order's price equals to bid1 or ask1 price, if not, order chaser will replace it with a new one satisfying the price condition.
##'
##' @seealso \link{multisubmission} \link{timeoutchasesubmission} \link{ordersubmissionsubmission} \link{chasesubmission}
##' @param instrumentid character, instrument identifier
##' @param orderid character, unique order id, can be generated by randomid()
##' @param direction integer, specifying trading direction. 1 for long, -1 for short.
##' @param price numeric, specifiying order pirce.NOTE: when price=0, ordersubmission() will submit a market order; when price=NULL, ordersubmission() will take the corresponding bid1 or ask1 price as order price.
##' @param hands integer, specifying hands to be submitted.
##' @param action character, action can take value from one of "open","close","closetoday","closepreday" and "cancel". hands submitted in action='close' can not be greater than the sum of current holdings and queuing open hands.
##' @param chasesleep numeric, specifying order chaser idle time
##' @return order status
##' @export
##' @author Chen
##'
chasesubmission <- function(instrumentid="qtid",orderid=NULL,direction=1,price=0,hands=1,action="open",chasesleep=1){
    if(missing(chasesleep)){
        warning("'chasesleep' not found! set to 1")
    }
    ordersubmission(instrumentid=instrumentid,
                    orderid=orderid,
                    direction=direction,
                    price=price,hands=hands,
                    action=action,
                    chaselist = TRUE,
                    chasesleep=chasesleep)
    return()
}

##' timeoutchasesubmission
##' 
##'  submit an order with timeout checking, chase bid1 or ask1 price to execute it when timeout. type ?timeoutsubmission and ?chasesubmission for more information
##'
##' @seealso \link{multisubmission} \link{ordersubmission} \link{timeoutsubmission} \link{chasesubmission}
##' @param instrumentid character, instrument identifier
##' @param orderid character, unique order id, can be generated by randomid()
##' @param direction integer, specifying trading direction. 1 for long, -1 for short.
##' @param price numeric, specifiying order pirce.NOTE: when price=0, ordersubmission() will submit a market order; when price=NULL, ordersubmission() will take the corresponding bid1 or ask1 price as order price.
##' @param hands integer, specifying hands to be submitted.
##' @param action character, action can take value from one of "open","close","closetoday","closepreday" and "cancel". hands submitted in action='close' can not be greater than the sum of current holdings and queuing open hands.
##' @param timeoutsleep numeric, specifying time out length, seconds
##' @param chasesleep numeric, specifying order chaser idle time
##' @return order status
##' @seealso \link{timeoutsubmission} \link{chasesubmission}
##' @export
##' @author Chen
##'
timeoutchasesubmission <- function(instrumentid="qtid",orderid=NULL,direction=1,price=0,hands=1,action="open",timeoutsleep=1,chasesleep=1){
    if(missing(timeoutsleep)){
        warning("'timeoutsleep' not found! set to 1")
    }
    if(missing(chasesleep)){
        warning("'chasesleep' not found! set to 1")
    }
    ordersubmission(instrumentid=instrumentid,
                    orderid=orderid,
                    direction=direction,
                    price=price,hands=hands,
                    action=action,
                    timeoutlist = TRUE,timeoutchase = TRUE,
                    timeoutsleep=timeoutsleep,
                    chasesleep=chasesleep)
    return()
}


##' meanopen
##' 
##'  calculate unclosed mean open price for a specific instrument and holdings side
##' 
##' @param instrumentid character, instrument identifier
##' @param side character, "long" or "short", specifying holdings's side
##' @return mean open price, numeric
##' @details mean open will calculate mean price according to following rules: 1. earlier open orders are prior to be closed. 2. return volume weighted mean of unclosed order's tradeprice.
##'
##' @seealso \link{initializestates}
##' @export
##' @examples
##'\dontrun{
##' ## check long holdings' mean open price of TF1603
##' meanopen("TF1603","long")
##' }
##' @author Chen
##' 
meanopen <- function(instrumentid=character(),side="long"){
    match.arg(side,c("long","short"))
    if(side=="long"){
        IDX <- .tradingstates$unclosedlong$instrumentid==instrumentid
        if(nrow(.tradingstates$unclosedlong[IDX,])==0){
            return(NULL)
        }
        else{
            return(sum(.tradingstates$unclosedlong$tradeprice[IDX]*.tradingstates$unclosedlong$tradehands[IDX])/sum(.tradingstates$unclosedlong$tradehands[IDX]))
        }
    }
    else{
        IDX <- .tradingstates$unclosedshort$instrumentid==instrumentid
        if(nrow(.tradingstates$unclosedshort[IDX,])==0){
            return(NULL)
        }
        else{
            return(sum(.tradingstates$unclosedshort$tradeprice[IDX]*.tradingstates$unclosedshort$tradehands[IDX])/sum(.tradingstates$unclosedshort$tradehands[IDX]))
        }
    }
}

##' holdings profit
##' 
##'  calculate unclosed holdings' dynamic profit. require setting unclosed=TRUE in initializestates()
##' 
##' @param instrumentid character, instrument identifier
##' @param side character, "long" or "short", specifying holdings's side
##' @return holdings profit, numeric
##' @details long holdings' dynamic profit = holdings * (lastprice - mean open price);    short holdings' dynamic profit = holdings * (mean open price - lastprice)
##'
##' @seealso \link{initializestates} \link{meanopen} \link{closedprofit}
##' @export
##' @examples
##'\dontrun{
##' holdingsprofit("TF1603","long")
##' }
##' @author Chen
##' 
holdingsprofit <- function(instrumentid=character(),side="long"){
    MEANOPEN <- meanopen(instrumentid,side)
    if(is.null(MEANOPEN)){return(0)}
    lastprice <- .INSTRUMENT$lastprice[[instrumentid]]
    multiplier <- .INSTRUMENT$multiplier[[instrumentid]]
    ## get holdings
    HOLDINGS <- ifelse(side=="long",.tradingstates$capital$totallongholdings[.tradingstates$capital$instrumentid==instrumentid],.tradingstates$capital$totalshortholdings[.tradingstates$capital$instrumentid==instrumentid])

    return(HOLDINGS*(lastprice-MEANOPEN)*multiplier)
}

##' closed profit
##' 
##'  calculate closed profit. require setting closed=TRUE in initializestates()
##' 
##' @param instrumentid character, instrument identifier
##' @return closed profit, numeric
##' @details closed profit is the most recent cash value when all holdings are equal to zero
##'
##' @seealso \link{initializestates} \link{holdingsprofit}
##' @export
##' @examples
##'\dontrun{
##' closedprofit("TF1603")
##' }
##' @author Chen
##' 
closedprofit <- function(instrumentid){
    return(.tradingstates$closedtracker$cash[.tradingstates$closedtracker$instrumentid==instrumentid])
}

##' initializeinstrument
##'
##'  initialize instruments to be traded, including data structure, fee for different actions, data time formant, corresponding multipliers and initial holdings. if you have already set IMLAZY=TRUE in initializestates(), initializeinstrument() will generate plenty of additional objects in order to simplify strategy writting procedure,see 'Details' below.
##' 
##' @param instrument character, specifying the name of a generated environment, all details about the instruments interested will be stored in this environment. see 'Examples' below.
##' @param instrumentid character, instrument identifier, unique.
##' @param pXXX integer, specifying index of XXX in 'data source', see 'Details' and 'Examples' for more about 'data source'.
##' @param fee named numeric, specifying conmissions of different actions, including open, close, closetoday and closepreday. 'cost' in orderhistory and capitalhistory are result calculated by 'fee'. 
##' @param closeprior character, specifying close priority when specified action='close' in ordersubmission. closeprior can only be one of 'today' and 'preday'. when closeprior='today', ordersubmission will close today's holdings prior than previous days', vise versa. type ?ordersubmission for more details.
##' @param multiplier numeric, quoted price * multiplier = real price.
##' @param timeformat character, specifying time format of the data source.
##' @param endoftheday character, specifying ending time of each trading day, simulator will move holdings to preholdings whenever new tradetime pass through 'endoftheday'.
##' @return an envrionment in .GlobalEnv containing all the informations specified above. the env's name is specified by parameter 'instrument'.
##' @details IMLAZY: if IMLAZY=TRUE, initializeinstrument() will generate a bunch of expressions named INSTRUMENTID.ATTRIBUTE.STATE or INSTRUMENTID.ATTRIBUTE. for example, TF1603.longopen.non represents 'TF1603 has no long open order in queue' and TF1603.longopen represents 'all TF1603's long open orders', you can retrive prices of all the long open orders of TF1603 by typing TF1603.longopenX$X'price'. see 'Examples' for more information. part of the expressions are listed below:
##'          INSTRUMENTID.orders.non
##'          INSTRUMENTID.orders.exists
##'          INSTRUMENTID.longopen.exists
##'          INSTRUMENTID.longopen.non
##'          INSTRUMENTID.shortopen.exists
##'          INSTRUMENTID.shortopen.non
##'          INSTRUMENTID.longclose.exists
##'          INSTRUMENTID.longclose.non
##'          INSTRUMENTID.shortclose.exists
##'          INSTRUMENTID.shortclose.non
##'          INSTRUMENTID.longholdings.exists
##'          INSTRUMENTID.longholdings.non
##'          INSTRUMENTID.shortholdings.exists
##'          INSTRUMENTID.shortholdings.non
##'          INSTRUMENTID.holdings.exists
##'          INSTRUMENTID.holdings.non
##'          INSTRUMENTID.longopen
##'          INSTRUMENTID.shortopen
##'          INSTRUMENTID.longclose
##'          INSTRUMENTID.shortclose
##'
##' data source: any kind of data sorce that can pass market data to strategy and simulator one row at a time.
##' @seealso \link{initializestates} \link{lazysubmission} \link{ordersubmission}
##' @export
##' @examples
##'\dontrun{
##' ## wirte capital and order histories to local file, don't sychronize target holdings, don't use simulated trade center and tell the simulator 'I'm lazy'.
##' initializestates(realtime=FALSE,writeholding=FALSE,tc=FALSE,IMLAZY = TRUE)
##'
##' ## generate an environment named 'TF', the instrument to be traded is 'TF1512'
##' initializeinstrument(instrument = "TF",instrumentid="TF1512",pbuyhands = seq(from = 32,by = 1,length.out = 5),
##'                      pbuyprice = seq(from = 22,by = 1,length.out = 5),
##'                      psellhands = seq(from = 37,by = 1,length.out = 5),
##'                      psellprice = seq(from = 27,by = 1,length.out = 5),
##'                      ptradetime = 2,plastprice = 4,pvolume = 12,
##'                      fee = c(long=0,short=0,closetoday=0,closepreday=0),
##'                      closeprior = "today",
##'                      timeformat = "%Y-%m-%d %H:%M:%S",
##'                      multiplier = 10000)
##'
##' TF1512 <- function(EXdata){
##'  CFEupdate(EXdata,TF,'TF1512')
##' 
##'  ## IMLAZY=TRUE
##'  if(TF1512.holdings.non & TF1512.longopen.non)
##'    ordersubmission(tradetime,"TF1512",orderid="xxx",direction=1,price=0,hands=1,action='open')
##' 
##'  ## IMLAZY=FALSE, as a comparison
##'  ##mycapital <- querycapital(instrumentid="TF1512")
##'  ##myorders <- queryorders()
##'  ##if(mycapital$totallongholdings==0 & mycapital$totalshortholdings=0 & nrow(myorders[myorders$instrumentid=='TF1512'&myorders$direction==1&myorders$action=='open',])==0)
##'  ## ordersubmission(tradetime,"TF1512",orderid="xxx",direction=1,price=0,hands=1,action='open')
##' }
##'
##' ## data srouce
##' datasource <- getHF_Future(instID = "TF1512", startDate = "2015-10-29", endDate = "2015-10-29")
##' ## backtest
##' for(i in 1:nrow(datasource)){TF1512(datasource[i,])}
##' 
##' }
##' @author Chen
##'
initializeinstrument <- function(instrumentid,pbuyhands,pbuyprice,psellhands,psellprice,ptradetime,plastprice,pvolume,ppresettleprice,fee=c(long=0,short=0,closetoday=0,closepreday=0),closeprior="today",timeformat="%Y%m%d%H%M%OS",endoftheday="15:15:00.000",multiplier=10000){

    ## IMPORTANT NOTE:
    ## initialize only one instrument at a time!
    ## run initializeinstrument() multiple times for multiple instruments
    
    ## !!!!!!!!!
    CASH <- 0
    
    ## initialize instrument
    
    .INSTRUMENT$instrumentid[[instrumentid]] <- instrumentid
    
    .INSTRUMENT$pbuyhands[[instrumentid]] <- pbuyhands
    .INSTRUMENT$pbuyprice[[instrumentid]] <- pbuyprice
    ## sellbook:
    .INSTRUMENT$psellhands[[instrumentid]] <- psellhands
    .INSTRUMENT$psellprice[[instrumentid]] <- psellprice
    
    .INSTRUMENT$ptradetime[[instrumentid]] <- ptradetime
    .INSTRUMENT$plastprice[[instrumentid]] <- plastprice
    .INSTRUMENT$pvolume[[instrumentid]] <- pvolume
    .INSTRUMENT$ppresettleprice[[instrumentid]] <- ppresettleprice
    
    .INSTRUMENT$fee[[instrumentid]] <- fee
    .INSTRUMENT$closeprior[[instrumentid]] <- closeprior
    
    .INSTRUMENT$timeformat[[instrumentid]] <- timeformat
    
    .INSTRUMENT$endoftheday[[instrumentid]] <- paste("1970-01-01",endoftheday)
    .INSTRUMENT$tomidnight[[instrumentid]] <- difftime("1970-01-02 00:00:00.000",.INSTRUMENT$endoftheday[[instrumentid]],units = "secs")
    
    .INSTRUMENT$multiplier[[instrumentid]] <- multiplier
    
    .INSTRUMENT$pre[[instrumentid]] <- 0
    .INSTRUMENT$current[[instrumentid]] <- 0
    
    ## new day tracker
    .tradingstates$startoftheday[instrumentid] <- FALSE
    
    ## add zero holding tracker
    .tradingstates$closedtracker <- unique(rbind(
        .tradingstates$closedtracker,
        data.frame(instrumentid=instrumentid,cash=CASH,stringsAsFactors=FALSE)
    ))
    
    
    ## initialize trade center
    .tradingstates$justchanged[instrumentid] <- FALSE
    .tradingstates$lastchange[instrumentid] <- "1970-01-01 00:00:01.300"
    
    ## initialize instrument capital
    if(nrow(.tradingstates$capital[.tradingstates$capital$instrumentid==instrumentid,])==0){
        .tradingstates$capital <- rbind(
            .tradingstates$capital,
            data.frame(
                instrumentid=instrumentid,
                longholdingstoday=0, shortholdingstoday=0,
                longholdingspreday=0,shortholdingspreday=0,
                totallongholdings=0,totalshortholdings=0,
                cash=CASH,stringsAsFactors=FALSE
                )
            )
    }
    else{
        .tradingstates$capital$longholdingstoday[.tradingstates$capital$instrumentid==instrumentid] <- 0
        .tradingstates$capital$shortholdingstoday[.tradingstates$capital$instrumentid==instrumentid] <- 0
        .tradingstates$capital$longholdingspreday[.tradingstates$capital$instrumentid==instrumentid] <- 0
        .tradingstates$capital$shortholdingspreday[.tradingstates$capital$instrumentid==instrumentid] <- 0
        .tradingstates$capital$totallongholdings[.tradingstates$capital$instrumentid==instrumentid] <- 0
        .tradingstates$capital$totalshortholdings[.tradingstates$capital$instrumentid==instrumentid] <- 0
        .tradingstates$capital$cash[.tradingstates$capital$instrumentid==instrumentid] <- CASH
    }
    
    ## initialize target holding(after read holding) for trade center
    .tradingstates$th <- rbind(.tradingstates$th,
                               data.frame(instrumentid=instrumentid,
                                          longholding=.tradingstates$capital$totallongholdings[.tradingstates$capital$instrumentid==instrumentid],
                                          shortholding=.tradingstates$capital$totalshortholdings[.tradingstates$capital$instrumentid==instrumentid],
                                          stringsAsFactors = FALSE))
    .tradingstates$th <- unique(.tradingstates$th)
    if(nrow(.tradingstates$th)==0){
        stop("error while generating target holdings")
    }
    
    ## I'm lazy?
    if(.tradingstates$IMLAZY){
        .lazyexpressions(instrumentid=instrumentid,type = "specific")
        .lazyexpressions(instrumentid=instrumentid,
                         ninstruments = length(.INSTRUMENT$instrumentid),
                         type = "general")
    }
    
}

##' randomid
##'
##' randomid
##' @title randomid
##' @param n id length
##' @return order id
##' @export 
##' @author Chen
##'
randomid <- function(n){paste(letters[ceiling(runif(n,0,26))],collapse = "")}

##' is new day?
##'
##' is new day?
##' @title is.newday
##' @param instrumentid character, instrument identifier, unique.
##' @return logical, indication wether current data come from a new trading day
##' @export 
##' @author Chen
##'
is.newday <- function(instrumentid){
    return(.tradingstates$startoftheday[instrumentid])
}


##' perfectexecution
##'
##' perfectexecution
##' @title perfectexecution
##' @return nothing
##' @export 
##' @author Chen
##'
perfectexecution<-function(instrumentid,orderid="xxx",direction,price,hands,action,type="limit"){

    tradetime=.tradingstates$currenttradetime

    if(any(hands<=0)) stop("hands must be greater than zero!")
    if(is(direction,"character") | any(!direction%in%c(-1,1))) stop("direction must be numeric or integer of value  1 or -1!")
    if(any(price<=0)) stop("price must be greater than 0!")
    if(any(!action%in%c("open","close"))) stop("action can only be open or close!")
    ## if(missing(type))
    ## stop("order type not found!")
    if(any(!type%in%c("limit","market"))) stop("type must be one of limit or market!")
    if(length(unique(type))>1) stop("can only submitt one type of orders!")
    
    ## multiple orders
    tryCatch(orders <- data.frame(instrumentid=instrumentid,direction=direction,price=price,hands=hands,action=action,stringsAsFactors = FALSE),
             warning=function(w){stop("instrumentid, direction, price, hands and action must be of length one or the same length with the number of orders!!")},
             error=function(e){stop("instrumentid, direction, price, hands and action must be of length one or the same length with the number of orders!!")})
    
    for(i in 1:nrow(orders)){
        fee <- .INSTRUMENT$fee[[instrumentid]]
        closeprior <- .INSTRUMENT$closeprior[[instrumentid]]
        multiplier <- .INSTRUMENT$multiplier[[instrumentid]]
        ## additional evaluation expression durring debuging, do not  delete
        ## eval(parse(text = paste(".tradingstates$currenttimeformat <- ",ENV,"$timeformat",sep ="")))
        
        ## add initial hands
        id <- randomid(5)
        .tradingstates$orders <- data.frame(instrumentid="someinstrument",orderid=id,direction=0,price=0,hands=0,action="someaction",initialhands=orders$hands[i],timeoutlist=FALSE,timeoutchase=FALSE,timeoutsleep=1,chaselist=FALSE,chasesleep=1,submitstart=tradetime,stringsAsFactors=FALSE)
        
        cost <- .updatecapital(orders$instrumentid[i],orders$direction[i],orders$hands[i],orders$action[i],orders$price[i],fee,closeprior,multiplier)
        .writecapitalhistory(instrumentid=orders$instrumentid[i],tradeprice=orders$price[i],tradehands=orders$hands[i],cost=cost)
        .writeorderhistory(instrumentid=orders$instrumentid[i],orderid=id,direction=orders$direction[i],hands=0,price=orders$price[i],tradeprice=orders$price[i],status=0,action=orders$action[i],cost=cost)
        .writetraded(orders$instrumentid[i],id,orders$action[i],orders$direction[i],orders$hands[i],orders$price[i])
        .trackclosed(orders$instrumentid[i],orders$action[i],orders$direction[i],orders$hands[i],orders$price[i],multiplier)
        .trackunclosed(orders$instrumentid[i],id,orders$action[i],orders$direction[i],orders$hands[i],orders$price[i])
    }
    
}

##' closeall
##'
##' closeall
##' @title closeall
##' @return nothing
##' @export
##' @author Chen
##'
closeall <- function(instrumentid="qtid",price=NULL,type="limit"){
    
    capital <- querycapital(instrumentids = instrumentid)
    if(nrow(capital)==0){
        warning(paste(instrumentid,"not found!"))
        return()
    }
    if(nrow(capital)>1){
        stop("close more than one instruments!")
    }
    if(capital$totallongholdings<=0 & capital$totalshortholdings>=0){
        warning("no holdings to be closed")
        return()
    }
    match.arg(type,c("limit","market"))

    ## ordersubmission
    if(capital$totallongholdings!=0)
        ordersubmission(instrumentid=instrumentid,orderid = randomid(5),
                        direction = -1,price = 0,hands=capital$totallongholdings,action = "close")
    if(capital$totalshortholdings!=0)
        ordersubmission(instrumentid=instrumentid,orderid = randomid(5),
                        direction = 1,price = 0,hands= -capital$totalshortholdings,action = "close")
    
    return()
}


##' cancelall
##' 
##'  cancel all satisfied orders
##' 
##' @param instrumentid character, specifying a filter for instrument identifiers.
##' @param direction integer, specifying a filter for trading directions. 1 for long and -1 for short.
##' @param pricemin numeric, specifying a filter for price lower limit.
##' @param pricemax numeric, specifying a filter for price upper limit.
##' @param action character, specifying a filter for actions, can take value from one of "open","close","closetoday","closepreday"
##' @param orderid character, specifying the set of orderids to be canceled. NOTE: if orderid is not null, cancelall will disregard any other filters and cancel orders only by orderid 
##' @return nothing
##' @seealso  \link{replaceall}
##' @export
##' @examples
##'\dontrun{
##' ## cancel all orders satisfy direction==-1
##' cancelall(tradetime,direction==-1)
##' }
##' @author Chen
##'
cancelall <- function(instrumentid=NULL,direction=NULL,action=NULL,pricemin=NULL,pricemax=NULL,orderid=NULL){
    orders <- .tradingstates$orders
    if(nrow(orders)==0){
        return()
    }
    
    ## orderid is not null
    if(!is.null(orderid)){
        orders <- orders[orders$orderid%in%orderid,]
        if(nrow(orders)==0){
            return()
        }
        for(i in seq_along(orders$orderid)){
            ordersubmission(instrumentid = orders$instrumentid[i],orderid = orders$orderid[i],action = "cancel")
        }
        return()
    }
    ## orderid is null
    if(!is.null(instrumentid)){
        orders <- orders[orders$instrumentid%in%instrumentid,]
    }
    if(!is.null(direction)){
        orders <- orders[orders$direction==direction,]
    }
    if(!is.null(action)){
        orders <- orders[orders$action%in%action,]
    }
    if(!is.null(pricemin)){
        orders <- orders[orders$price>=pricemin,]
    }
    if(!is.null(pricemax)){
        orders <- orders[orders$price<=pricemax,]
    }
    if(nrow(orders)==0){
        return()
    }
    for(i in seq_along(orders$orderid)){
        ordersubmission(instrumentid = orders$instrumentid[i],orderid = orders$orderid[i],action = "cancel")
    }
    return()
}

##' replaceall
##' 
##'  replace all satisfied orders with one new order which has a new price and a new hands equal to the cumulated hands of orders replaced.
##' 
##' @param tradetime character, time in current tick.
##' @param instrumentid character, specifying a filter for instrument identifier.
##' @param direction integer, specifying a filter for trading direction. 1 for long and -1 for short.
##' @param pricemin numeric, specifying a filter for price lower limit.
##' @param pricemax numeric, specifying a filter for price upper limit.
##' @param action character, specifying a filter for actions, can take value from one of "open","close","closetoday","closepreday". 
##' @param newprice numeric, new order price, will replace with a market order when newprice=0
##' @return nothing
##' @seealso  \link{cancelall}
##' @export
##' @examples
##'\dontrun{
##' ## find all orders satisfy direction==-1 and action=='open' and price <=101, replace them with a new order with price 100.01.
##' replaceall(tradetime,"TF1512",direction=-1,action='open',pricemax=101,newprice=100.01)
##' }
##' @author Chen
##'
replaceall <- function(instrumentid=NULL,direction=NULL,action=NULL,pricemin=NULL,pricemax=NULL,newprice=NULL){
    ## cancel old orders
    orders <- .tradingstates$orders
    if(nrow(orders)==0){
        print("no orders to replace")
        return()
    }
    if(is.null(instrumentid) | is.null(direction) | is.null(action) | is.null(newprice) ){
        stop("instrumentid, direction, action and newprice can not be NULL!")
    }
    else{
        orders <- orders[orders$instrumentid%in%instrumentid &
                         orders$direction==direction &
                         orders$action%in%action,]
    }
    if(!is.null(pricemin)){
        orders <- orders[orders$price>=pricemin,]
    }
    if(!is.null(pricemax)){
        orders <- orders[orders$price<=pricemax,]
    }
    if(nrow(orders)==0){
        print("no orders to replace")
        return()
    }
    for(i in seq_along(orders$orderid)){
        ordersubmission(instrumentid = orders$instrumentid[i],orderid = orders$orderid[i],action = "cancel")
    }
    ## submit a new one
    ordersubmission(instrumentid = instrumentid,orderid = randomid(5),direction=direction,price=newprice,hands=sum(orders$hands),action = action)
    return()
}


##' lazysubmission
##'
##'   submit target holdings, trade center will cancel all irrevelant orders and chase bid1 or ask1 price automatically to achieve target holdings. this function can only be used when set tc=TRUE in initializestates()
##' 
##' @param instrumentid character, instrument identifier
##' @param longholding integer, specifying target long holdings of 'instrumentid', longholding >=0
##' @param shortholding integer, specifying target short holdings of 'instrumentid', shortholding <= 0
##' @return nothing
##' @export
##' @seealso  \link{initializestates}
##' @examples
##'\dontrun{
##' 
##'  lazysubmission(tradetime,"TF1512",longholding=5,shortholding=-3)
##' }
##' @author Chen
##'
lazysubmission <- function(instrumentid,longholding=NULL,shortholding=NULL){
    
    tradetime=.tradingstates$currenttradetime
    if(!.tradingstates$tc){
        stop("lazysubmission: trade center not enabled! pleas set tc=TRUE at initialization")
    }
    
    if(!is.null(longholding)){
        .tradingstates$th$longholding[.tradingstates$th$instrumentid==instrumentid] <- longholding
    }
    if(!is.null(shortholding)){
        .tradingstates$th$shortholding[.tradingstates$th$instrumentid==instrumentid] <- shortholding
    }
    
    ## update immediatelly
    .tradingstates$justchanged[instrumentid] <- TRUE
    .tradingstates$lastchange[instrumentid] <- tradetime
    .tradecenter(instrumentid)

}

## cancelallother cancel all other orders other than specific levels
## cancelprime cancel all orders with higher priority price
## cancelsub cancel all orders with lower priority price
## cancelnotinthebook cancel orders not in orderbook
submitmultilevelopen <- function(instrumentid,LEVELS=c(1,2),hands=1,cancelallother=FALSE,cancelprime=FALSE,cancelsub=FALSE,DIRECTION=1,cancelnotinthebook=FALSE){
    LIMITS <- subset(.tradingstates$orders,price!=0&direction==DIRECTION)
    if(DIRECTION==1){
        orderbook <- .INSTRUMENT$orderbook[[instrumentid]]$buybook
    }
    else{
        orderbook <- .INSTRUMENT$orderbook[[instrumentid]]$sellbook
    }

    if(nrow(LIMITS)!=0){
        idx <- match(LIMITS$price,orderbook$price)
        ## 0. cancel orders not in the book
        if(cancelnotinthebook){
            if(any(is.na(idx))){
                cancelall(orderid = LIMITS$orderid[is.na(idx)])
            }
        }
        ## 1. conditional cancel and open
        if(any(!is.na(idx))){
            LIMITS <- LIMITS[!is.na(idx),]
            idx <- na.omit(idx)
            ## 1.1 cancel
            if(cancelallother){
                allother <- !(idx%in%LEVELS)
                if(any(allother)){
                    cancelall(orderid = LIMITS$orderid[allother])
                }
            }
            else if(cancelprime){
                primeorders <- idx<min(LEVELS)
                if(any(primeorders)){
                    cancelall(orderid = LIMITS$orderid[primeorders])
                }
            }
            else if(cancelsub){
                suborders <- idx>max(LEVELS)
                if(any(suborders)){
                    cancelall(orderid = LIMITS$orderid[suborders])
                }
            }
            ## 1.2 open
            neworders <- !(LEVELS%in%idx)
            if(any(neworders)){
                multisubmission(instrumentid=instrumentid,direction = DIRECTION,price = orderbook$price[LEVELS[neworders]],hands = hands,action = "open")
            }
        }
    }
    else{
        multisubmission(instrumentid=instrumentid,direction = DIRECTION,price = orderbook$price[LEVELS],hands = hands,action = "open")
    }
    
}

chasecloseall <- function(instrumentid,chasesleep=1){
    ## long holdings
    LH <- .tradingstates$capital$totallongholdings[.tradingstates$capital$instrumentid==instrumentid]
    ## short holdigns
    SH <- .tradingstates$capital$totalshortholdings[.tradingstates$capital$instrumentid==instrumentid]
    ## long close
    LC <- sum(.tradingstates$orders$hands[.tradingstates$orders$instrumentid==instrumentid & .tradingstates$orders$direction==1 & .tradingstates$orders$action=="close"])
    ## short close
    SC <- sum(.tradingstates$orders$hands[.tradingstates$orders$instrumentid==instrumentid & .tradingstates$orders$direction==-1 & .tradingstates$orders$action=="close"])

    orderbook <- .INSTRUMENT$orderbook[[instrumentid]]

    if(LH-SC>0){
        chasesubmission(instrumentid=instrumentid,orderid = randomid(5),
                        direction = -1,price = orderbook$sellbook$price[1],hands = LH-SC,action = "close",chasesleep = chasesleep)
    }

    if((-SH)-LC>0){
        chasesubmission(instrumentid=instrumentid,orderid = randomid(5),
                        direction = 1,price = orderbook$buybook$price[1],hands = (-SH)-LC,action = "close",chasesleep = chasesleep)
    }

}

## market order flow:
## bid1,ask1 : previous bid1 and ask1 prices
## lastprice,volume : current last price and volume
## AGGREGATE: indicating return cumulate value or not
## return a matirx with two columes.
BSI <- function(lastprice,bid1,ask1,volume,AGGREGATE=FALSE){
    mid <- (bid1+ask1)/2
    if(AGGREGATE){
        BI <- sum(volume[lastprice>mid],na.rm = TRUE)
        SI <- sum(volume[lastprice<mid],na.rm = TRUE)
        other <- sum(volume[lastprice==mid],na.rm = TRUE)/2
        BI <- BI+other
        SI <- SI+other
        return(c(BI=BI,SI=SI))
    }
    else{
        BI <- volume
        SI <- volume
        BI[lastprice<mid] <- 0
        SI[lastprice>mid] <- 0
        idx <- lastprice==mid
        if(any(idx)){
            BI[idx] <- volume[idx]/2
            SI[idx] <- BI[idx]
        }
        return(cbind(BI,SI))
    }
}

## limit order flow:
BSO <- function(orderbook,preorderbook,bsi){
    
}


## dataformat <- list(pbuyhands = seq(from = 32,by = 1,length.out = 5),
## pbuyprice = seq(from = 22,by = 1,length.out = 5),
## psellhands = seq(from = 37,by = 1,length.out = 5),
## psellprice = seq(from = 27,by = 1,length.out = 5),
## ptradetime = 2,plastprice = 4,pvolume = 12,
## ppresettleprice=8)

## ...: other parameters passed to stg
## datalist must be a list of data.frame(s) or a data.frame.
## formatlist is either a list of data format specifycation or a list of lists of specifications.
## instrumentids: instrument identifier
HFTsimulator <- function(stg,...,instrumentids,datalist,formatlist,
                         tc=FALSE,Sleep=1,IMLAZY=FALSE,DIGITSSECS=3,STRINGSASFACTORS=FALSE,septraded=FALSE,unclosed=TRUE,closed=TRUE,interdaily=FALSE,
                         verboselimitpriors=TRUE){
    ## strategy function check
    if(!is(stg,"function")){
        stop(substitute(stg),"is not a function!")
    }
    ## data check
    ## put all data in a list, the list is of the same length of instrumetids
    if(!is(instrumentids,"character")) stop("instrumentids must be of type character.")
    if(is(datalist,list)){
        if(length(instrumentids)!=length(datalist)) stop("length of instrumentids is not equal to length of datalist!")
        names(datalist) <- instrumentids #sequence of the datas must be in accordance with instrumentids.
    }else if(is(datalist,"data.frame")){
        if(length(instrumentids)!=1) stop("unequal length of data and instrumentids")
        eval(parse(text = paste("datalist<- list(",instrumentids,"=datalist)",sep = ""))) #convert to list
    }else{
        stop("datalist must be of type data.frame or list")
    }
    ## data format check
    ## put all dataformat in a list, the list is of the same length of instrumetids
    requiredformat <- c("pbuyhands","pbuyprice","psellhands","psellprice","ptradetime","plastprice","pvolume","ppresettleprice")
    if(all(requiredformat%in%names(formatlist))){
        eval(parse(text = paste("formatlist <- list(",paste(paste(instrumentids,"=formatlist"),collapse = ","),")")))
    }else if(all(requiredformat%in%names(formatlist[[1]]))){
        if(length(formatlist)!=1 & length(formatlist)!=length(instrumentids)) stop("unequal length of formatlist and datalist.")
    }else{
        stop("missing format specifications in ",substitute(formatlist))
    }


    ## garbage picker
    garbagepicker <- eval(parse(text = deparse(stg)))

    ## environment settings
    options(digits.secs=DIGITSSECS)
    options(stringsAsFactors = STRINGSASFACTORS)

    ## initialize simulator state
    .tradingstates$tc <- tc             #trade-center
    .tradingstates$septraded <- septraded
    .tradingstates$interdaily <- interdaily #interdaily support
    .tradingstates$Sleep <- Sleep           #trade-center idle time
    .tradingstates$closed <- closed         #recored all closed orders
    .tradingstates$unclosed <- unclosed     #track all unclosed orders

    ## <<<<<<<<<<<<<<< TO DO >>>>>>>>>>>>>>>
    ## rearrange data sequence (to support multiple instruments with different data formats)
    if(length(formatlist)>=2){
        if(any(vapply(2:length(formatlist),function(i){
            !identical(formatlist[[i]],formatlist[[i-1]])
        },FUN.VALUE = logical(1)))) stop("multiple instruments with different data formats is not supported yet.")
    }
    ## merge all instruments' data to a large data.frame
    tags <- rep(instrumentids,times=vapply(datalist,function(d){nrow(d)},FUN.VALUE = numeric(1)))
    datalist <- lapply(datalist,function(d){names(d) <- paste("V",1:ncol(d),sep = "");return(d)})
    datalist <- do.call(rbind,datalist)
    datalist$instrumentid <- tags
    datalist <- datalist[order(datalist[,formatlist[[1]]$ptradetime]),] #order by time
    
    ## initialize instruments' states
    if(length(formatlist)==1 & length(formatlist)!=length(instrumentids)){
        formatlist <- rep(formatlist,length(instrumentids))
        names(formatlist) <- instrumentids
    }
    for(instrumentid in instrumentids){

        dataformat <- formatlist[[instrumentid]]
        
        if(is.null(dataformat[["fee"]])){
            dataformat$fee=c(long=0,short=0,closetoday=0,closepreday=0)
        }
        if(is.null(dataformat[["closeprior"]])){
            dataformat$closeprior = "today"
        }
        if(is.null(dataformat[["timeformat"]])){
            dataformat$timeformat = "%Y-%m-%d %H:%M:%OS"
        }
        if(is.null(dataformat[["endoftheday"]])){
            dataformat$endoftheday="23:59:59.999"
        }
        if(is.null(dataformat[["multiplier"]])){
            dataformat$multiplier=1
        }

        initializeinstrument(instrumentid=instrumentid,
                             pbuyhands=dataformat$pbuyhands,
                             pbuyprice=dataformat$pbuyprice,
                             psellhands=dataformat$psellhands,
                             psellprice=dataformat$psellprice,
                             ptradetime=dataformat$ptradetime,
                             plastprice=dataformat$plastprice,
                             pvolume=dataformat$pvolume,
                             ppresettleprice=dataformat$ppresettleprice,
                             fee=dataformat$fee,
                             closeprior=dataformat$closeprior,
                             timeformat=dataformat$timeformat,
                             endoftheday=dataformat$endoftheday,
                             multiplier=dataformat$multiplier)
    }

    ## simulation
    for(i in 1:nrow(datalist)){
        .CFEupdate(DATA = datalist[i,],INSTRUMENTID = datalist[i,"instrumentid"])
        garbagepicker(...)
        if(verboselimitpriors){
            .verboselimitpriors()
        }
    }
    invisible(list(orders=.tradingstates$orderhistory,capitals=.tradingstates$capitalhistory))
}
