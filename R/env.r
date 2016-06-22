options(digits.secs=3)
options(stringsAsFactors = FALSE)

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
