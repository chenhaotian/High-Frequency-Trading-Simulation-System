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
## .INSTRUMENT$ppresettleprice <- list()


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


##' Treasury Future's TAQ data in 2015-12-25
##'
##' A dataset containing all the TAQ informations. The variables are as follows:
##'
##' \itemize{
##'    \item CONTRACTID. instrument id
##'    \item TDATETIME. trade time
##'    \item OPENPRICE. open price
##'    \item LASTPRICE. last price
##'    \item HIGHPRICE. high price
##'    \item LOWPRICE. low price
##'    \item SETTLEPRICE. settle price
##'    \item PRESETTLE. previous settle price
##'    \item CLOSEPRICE. close price
##'    \item PRECLOSE. previous close
##'    \item CQ. current trade volume
##'    \item VOLUME. total volume
##'    \item CM. cm
##'    \item AMOUNT. amount
##'    \item PREPOSITION. preposition
##'    \item POSITION. position
##'    \item POSITIONCHANGE. positionchange
##'    \item LIMITUP. limitup
##'    \item LIMITDOWN. limitdown
##'    \item SIDE. side
##'    \item OC. oc
##'    \item B01. bid price 01
##'    \item B02. bid price 02
##'    \item B03. bid price 03
##'    \item B04. bid price 04
##'    \item B05. bid price 05
##'    \item S01. ask price 01
##'    \item S02. ask price 02
##'    \item S03. ask price 03
##'    \item S04. ask price 04
##'    \item S05. ask price 05
##'    \item BV01. bid volume 01
##'    \item BV02. bid volume 02
##'    \item BV03. bid volume 03
##'    \item BV04. bid volume 04
##'    \item BV05. bid volume 05
##'    \item SV01. ask volume 01
##'    \item SV02. ask volume 02
##'    \item SV03. ask volume 03
##'    \item SV04. ask volume 04
##'    \item SV05. ask volume 05
##'    \item CURRDELTA. currdelta
##'    \item PREDELTA. predelta
##'    \item SETTLEMENTGROUPID. settlementgroupid
##'    \item SETTLEMENTID. settlementid
##'    \item CHANGE. change
##'    \item CHANGERATIO. changeratio
##'    \item CONTINUESIGN. continuesign
##'    \item TRADINGDATE. tradingdate
##'    \item LOCALTIME. localtime
##'    \item RECTIME. rectime
##'    \item EXCHANGECODE. exchange code
##'    \item ID. id
##'    \item UNIX. unix time stamp
##'    \item DATE. date, (%Y-%m-%d)
##' }
##'
##' @docType data
##' @keywords datasets
##' @name TFtaq
##' @format A data frame with 19178 rows and 55 variables
"TFtaq"

         
##' Data Format of Treasury Future's TAQ data
##'
##' A dataset containing format informations. The variables are as follows:
##'
##' \itemize{
##'    \item pbuyhands. position of bid volumes
##'    \item pbuyprice. position of bid prices
##'    \item psellhands. position of ask volumes
##'    \item psellprice. position of ask prices
##'    \item ptradetime. position of trade time
##'    \item plastprice. position of last price
##'    \item pvolume. position total volume
##'    \item fee. fee
##'    \item closeprior. close priority
##'    \item timeformat. time format
##'    \item multiplier. multiplier
##' }
##'
##' @docType data
##' @keywords datasets
##' @name TFformat
##' @format A list with 12 elements
"TFformat"
