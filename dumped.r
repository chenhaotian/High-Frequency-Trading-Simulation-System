
##' initializestates
##' 
##'  initialize simulator states, including simulation back ground functionality and many ohter simulator related parameters. return an environment named 'tradingstates'. queuing orders and capital state will be saved and kept updated in tradingstates, see 'Details' for more information. *please read this documentation carefully before running any strategy!*
##' 
##' @param tc logical, indicating wehter to use a simulated tradecenter. when tc=TRUE, submitmethod will be coerced to 'lazysubmission'(type ?lazysubmission for details). see 'Details' for more about tradecenter
##' @param Sleep numeric, idle time length of simulated tradecenter, measured in seconds, default 1. see 'Details' for more information.
##' @param IMLAZY logical, pleas set it to TRUE if you are lazy. type ?initializeinstrument for more infromation.
##' @param DIGITSSECS integer, second digits, default 3
##' @param septraded logical, indicating wether to record traded orders separately.
##' @param unclosed logical, indicating wether to track all unclosed orders, set unclosed=TRUE when you need to calculate mean open price and open profit. type ?meanopen for more infromation.
##' @param closed logical, indicating wether to track all zero holding states, set closed=TRUE when you need to calculate close profit.
##' @param interdaily logical, indicating wether to support interdaily trading.
##' @return tradingstates env, an environment in .GlobelEnv containing all the parameters specified above.
##' @details tradingstates: an environment containing all the simulators' parameters, there are two improtant dataframes stored in this envrionment, 'orders' and 'capital'. All current queuing orders will be recorded as one row in 'orders' during simulation. if there haven't submitted any orders or all the orders are traded(i.e. no queuing orders), 'orders' will be a data.frame with 0 rows. each instrument's capital state will be stored as one row in 'capital'. 'capital' has at least one row. one can use queryorder() and qureycapital() inside their strategy to fetch 'orders' and 'capital' from tradingstates.
##'
##' orderhistory: every changed order will be recorded as one additional row in orderhistory after every updating(submit new order, cancel order, partial traded, all traded ...) of orders, saved as either a comma separated table in Redis or a local file. columns of the table are: instrumentid,orderid,direction,price, hands,action,trade time,trade price,cost and status. 'cost' represent the commission of current update, it's calculated from parameter `fee` specified in initializeinstrument(), type ?initializeinstrument for more details; 'status' represent current order's status: 0, all traded; 1, part traded, rest queuing; 2, part traded, rest canceled; 3, no trade, queuing; 4, no trade, no queuing; 5, canceled; 6, submission failed;
##'
##' capitalhistory: the newest capital state will be recorded as one additional row in capitalhistory after each change, saved the same way as orderhistory. the columns are: instrumentid,today's long holdings, today's short holdings, previous long holdings, previous short holdings, total long holdings, total short holdings, cash, update time, trade price, traded hands and cost.
##'
##' tradecenter: a simulated trade center. automatically check for unsatisfied orders and repalce them with new ones to achieve target holdings(target holdings are set by lazysubmission).  'Sleep' set the idle time between each checking, default 0.
##' @seealso \link{initializeinstrument} \link{lazysubmission} \link{meanopen}
##' @export
##' @examples
##'\dontrun{
##' ## wirte capital and order histories to local file, don't sychronize target holdings, don't use simulated trade center and tell the simulator 'I'm not lazy'.
##' initializestates(realtime=FALSE,writeholding=FALSE,tc=FALSE,IMLAZY = FALSE)
##' }
##' @author Chen
##' 
initializestates <- function(tc=FALSE,Sleep=1,IMLAZY=FALSE,DIGITSSECS=3,STRINGSASFACTORS=FALSE,septraded=FALSE,unclosed=TRUE,closed=TRUE,interdaily=FALSE){
    

    ## second digits, default 3
    options(digits.secs=DIGITSSECS)
    options(stringsAsFactors = STRINGSASFACTORS)
    
    .tradingstates <- new.env(parent = globalenv())
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
    .tradingstates$tc <- tc              #trade center?
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
    .tradingstates$septraded <- septraded
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
    .tradingstates$interdaily <- interdaily
    .tradingstates$startoftheday <- logical()

    ## verbose
    .tradingstates$verbosepriors <- NULL
    
    ## trade center invoke tag and sleep recorder
    .tradingstates$justchanged <- NULL
    .tradingstates$lastchange <- NULL
    .tradingstates$Sleep <- Sleep
    
    ## instrument-closeprofit tracker
    .tradingstates$closed <- closed
    .tradingstates$closedtracker <- data.frame(instrumentid=character(),cash=numeric(),stringsAsFactors=FALSE)

    ## track unclosed orders
    .tradingstates$unclosed <- unclosed
    .tradingstates$unclosedlong <- .tradingstates$longopen
    .tradingstates$unclosedshort <- .tradingstates$longopen
    .tradingstates$unclosedsettleprice <- logical()

    
    ## I'm lazy?
    .tradingstates$IMLAZY <- IMLAZY
    if(IMLAZY){
        .lazyfunctions()
    }
    
    ## .tradingstates will be deleted after exit
    ## assign it to an object in globalenv to aviod deletion
    assign(".tradingstates",.tradingstates,envir = globalenv())
}

##' initializeTF
##'
##'   a simple wrapper of initializestates() and initializeinstrument() for Treasury Futures. type ?initializestates and ?initializeinstrument for details.
##'
##' @param realtimeDATA logical, indicating wether to use realtime data.
##' @param TFs character, specifying TF ids to be initialized.
##' @param fee named numeric, specifying conmissions of different actions, including open, close, closetoday and closepreday. 'cost' in orderhistory and capitalhistory are result calculated by 'fee'.
##' @param closeprior character, specifying close priority when specified action='close' in ordersubmission. closeprior can only be one of 'today' and 'preday'. when closeprior='today', ordersubmission will close today's holdings prior than previous days', vise versa. type ?ordersubmission for details.
##' @param tc logical, indicating wehter to use a simulated tradecenter. when tc=TRUE, submitmethod will be coerced to 'lazysubmission'(type ?lazysubmission for details). see 'Details' for more about tradecenter
##' @param Sleep numeric, idle time length of simulated tradecenter, measured in seconds, default 1. see 'Details' for more information.
##' @param IMLAZY logical, pleas set it to TRUE if you are lazy. type ?initializeinstrument for more infromation.
##' @param DIGITSSECS integer, second digits, default 3
##' @param septraded logical, indicating wether to save traded orders separately.
##' @param unclosed logical, indicating wether to track all unclosed orders, set unclosed=TRUE when you need to calculate mean open price and open profit. type ?meanopen for more infromation.
##' @param closed logical, indicating wether to track all zero holding states, set closed=TRUE when you need to calculate close profit.
##' @param interdaily logical, indicating wether to support interdaily trading.
##' @return nothing
##' @seealso  \link{initializestates} \link{initializeinstrument}
##' @export 
##' @author Chen
##'
initializeTF <- function(realtimeDATA=TRUE,TFs="TF1512",fee=c(long=0.00000225,short=0.00000225,closetoday=0.00000225,closepreday=0.00000225),closeprior="today",tc=FALSE,Sleep=1,IMLAZY=FALSE,DIGITSSECS=3,STRINGSASFACTORS=FALSE,septraded=FALSE,unclosed=TRUE,closed=TRUE,interdaily=FALSE){

    if(missing(realtimeDATA)){
        stop("realtimeDATA must be specified!")
    }
    
    ## initialize states
    initializestates(tc=tc,Sleep=Sleep,IMLAZY=IMLAZY,DIGITSSECS=DIGITSSECS,STRINGSASFACTORS=STRINGSASFACTORS,septraded=septraded,unclosed=unclosed,closed=closed,interdaily = interdaily)
    
    ## initialize instruments
    if(realtimeDATA){
        for(tf in TFs){
            initializeinstrument(tf,
                                 pbuyhands = seq(from = 19,by = 2,length.out = 5),
                                 pbuyprice = seq(from = 20,by = 2,length.out = 5),
                                 psellhands = seq(from = 29,by = 2,length.out = 5),
                                 psellprice = seq(from = 30,by = 2,length.out = 5),
                                 ptradetime = 2,plastprice = 8,pvolume = 17,
                                 ppresettleprice=4,
                                 fee = fee,
                                 closeprior = "today",
                                 timeformat="%Y%m%d%H%M%OS",
                                 endoftheday="15:15:00.000",
                                 multiplier=10000)
        }
    }
    else{
        for(tf in TFs){
            initializeinstrument(tf,
                                 pbuyhands = seq(from = 32,by = 1,length.out = 5),
                                 pbuyprice = seq(from = 22,by = 1,length.out = 5),
                                 psellhands = seq(from = 37,by = 1,length.out = 5),
                                 psellprice = seq(from = 27,by = 1,length.out = 5),
                                 ptradetime = 2,plastprice = 4,pvolume = 12,
                                 ppresettleprice=8,
                                 fee = fee,
                                 closeprior = "today",
                                 timeformat = "%Y-%m-%d %H:%M:%OS",
                                 endoftheday="15:15:00.000",
                                 multiplier = 10000)
        }
    }
    
}

##' initializeEnv
##' 
##'  initialize all simulator states
##' 
##' @export
##' @examples
##'\dontrun{
##' instruments <- c("AL1603", "TF1606", "AU1606")
##' detail <- getdetail(instruments)
##' initializeENV(realtimeDATA = FALSE,ENVname = "AL",instruments = instruments[1],exchange = detail$exchanges[1],multiplier = detail$multipliers[1],endoftheday = detail$endofthedays[1])
##' initializeENV(realtimeDATA = FALSE,ENVname = "TF",instruments = instruments[2],exchange = detail$exchanges[2],multiplier = detail$multipliers[2],endoftheday = detail$endofthedays[2])
##' }
##'
initializeENV <- function(realtimeDATA,instruments,exchanges,multipliers,endofthedays,fee=c(long=0.000004,short=0.000004,closetoday=0.000004,closepreday=0.000004),closeprior="preday",tc=FALSE,Sleep=1,IMLAZY=FALSE,DIGITSSECS=3,STRINGSASFACTORS=FALSE,septraded=FALSE,unclosed=TRUE,closed=TRUE,interdaily=FALSE){
    if(missing(realtimeDATA)|missing(exchanges)|missing(multipliers)|missing(endofthedays)){
        stop("realtimeDATA, exchanges, multipliers and endofthedays must be specified!")
    }
    if(!all(exchanges%in%c("CFE","SHF","DCE","CZCE"))){
        stop("exchange must be one of CFE, SHF, DCE or CZCE")
    }

    ## initialize states
    initializestates(tc=tc,Sleep=Sleep,IMLAZY=IMLAZY,DIGITSSECS=DIGITSSECS,STRINGSASFACTORS=STRINGSASFACTORS,septraded=septraded,unclosed=unclosed,closed=closed,interdaily = interdaily)

    tryCatch(tmp <- data.frame(instruments=instruments,exchanges=exchanges,multipliers=multipliers,endofthedays=endofthedays,stringsAsFactors = FALSE),
             warning=function(w){
                 stop("exchanges, multipliers, endofthedays can either be length one or the same length as instruments!")
             },
             error=function(e){
                 stop("exchanges, multipliers, endofthedays can either be length one or the same length as instruments!")
             }
             )

    ## for ifelse only supprot length one output
    IFELSE <- function(CONDITION,yes,no){
        if(CONDITION){
            return(yes)
        }else{
            return(no)
        }
    }

    for(i in 1:nrow(tmp)){
        exchange <- tmp$exchanges[i]
        instrument <- tmp$instruments[i]
        endoftheday <- tmp$endofthedays[i]
        multiplier <- tmp$multipliers[i]
        
        pbuyhands <- switch(exchange,
                            CFE=IFELSE(realtimeDATA,
                                       seq(from = 19,by = 2,length.out = 5),
                                       seq(from = 32,by = 1,length.out = 5)),
                            SHF=IFELSE(realtimeDATA,20,30),
                            DCE=IFELSE(realtimeDATA,0,0),
                            CZCE=IFELSE(realtimeDATA,0,0)
                            )
        pbuyprice <- switch(exchange,
                            CFE=IFELSE(realtimeDATA,
                                       seq(from = 20,by = 2,length.out = 5),
                                       seq(from = 22,by = 1,length.out = 5)),
                            SHF=IFELSE(realtimeDATA,19,28),
                            DCE=IFELSE(realtimeDATA,0,0),
                            CZCE=IFELSE(realtimeDATA,0,0)
                            )
        psellhands <- switch(exchange,
                             CFE=IFELSE(realtimeDATA,
                                        seq(from = 29,by = 2,length.out = 5),
                                        seq(from = 37,by = 1,length.out = 5)),
                             SHF=IFELSE(realtimeDATA,22,29),
                             DCE=IFELSE(realtimeDATA,0,0),
                             CZCE=IFELSE(realtimeDATA,0,0)
                             )
        psellprice <- switch(exchange,
                             CFE=IFELSE(realtimeDATA,
                                        seq(from = 30,by = 2,length.out = 5),
                                        seq(from = 27,by = 1,length.out = 5)),
                             SHF=IFELSE(realtimeDATA,21,27),
                             DCE=IFELSE(realtimeDATA,0,0),
                             CZCE=IFELSE(realtimeDATA,0,0)
                             )
        ptradetime <- switch(exchange,
                             CFE=IFELSE(realtimeDATA,2,2),
                             SHF=IFELSE(realtimeDATA,2,2),
                             DCE=IFELSE(realtimeDATA,0,0),
                             CZCE=IFELSE(realtimeDATA,0,0)
                             )
        plastprice <- switch(exchange,
                             CFE=IFELSE(realtimeDATA,8,4),
                             SHF=IFELSE(realtimeDATA,3,4),
                             DCE=IFELSE(realtimeDATA,0,0),
                             CZCE=IFELSE(realtimeDATA,0,0)
                             )
        pvolume <- switch(exchange,
                          CFE=IFELSE(realtimeDATA,17,12),
                          SHF=IFELSE(realtimeDATA,10,8),
                          DCE=IFELSE(realtimeDATA,0,0),
                          CZCE=IFELSE(realtimeDATA,0,0)
                          )
        ppresettleprice <- switch(exchange,
                              CFE=IFELSE(realtimeDATA,4,8),
                              SHF=IFELSE(realtimeDATA,4,16),
                              DCE=IFELSE(realtimeDATA,0,0),
                              CZCE=IFELSE(realtimeDATA,0,0)
                              )
        ## closeprior <- "preday"        #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        timeformat <- IFELSE(realtimeDATA,"%Y%m%d%H%M%OS",
                         "%Y-%m-%d %H:%M:%OS")
        initializeinstrument(instrument,
                             pbuyhands = pbuyhands,
                             pbuyprice = pbuyprice,
                             psellhands = psellhands,
                             psellprice = psellprice,
                             ptradetime = ptradetime,
                             plastprice = plastprice,
                             pvolume = pvolume,
                             ppresettleprice=ppresettleprice,
                             fee = fee,
                             closeprior = closeprior,
                             timeformat= timeformat,
                             endoftheday= endoftheday,
                             multiplier=multiplier)
    }
}

