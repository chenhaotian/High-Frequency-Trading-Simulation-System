library(grid)
library(plyr)
library(zoo)                            #na.locf
library(ggplot2)

## analysing tools
## 1.specific functions------------------
## 1.1 manipulate 0.5s data
## datamanipulation
##

datamanipulation <- function(instrumentdata,instrumentid){

    timeformat <- .INSTRUMENT$timeformat[[instrumentid]]
    plastprice <- .INSTRUMENT$plastprice[[instrumentid]]
    ptradetime <- .INSTRUMENT$ptradetime[[instrumentid]]
    pvolume <- .INSTRUMENT$pvolume[[instrumentid]]
    pbuyhands <- .INSTRUMENT$pbuyhands[[instrumentid]]
    pbuyprice <- .INSTRUMENT$pbuyprice[[instrumentid]]
    psellhands <- .INSTRUMENT$psellhands[[instrumentid]]
    psellprice <- .INSTRUMENT$psellprice[[instrumentid]]

    ## basic information and time format
    instrumentdata <- instrumentdata[,c(ptradetime,plastprice,pvolume,pbuyprice,pbuyhands,psellprice,psellhands)]
    names(instrumentdata) <- c("tradetime","lastprice","volume",paste("bid",1:length(pbuyprice),sep=""),paste("bidv",1:length(pbuyhands),sep=""),paste("ask",1:length(psellprice),sep=""),paste("askv",1:length(psellhands),sep=""))
    instrumentdata[-1] <- do.call(cbind.data.frame,llply(instrumentdata[,-1],function(l){round(as.numeric(l),5)}))
    instrumentdata$volume <- c(0,diff(instrumentdata$volume))
    instrumentdata$tradetime <- strftime(strptime(instrumentdata$tradetime,format = timeformat),format="%Y-%m-%d %H:%M:%OS")

    ## BI and SI
    L <- nrow(instrumentdata)
    instrumentdata$fairness <- "fair"
    instrumentdata$fairness[c(FALSE,instrumentdata$lastprice[-1]>(instrumentdata$bid1[-L]+instrumentdata$ask1[-L])/2+0.0000001)] <- "head"
    instrumentdata$fairness[c(FALSE,instrumentdata$lastprice[-1]<(instrumentdata$bid1[-L]+instrumentdata$ask1[-L])/2-0.0000001)] <- "tail"

    return(instrumentdata)
}
## 1.2 calculate profit and loss
pnl <- function(instrumentdata,capitalhistory,instrumentid){

    multiplier <- .INSTRUMENT$multiplier[[instrumentid]]

    ## close profit
    ## closeprofit <- unique(capitalhistory[capitalhistory$totallongholdings==0&capitalhistory$totalshortholdings==0,c("tradetime","cash")])
    
    capitalhistory <- ddply(capitalhistory,.(tradetime),function(d){
    tail(d,1)})
    pl <- merge(x=instrumentdata[,c("tradetime","lastprice")],y=capitalhistory[,c("tradetime","totallongholdings","totalshortholdings","cash")],by = "tradetime",all.x = TRUE)

    if(is.na(pl$totallongholdings[1]))
        pl$totallongholdings[1] <- 0
    if(is.na(pl$totalshortholdings[1]))
        pl$totalshortholdings[1] <- 0
    if(is.na(pl$cash[1]))
        pl$cash[1] <- 0
    
    pl$totallongholdings <- na.locf(pl$totallongholdings)
    pl$totalshortholdings <- na.locf(pl$totalshortholdings)
    pl$cash <- na.locf(pl$cash)

    return(data.frame(tradetime=pl$tradetime,pl=pl$lastprice*(pl$totallongholdings+pl$totalshortholdings)*multiplier+pl$cash,stringsAsFactors=FALSE))
    
}
## 1.3 max draw-down's range and value
## return a list containing draw-down range and value
maxdrawdown <- function(pl,ddown){
    MAXdown <- min(ddown)
    minidx <- which.min(ddown)
    maxidx <- which.max(pl[1:minidx])
    return(list(starttag=maxidx,endtag=minidx,MAXdown=MAXdown))
}
## 1.4 plot
## vplayout
vplayout<-function( x, y ){
    viewport( layout.pos.row=x, layout.pos.col=y )
}
## plot a two column's table, NAME and VALUE
plottwocolumntable <- function(d,h1=-1,h2=1,v1=0,v2=0,s=3){
    names(d) <- c("NAME","VALUE")
    d <- rbind(data.frame(NAME=" ",VALUE=" "),d)
    d$y <- (nrow(d):1)*0.5+0.5
    p5 <- ggplot(d)+geom_text(aes(x=1,y=y,label=NAME),hjust=h1,vjust=v1,size=s)+
        geom_text(aes(x=2,y=y,label=VALUE),hjust=h2,vjust=v2,size=s)+
        theme_minimal() + 
        theme(panel.grid.major = element_blank(), legend.position = "none",panel.border = element_blank(), axis.text.x =  element_blank(),axis.ticks =  element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())+scale_y_continuous(breaks=NULL)
    
    return(p5)
}
## pd: vpdata; pl: profit and loss; dd: draw down
## ddinfo: draw down information; lossinfo: successive loss; wininfo: successive win
## os: order summary table(ALL traded and cancled); ss: successive profit and loss summary table; pds: profit loss and drawdown summary
## traded: all and partial traded orders
summaryvpplot <- function(pd,pl,dd,ddinfo,lossinfo,wininfo,os,ss,pds,traded,SUMMARY=TRUE,TRADED=TRUE){
    pd$tradetime <- as.POSIXct(pd$tradetime)
    dd$tradetime <- as.POSIXct(dd$tradetime)
    pl$tradetime <- as.POSIXct(pl$tradetime)
    mapper <- c(fair="black",tail="steelblue",head="darkred")
    pd$fairness <- mapper[pd$fairness]

    ## price
    pmaxidx <- which.max(pd$lastprice)
    pminidx <- which.min(pd$lastprice)
    pd$MAX <- pd$lastprice[pmaxidx]
    pd$MIN <- pd$lastprice[pminidx]

    ## pl
    plmaxidx <- which.max(pl$pl)
    plminidx <- which.min(pl$pl)
    pl$MAX <- pl$pl[plmaxidx]
    pl$MIN <- pl$pl[plminidx]
    

    ## drawdown
    ddmaxidx <- which.min(dd$ddown)

    ## price and win/loss information
    p1 <- ggplot(pd)+
        geom_rect(aes(xmin=tradetime[1],xmax= tradetime[2],ymin=MIN,ymax=MAX),data=pd[c(ddinfo$starttag,ddinfo$endtag),],alpha=0.3,fill="blue")+
        geom_rect(aes(xmin=tradetime[1],xmax= tradetime[2],ymin=MIN,ymax=MAX),data=pd[c(lossinfo$starttag,lossinfo$endtag),],alpha=0.3,fill="steelblue")+
        geom_rect(aes(xmin=tradetime[1],xmax= tradetime[2],ymin=MIN,ymax=MAX),data=pd[c(wininfo$starttag,wininfo$endtag),],alpha=0.3,fill="darkred")+
        geom_line(aes(x=tradetime,y=lastprice))+
        xlab(NULL)+ylab(NULL)+
        theme(plot.margin=unit(c(0.5,0.5,0,0.2),units = "in"))+
        theme(panel.background=element_blank())+
        theme(panel.grid.major=element_line(linetype = 4,color = "gray40"))+
        scale_y_continuous(breaks=NULL)+
        geom_text(aes(x=tradetime,y=lastprice,label=lastprice),data=pd[pmaxidx,],vjust=0.5)+
        geom_text(aes(x=tradetime,y=lastprice,label=lastprice),data=pd[pminidx,],vjust=0.5)+
        scale_x_datetime(label=NULL)+
        scale_color_identity()+
        scale_fill_identity()

    if(TRADED & nrow(traded)!=0){
        traded$tradetime <- as.POSIXct(traded$tradetime)
        longopen <- traded[traded$direction==1&traded$action=="open",]
        longclose <- traded[traded$direction==1&traded$action=="close",]
        shortclose <- traded[traded$direction==-1&traded$action=="close",]
        shortopen <- traded[traded$direction==-1&traded$action=="open",]
        ## short <- traded[traded$direction==-1,]
        if(nrow(longopen)!=0)
            p1 <- p1+geom_text(aes(x=tradetime,y=price,label="↑"),data=longopen,color="darkred",size=2.5)
        if(nrow(shortclose)!=0)
            p1 <- p1+geom_text(aes(x=tradetime,y=price,label="#"),data=shortclose,color="darkred",size=2.5)
        if(nrow(shortopen)!=0)
            p1 <- p1+geom_text(aes(x=tradetime,y=price,label="↓"),data=shortopen,color="black",size=2.5)
        if(nrow(longclose)!=0)
            p1 <- p1+geom_text(aes(x=tradetime,y=price,label="#"),data=longclose,color="black",size=2.5)


    }
    ## geom_text(aes(x=pwintxt,y=(1/3)*lastprice[pminidx]+(2/3)*lastprice[pmaxidx],label=WIN),vjust=0.5,alpha=0.3,size=3,color="black")


    ## top left bottom right
    ## volume
    vmaxidx <- which.max(pd$volume)
    p2 <- ggplot(pd)+geom_bar(aes(x=tradetime,y=volume,fill=fairness,color=fairness),stat="identity")+
        ylab(NULL)+xlab(NULL)+
        theme(plot.margin=unit(c(0,0.5,0,0.2),units = "in"))+
        scale_x_datetime(label=NULL)+
        theme(panel.background=element_blank())+
        theme(panel.grid.major=element_line(linetype = 4,color = "gray40"))+
        scale_y_continuous(breaks=NULL)+
        geom_text(aes(x=tradetime,y=volume,label=volume),data=pd[vmaxidx,],vjust=0.5)+
        scale_fill_identity()+
        scale_color_identity()

    p3 <- ggplot(pl)+
        geom_rect(aes(xmin=tradetime[1],xmax= tradetime[2],ymin=MIN,ymax=MAX),data=pl[c(ddinfo$starttag,ddinfo$endtag),],alpha=0.3,fill="blue")+
        geom_rect(aes(xmin=tradetime[1],xmax= tradetime[2],ymin=MIN,ymax=MAX),data=pl[c(lossinfo$starttag,lossinfo$endtag),],alpha=0.3,fill="steelblue")+
        geom_rect(aes(xmin=tradetime[1],xmax= tradetime[2],ymin=MIN,ymax=MAX),data=pl[c(wininfo$starttag,wininfo$endtag),],alpha=0.3,fill="darkred")+
        scale_color_identity()+
        scale_fill_identity()+
        geom_line(aes(x=tradetime,y=pl))+
        xlab(NULL)+ylab(NULL)+
        theme(plot.margin=unit(c(0,0.5,0,0.2),units = "in"))+
        theme(panel.background=element_blank())+
        theme(panel.grid.major=element_line(linetype = 4,color = "gray40"))+
        scale_y_continuous(breaks=NULL)+
        geom_text(aes(x=tradetime,y=pl,label=round(pl,3)),data=pl[plminidx,],vjust=0.5,size=3)+
        geom_text(aes(x=tradetime,y=pl,label=round(pl,3)),data=pl[plmaxidx,],vjust=0.5,size=3)+
        scale_x_datetime(label=NULL)

    p4 <- ggplot(dd)+
        scale_color_identity()+
        scale_fill_identity()+
        geom_line(aes(x=tradetime,y=ddown))+
        xlab(NULL)+ylab(NULL)+
        theme(plot.margin=unit(c(0,0.5,0.2,0.2),units = "in"))+
        theme(panel.background=element_blank())+
        theme(panel.grid.major=element_line(linetype = 4,color = "gray40"))+
        scale_y_continuous(breaks=NULL)+
        geom_text(aes(x=tradetime,y=ddown,label=ddown),data=dd[ddmaxidx,],vjust=0.5,size=3)+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    grid.newpage()
    pushViewport(viewport(layout=grid.layout(10,10)))
    print(p1,vp=vplayout(1:4,1:10))
    print(p2,vp=vplayout(5:6,1:10))
    print(p3,vp=vplayout(7:8,1:10))
    print(p4,vp=vplayout(9:10,1:10))

    if(SUMMARY){
        ## order summary
        p5 <- plottwocolumntable(os,s=2.5)
        print(p5,vp=vplayout(1,4:5))
        ## successive change summary
        p6 <- plottwocolumntable(ss)
        print(p6,vp=vplayout(1:2,8:10))
        ## profit loss and draw down summary
        p7 <- plottwocolumntable(pds)
        print(p7,vp=vplayout(7:8,8:10))
    }

    ## print(p5,vp=vplayout(8,9))

}


## 2.general functions---------------
## 2.1 draw-down sequence
drawdown <- function(pl){
    MAX <- pl[1]
    ddown <- 0
    for(i in seq_along(pl)[-1]){
        MAX <- max(pl[i],MAX)
        ddown <- c(ddown,pl[i]-MAX)
    }
    return(ddown)
}
## 2.2 max successive change's range and value
## return a list
maxsuccessivechange <- function(sequence,direction,DIFF=TRUE,filtzeros=TRUE){
    if( (direction!=1 & direction!=-1) | !is.logical(DIFF)){
        stop("direction must be 1 or -1, DIFF must be logical!")
    }
    if(DIFF){
        sequence <- diff(sequence)
    }
    
    ## neutralize the difference of directions
    sequence <- sequence*direction
    
    ## get successtive change indexes
    starttags <- which(diff(c(FALSE,sequence>=0))==1)
    endtags <- which(diff(c(sequence>=0,FALSE))==-1)
    if(length(starttags)==0){
        warning("no satisfied sequence fund!")
        return()
    }
    ## calculate cumulate change for each successive period
    maxsuccessive <- vapply(X=seq_along(starttags),FUN=function(i){
        sum(sum(sequence[starttags[i]:endtags[i]]))
    },FUN.VALUE = 0.1)
    ## locate the max successive change
    idx <- which.max(maxsuccessive)
    ## TO DO : add zeros filter to the start and end of successive period---------
    if(direction==1){
        return(list(starttag=starttags[idx],endtag=endtags[idx],
                    MAXchange=max(maxsuccessive)))
    }else{
        return(list(starttag=starttags[idx],endtag=endtags[idx],
                    MAXchange=-max(maxsuccessive)))
    }
}

## data, capitalhistory, orderhistory, verbosepriors
## limitorders: plot which level's limit order price. for example limitorders=c(1,3,4)
## check the details of a specific limit order
## summary trade result
tradesummary <- function(instrumentdata,instrumentid="qtid",limitorders=NULL,starttime="09:15:00.000",endtime="15:15:00.000",SUMMARY=TRUE,TRADED=TRUE){
    
    ## data manipulation
    instrumentdata <- datamanipulation(instrumentdata,instrumentid)

    if(!is.null(starttime) & !is.null(endtime)){
        hfm <- strftime(as.POSIXct(instrumentdata$tradetime),"%H:%M:%OS")
        instrumentdata <- instrumentdata[hfm>=starttime & hfm<=endtime,]
    }
    if(nrow(instrumentdata)==0)
        stop("no data in selected time period!")
    
    ## get curren instrument's order and capital history

    orders <- .tradingstates$orderhistory[.tradingstates$orderhistory$instrumentid==instrumentid,]
    if(!is.null(starttime) & !is.null(endtime)){
        hfm <- strftime(as.POSIXct(orders$tradetime),"%H:%M:%OS")
        orders <- orders[hfm>=starttime & hfm<=endtime,]
    }
    capital <- .tradingstates$capitalhistory[.tradingstates$capitalhistory$instrumentid==instrumentid,]
    if(!is.null(starttime) & !is.null(endtime)){
        hfm <- strftime(as.POSIXct(capital$tradetime),"%H:%M:%OS")
        capital <- capital[hfm>=starttime & hfm<=endtime,]
    }

    ## profit and loss
    pl <- pnl(instrumentdata,capital,instrumentid)
    ## draw-down
    dd <- data.frame(tradetime=instrumentdata$tradetime,
                     ddown=drawdown(pl$pl))

    ## draw-down details
    ddinfo <- maxdrawdown(pl=pl$pl,ddown = dd$ddown)
    ## successive win or loss details
    lossinfo <- maxsuccessivechange(sequence=pl$pl,direction = -1,DIFF = TRUE)
    wininfo <- maxsuccessivechange(sequence=pl$pl,direction = 1,DIFF = TRUE)

    ## order summary
    os <- as.data.frame(table(orders$status),stringsAsFactors = FALSE)
    os <- os[os$Var1%in%c("0","5"),]
    statusmapper <- c(`0`="executed orders:",`5`="canceled orders:")
    os$Var1 <- statusmapper[os$Var1]

    ## profit and loos period summary:
    ss <- data.frame(
        NAME=c("     draw-donw start:","     draw-down end:","successive loss start:","successive loss end:","successive win start:","successive win end:"),
        value=strftime(c(dd$tradetime[ddinfo$starttag],dd$tradetime[ddinfo$endtag],pl$tradetime[lossinfo$starttag],pl$tradetime[lossinfo$endtag],pl$tradetime[wininfo$starttag],pl$tradetime[wininfo$endtag]),"%H:%M:%OS")
    )

    ## pl drawdown summary
    pds <- data.frame(
        NAME=c("                    max pl:","          max draw-donw:","max successive loss","max successive win"),
        VALUE=c(round(max(pl$pl),3),round(ddinfo$MAXdown,3),round(lossinfo$MAXchange,3),round(wininfo$MAXchange,3))
        )

    ## all traded and partially traded
    traded <- orders[orders$status%in%c(0,1),c("tradetime","direction","price","action")]

    summaryvpplot(instrumentdata,pl,dd,ddinfo,lossinfo,wininfo,os,ss,pds,traded,SUMMARY = SUMMARY,TRADED = TRADED)

    invisible(list(orderhistory=orders,capitalhistory=capital,
                   pl=pl,dd=dd,ddinfo=ddinfo,lossinfo=lossinfo,wininfo=wininfo,
                   traded=unique(orders[orders$status%in%c(0,1),c("tradetime","orderid","direction")]),
                   partiallytraded=unique(orders[orders$status==1,c("tradetime","orderid")]),
                   canceled=unique(orders[orders$status==5,c("tradetime","orderid")])
                   )
              )
    
}
## check the details of a specific limit order
checklimit <- function(instrumentdata,orderid){
    currentorder <- head(.tradingstates$orderhistory[.tradingstates$orderhistory$orderid==orderid,],1)
    if(nrow(currentorder)==0){stop("can't find ",orderid)}
    if(currentorder$price==0)(stop("must be a limit order!"))
    instrumentdata <- datamanipulation(instrumentdata,currentorder$instrumentid)
    ## limit? market?
    ## traded? canceled?
    ## time mapping

    ## locate current order and corresponding market data
    if(is.null(.tradingstates$verbosepriors)){warning("can't find any verbose information")}
    startandend <- range(.tradingstates$orderhistory$tradetime[.tradingstates$orderhistory$orderid==orderid])
    timeidx <- names(.tradingstates$verbosepriors)
    timeidx <- timeidx>=startandend[1] & timeidx<=startandend[2]
    currentverbose <- .tradingstates$verbosepriors[timeidx]
    currentdata <- instrumentdata[instrumentdata$tradetime>=startandend[1] & instrumentdata$tradetime<=startandend[2],]
    ## filter all records without updates
    updateidx <- c(TRUE,
                   vapply(X=2:length(currentverbose),FUN=function(i){
                       return(!identical(currentverbose[[i]][[orderid]],currentverbose[[i-1]][[orderid]]))
                   },FUN.VALUE = TRUE)
                   )
    updateidx[length(updateidx)] <- TRUE
    currentverbose <- currentverbose[updateidx]
    ## extract current order's change records, generate a data.frame
    d <- data.frame(tradetime=character(),hands=numeric(),price=numeric(),stringsAsFactors = FALSE)
    for(i in seq_along(currentverbose)){
        co <- currentverbose[[i]][[orderid]]
        ct <- data.frame(tradetime=rep(names(currentverbose[i]),nrow(co)))
        d <- rbind(d,cbind(ct,co))
    }

    ## 1.plot current order change
    d$price <- as.character(d$price)
    d$x <- as.factor(strftime(d$tradetime,format = "%H:%M:%OS"))
    d$y <- ddply(d,.(tradetime),function(x){data.frame(y=cumsum(c(0,x$hands[-nrow(x)]))+ceiling(x$hands/2))})$y #generate label positions
    p1 <- ggplot(d)+geom_bar(aes(x=x,y=hands,fill=price),position = "stack",stat = "identity")+scale_y_discrete(breaks=NULL)+scale_fill_grey()+geom_text(aes(x=x,y=y,label=paste(price,hands,sep = " : ")),size=3)+xlab(NULL)+theme(panel.background=element_blank())
    ## 2.plot corresponding orderbook change
    currentidx <- currentdata$tradetime%in%names(currentverbose)
    currentbook <- currentdata[currentidx,]
    if(currentorder$direction==1){
        currentbook <- currentbook[,c("tradetime",grep("bid",names(currentbook),value = TRUE))]
    }else{
        currentbook <- currentbook[,c("tradetime",grep("ask",names(currentbook),value = TRUE))]
    }
    prices <- currentbook[,2:((ncol(currentbook)-1)/2+1)]
    handss <- currentbook[,((ncol(currentbook)-1)/2+2):ncol(currentbook)]
    d2 <- data.frame(tradetime=character(),hands=numeric(),price=numeric(),stringsAsFactors = FALSE)
    for(i in 1:nrow(currentbook)){
        d2 <- rbind(d2,data.frame(tradetime=currentbook$tradetime[i],hands=as.numeric(handss[i,]),price=as.numeric(prices[i,]),stringsAsFactors = FALSE))
    }
    d2$x <- as.factor(strftime(d2$tradetime,format = "%H:%M:%OS"))
    d2$y <- rep((nrow(d2)/length(unique(d2$tradetime))):1,length(unique(d2$tradetime)))
    d2$label <- paste(d2$price,d2$hands,sep = " : ")
    p2 <- ggplot(d2)+geom_text(aes(x=x,y=y,label=label),size=2.5)+theme(panel.background=element_blank(),panel.grid.major=element_line(linetype = 4,color = "gray40"))+xlab(NULL)+ylab(NULL)
    
    ## 3. plot corresponding market data
    if(nrow(currentdata)>20){
        ## character is discrete but POSIXct is continuous
        currentdata$tradetime <- as.POSIXct(currentdata$tradetime)
    }
    mapper <- c(fair="black",tail="steelblue",head="darkred")
    currentdata$fairness <- mapper[currentdata$fairness]
    pmaxidx <- which.max(currentdata$lastprice)
    pminidx <- which.min(currentdata$lastprice)
    p3 <- ggplot(currentdata)+
        xlab(NULL)+ylab(NULL)+
        theme(plot.margin=unit(c(0.5,0.5,0,0.2),units = "in"))+
        theme(panel.background=element_blank(),legend.position = "none")+
        theme(panel.grid.major=element_line(linetype = 4,color = "gray40"))+
        scale_y_continuous(breaks=NULL)+
        geom_text(aes(x=tradetime,y=lastprice,label=lastprice),data=currentdata[pmaxidx,],vjust=0.5)+
        geom_text(aes(x=tradetime,y=lastprice,label=lastprice),data=currentdata[pminidx,],vjust=0.5)
    if(nrow(currentdata)>20){
        p3 <- p3+geom_line(aes(x=tradetime,y=lastprice))+scale_x_datetime(label=NULL)
    }else{       #break=prettyDate can't handle too small time period
        p3 <- p3+geom_line(aes(x=tradetime,y=lastprice,group="1"))+scale_x_discrete(label=NULL)
    }

    ## 3.5 order information
    currentstatus <- .tradingstates$orderhistory[.tradingstates$orderhistory$tradetime%in%names(currentverbose)&.tradingstates$orderhistory$orderid==orderid,]
    if(nrow(currentdata)>20){
        currentstatus$tradetime <- as.POSIXct(currentstatus$tradetime)
    }
    currentstatus$status <- as.character(currentstatus$status)
    p3 <- p3+geom_point(aes(x=tradetime,y=price,color=status,shape=status,fill=status),data = currentstatus)

    ## top left bottom right
    ## 4.volume
    p4 <- ggplot(currentdata)+geom_bar(aes(x=tradetime,y=volume,fill=fairness,color=fairness),stat="identity")+
        ylab(NULL)+xlab(NULL)+
        theme(plot.margin=unit(c(0,0.5,0.2,0.2),units = "in"))+
        theme(panel.background=element_blank())+
        theme(panel.grid.major=element_line(linetype = 4,color = "gray40"))+
        scale_y_continuous(breaks=NULL)+
        geom_text(aes(x=tradetime,y=volume,label=volume),data=currentdata[currentidx,],vjust=0.5)+
        scale_fill_identity()+
        scale_color_identity()
    if(nrow(currentdata)>20){
        p4 <- p4+scale_x_datetime()
    }else{       #break=prettyDate can't handle too small time period
          p4 <- p4+scale_x_discrete(label=strftime(currentdata$tradetime,format = "%M:%OS"),breaks=currentdata$tradetime)
    }

    ## scale_x_datetime(label=NULL)

    grid.newpage()
    pushViewport(viewport(layout=grid.layout(10,10)))
    print(p3,vp=vplayout(1:4,1:10))
    print(p4,vp=vplayout(5:6,1:10))
    print(p2,vp=vplayout(7:8,1:10))
    print(p1,vp=vplayout(9:10,1:10))
}

## ## 1. agents: output target holdings' signal
## ## 2. riskmanager
## ## 3. assetallocator

## agent <- function(){}
## riskmanager.instrument <- function(){}
## ## input: pnls of all instruments
## riskmanager.portfolio <- function(){}
## allocator <- function(){}

## agent.backtest <- function(){}

## ## series: numeric, price series
## ## trades: integer, indicating trade indexes in price series
## ## targetholdings: integer, target long or short holdings, length(targetholdings) must equal length(trades)
## ## pertradedd: numeric, each trade's max allowable drawdown
## ## cumdd: numeric, max allowable drawdown
## riskmanager.instrument.backtest <- function(series,trades,targetholdings,pertradedd,cumdd,slippage){
##     L <- length(series)
##     if(length(trades)!=length(targetholdings)){stop("unequal number of trades")}
##     ## 1. per trade, return pl of each trade
##     eachtrade <- sapply(1:length(trades),function(i){
##         pl <- (series[trades[i]:min(trades[i+1],L,na.rm=TRUE)]-series[trades[i]])*targetholdings[i]-slippage*abs(targetholdings[i])
##         STOP <- which(drawdown(pl)<=pertradedd)[1]
##         if(is.na(STOP)){
##             return(tail(pl,1))
##         }else{
##             return(pl[STOP])
##         }
##     })
##     ## 2. cumtrade
##     ## STOP <- which(drawdown(eachtrade)<=cumdd)[1]
##     STOP <- which(drawdown(cumsum(eachtrade))<=cumdd)[1]
##     if(is.na(STOP)){
##         return(eachtrade)
##     }else{
##         ## return(sum(eachtrade[1:STOP]))
##         return(eachtrade[1:STOP])
##     }
## }

## riskmanager.portfolio.backtest <- function(){}
## allocator.backtest <- function(){}
