require(PerformanceAnalytics)
#import data
path<- 'RawData//002010.csv'
s<-getHistoryData(path, f ='%Y/%m/%d')
#s<-s[100:200,]
s$VOLUME<-NULL
txnFees <- 100

#init portfolio
require(blotter)
Symbol<-'s'
CNY <- 'CNY'
currency(CNY)
get(CNY,envir=FinancialInstrument:::.instrument)
stock(primary_id = Symbol, currency = CNY, multiplier=1) 
get(Symbol,envir=FinancialInstrument:::.instrument)

Sys.setenv(TZ = 'UTC')
startDate <- '2001-01-01'

if (!exists('.blotter')) .blotter <- new.env()
rm(list=ls(envir=.blotter),envir=.blotter)
myAcct<-'myAcct'
myPort<-'myPortfolio'
initPortf(name=myPort,symbols=Symbol, initDate=startDate,currency=CNY)
initAcct(name= myAcct,portfolios=myPort,initDate=startDate,currency=CNY,initEq=10000)


require(quantstrat)
if (!exists('.strategy')) .strategy <- new.env()
rm(list=ls(envir=.strategy),envir=.strategy)
myStrgy <- 'trendLine'
rm.strat(myStrgy)

initOrders(portfolio = myPort,symbols = Symbol,initDate = startDate)

strategy(name = myStrgy,store = T)

ls(.strategy)
strat <-getStrategy(myStrgy)
summary(strat)
add.indicator(strategy = myStrgy,name = 'TrendLine',arguments = list(mkt = s, r = 0.015))


add.signal(strategy = myStrgy, name="sigComparison", 
           arguments=list(columns=c('Close','up.TrendLine.ind'), relationship="gte"),
           label="longEntry")
add.signal(strategy = myStrgy, name="sigComparison", 
           arguments=list( columns=c('Close','up.TrendLine.ind'), relationship="lt"),
           label="longExit")

add.rule(strategy = myStrgy , name="ruleSignal", 
         arguments=list(sigcol="longEntry", sigval=TRUE, orderqty='', 
                        ordertype="market", orderside="long", replace=FALSE, prefer="Open", osFUN = 'orderSize', TxnFees = txnFees), 
         type="enter", path.dep=TRUE)

add.rule(strategy = myStrgy, name="ruleSignal", 
         arguments=list(sigcol="longExit", sigval=TRUE, orderqty="all", 
                        ordertype="market", orderside="long", replace=FALSE, prefer="Open"), 
         type="exit", path.dep=TRUE)

addPosLimit(portfolio = myPort,symbol = Symbol,maxpos = 5, timestamp = startDate)

t1 <- Sys.time()
out <- applyStrategy(strategy=myStrgy,portfolios=myPort, verbose=T)
t2 <- Sys.time()
print(t2-t1)
updatePortf(myPort)
updateAcct(myAcct)
updateEndEq(myAcct)  
chart.Posn(myPort,Symbol,theme=myTheme)
ts2<-cbind(getTxns(Portfolio=myPort, Symbol=Symbol))