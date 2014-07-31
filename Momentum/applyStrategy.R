require(PerformanceAnalytics)
#import data
path<- 'RawData//399006.csv'
s<-getHistoryData(path, f ='%Y/%m/%d')
s<-s[100:400,]
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
add.indicator(strategy = myStrgy,name = 'TrendPoint',arguments = list(mkt = s, r = 0.02))


add.signal(strategy = myStrgy, name="sigThreshold", 
           arguments=list(threshold = 1, column='longSig.TrendPoint.ind', relationship="eq"),
           label="longEntry")
add.signal(strategy = myStrgy, name="sigThreshold", 
           arguments=list(threshold = -1, column='longSig.TrendPoint.ind', relationship="eq"),
           label="longExit")



add.signal(strategy = myStrgy, name="sigThreshold", 
           arguments=list(threshold = 1, column='longInitSig.TrendPoint.ind', relationship="eq"),
           label="longInitEntry")
add.signal(strategy = myStrgy, name="sigThreshold", 
           arguments=list(threshold = -1, column='longInitSig.TrendPoint.ind', relationship="eq"),
           label="longInitExit")

add.signal(strategy = myStrgy, name="sigThreshold", 
           arguments=list(threshold = 1, column='shortInitSig.TrendPoint.ind', relationship="eq"),
           label="shortInitEntry")
add.signal(strategy = myStrgy, name="sigThreshold", 
           arguments=list(threshold = -1, column='shortInitSig.TrendPoint.ind', relationship="eq"),
           label="shortInitExit")
add.signal(strategy = myStrgy, name="sigThreshold", 
           arguments=list(threshold = 1, column='shortSig.TrendPoint.ind', relationship="eq"),
           label="shortEntry")
add.signal(strategy = myStrgy, name="sigThreshold", 
           arguments=list(threshold = -1, column='shortSig.TrendPoint.ind', relationship="eq"),
           label="shortExit")


add.rule(strategy = myStrgy , name="ruleSignal", 
         arguments=list(sigcol="longEntry", sigval=TRUE, orderqty='', 
                        ordertype="market", orderside="long", replace=FALSE, prefer="Close", osFUN = 'orderSize', TxnFees = -txnFees), 
         type="enter", path.dep=TRUE)

add.rule(strategy = myStrgy, name="ruleSignal", 
         arguments=list(sigcol="longExit", sigval=TRUE, orderqty="all", 
                        ordertype="market", orderside="long", replace=FALSE, prefer="Close"), 
         type="exit", path.dep=TRUE)




add.rule(strategy = myStrgy , name="ruleSignal", 
         arguments=list(sigcol="longInitEntry", sigval=TRUE, orderqty='', 
                        ordertype="market", orderside="long", replace=FALSE, prefer="Close", osFUN = 'orderSize', TxnFees = -txnFees), 
         type="enter", path.dep=TRUE)

add.rule(strategy = myStrgy, name="ruleSignal", 
         arguments=list(sigcol="longInitExit", sigval=TRUE, orderqty="all", 
                        ordertype="market", orderside="long", replace=FALSE, prefer="Close"), 
         type="exit", path.dep=TRUE)

add.rule(strategy = myStrgy , name="ruleSignal", 
         arguments=list(sigcol="shortEntry", sigval=TRUE, orderqty='', 
                        ordertype="market", orderside="short", replace=FALSE, prefer="Close", osFUN = 'orderSize', TxnFees = -txnFees), 
         type="enter", path.dep=TRUE)

add.rule(strategy = myStrgy, name="ruleSignal", 
         arguments=list(sigcol="shortExit", sigval=TRUE, orderqty="all", 
                        ordertype="market", orderside="short", replace=FALSE, prefer="Close"), 
         type="exit", path.dep=TRUE)

add.rule(strategy = myStrgy , name="ruleSignal", 
         arguments=list(sigcol="shortInitEntry", sigval=TRUE, orderqty='', 
                        ordertype="market", orderside="short", replace=FALSE, prefer="Close", osFUN = 'orderSize', TxnFees = -txnFees), 
         type="enter", path.dep=TRUE)

add.rule(strategy = myStrgy, name="ruleSignal", 
         arguments=list(sigcol="shortInitExit", sigval=TRUE, orderqty="all", 
                        ordertype="market", orderside="short", replace=FALSE, prefer="Close"), 
         type="exit", path.dep=TRUE)
#addPosLimit(portfolio = myPort,symbol = Symbol,maxpos = 5, timestamp = startDate)

t1 <- Sys.time()
out <- applyStrategy(strategy=myStrgy,portfolios=myPort, verbose=T)
t2 <- Sys.time()
print(t2-t1)
updatePortf(myPort)
updateAcct(myAcct)
updateEndEq(myAcct)  
chart.Posn(myPort,Symbol,theme=myTheme)
ts2<-getTxns(Portfolio=myPort, Symbol=Symbol)
View(ts2)
ob <- getOrderBook(portfolio = myPort)
ob

tstats <- tradeStats(Portfolio=myPort, Symbol=Symbol)

tab.trades <- cbind(
  c("Trades","Win Percent","Loss Percent","W/L Ratio"),
  c(tstats[,"Num.Trades"],tstats[,c("Percent.Positive","Percent.Negative")],
    tstats[,"Percent.Positive"]/tstats[,"Percent.Negative"]))

tab.profit <- cbind(
  c("Net Profit","Gross Profits","Gross Losses","Profit Factor"),
  c(tstats[,c("Net.Trading.PL","Gross.Profits","Gross.Losses",
              "Profit.Factor")]))

tab.wins <- cbind(
  c("Avg Trade","Avg Win","Avg Loss","Avg W/L Ratio"),
  c(tstats[,c("Avg.Trade.PL","Avg.Win.Trade","Avg.Losing.Trade",
              "Avg.WinLoss.Ratio")]))

trade.stats.tab <- data.frame(tab.trades,tab.profit,tab.wins)
View(trade.stats.tab)

