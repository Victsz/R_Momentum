require(PerformanceAnalytics)

isDraw <- T
range <- 0.02
r <- 0.015
stopThreshold <- 0.03

#import data
path<- 'RawData//399006.csv'
formate = '%Y/%m/%d'
# s<-getHistoryData(path, f ='%Y/%m/%d')
s<-getHistoryData(path, f =formate)

txnFees <- 1900

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
add.indicator(strategy = myStrgy,name = 'TrendPoint',arguments = list(mkt = s, r = r, dayAdvance = 1))

#Short
#Enter
add.signal(strategy = myStrgy, name="sigThreshold", 
           arguments=list(threshold = 1, column='shortPoint.TrendPoint.ind', relationship="eq"),
           label="shortSig")

add.signal(strategy = myStrgy, name="sigThreshold", 
           arguments=list(threshold = 1, column='shortInitPoint.TrendPoint.ind', relationship="eq"),
           label="shortInitSig")


add.signal(strategy = myStrgy, name="sigFormula", 
           arguments=list(formula="(shortInitSig | shortSig)",cross = F),
           label="shortBegin")

add.rule(strategy = myStrgy , name="ruleSignal",label = 'shortEntry', 
         arguments=list(sigcol="shortBegin", sigval=TRUE, orderqty='', 
                        ordertype="market", 
                        orderside="short",
                        replace=FALSE, prefer="Close", osFUN = 'orderSize', TxnFees = 'getTxnFeeStock'), 
         type="enter")

#Exit
add.signal(strategy = myStrgy, name="sigThreshold", 
           arguments=list(threshold = -1, column='shortPoint.TrendPoint.ind', relationship="eq"),
           label="shortSig2")

add.signal(strategy = myStrgy, name="sigThreshold", 
           arguments=list(threshold = -1, column='shortInitPoint.TrendPoint.ind', relationship="eq"),
           label="shortInitSig2")


add.signal(strategy = myStrgy, name="sigFormula", 
           arguments=list(formula="(shortInitSig2 | shortSig2)",cross = F),
           label="shortEnd")


add.rule(strategy = myStrgy, name="ruleSignal", label = 'shortExit', type="exit",
         arguments=list(sigcol="shortEnd", 
                        sigval=TRUE,
                        replace=FALSE, 
                        orderside="short",
                        orderqty="all",
                        tmult= T,  
                        ordertype="market", 
                        prefer="Close",
                        orderset ='ocoshort')        
)

# Stop & Trailling

add.rule(strategy = myStrgy, name="ruleSignal", label = 'shortStop', type="chain",parent = 'shortEntry',
         arguments=list(sigcol="shortBegin", 
                        sigval=TRUE,
                        replace=FALSE, 
                        orderside="short",
                        orderqty="all",
                        tmult= T, 
                        threshold= stopThreshold,
                        ordertype="stoplimit", 
                        prefer="Open",
                        orderset ='ocoshort')         
)

add.rule(strategy = myStrgy, name="ruleSignal", label = 'shorttrailing', type="chain", parent = 'shortEntry',
         arguments=list(sigcol="shortBegin", 
                        sigval=TRUE,
                        replace=FALSE, 
                        orderside="short",
                        orderqty="all",
                        tmult= T, 
                        threshold= stopThreshold,
                        ordertype="stoptrailing", 
                        prefer="Open",
                        orderset ='ocoshort')        
)

#long
#Enter
add.signal(strategy = myStrgy, name="sigThreshold", 
           arguments=list(threshold = 1, column='longPoint.TrendPoint.ind', relationship="eq"),
           label="longSig")

add.signal(strategy = myStrgy, name="sigThreshold", 
           arguments=list(threshold = 1, column='longInitPoint.TrendPoint.ind', relationship="eq"),
           label="longInitSig")


add.signal(strategy = myStrgy, name="sigFormula", 
           arguments=list(formula="(longInitSig | longSig)",cross = F),
           label="longBegin")

add.rule(strategy = myStrgy , name="ruleSignal",label = 'longEntry', 
         arguments=list(sigcol="longBegin", sigval=TRUE, orderqty='', 
                        ordertype="market", 
                        orderside="long",
                        replace=FALSE, prefer="Close", osFUN = 'orderSize', TxnFees = 'getTxnFeeStock'), 
         type="enter")

#Exit
add.signal(strategy = myStrgy, name="sigThreshold", 
           arguments=list(threshold = -1, column='longPoint.TrendPoint.ind', relationship="eq"),
           label="longSig2")

add.signal(strategy = myStrgy, name="sigThreshold", 
           arguments=list(threshold = -1, column='longInitPoint.TrendPoint.ind', relationship="eq"),
           label="longInitSig2")


add.signal(strategy = myStrgy, name="sigFormula", 
           arguments=list(formula="(longInitSig2 | longSig2)",cross = F),
           label="longEnd")


add.rule(strategy = myStrgy, name="ruleSignal", label = 'longExit', type="exit",
         arguments=list(sigcol="longEnd", 
                        sigval=TRUE,
                        replace=FALSE, 
                        orderside="long",
                        orderqty="all",
                        tmult= T,  
                        ordertype="market", 
                        prefer="Close",
                        orderset ='ocolong')        
)

# Stop & Trailling

add.rule(strategy = myStrgy, name="ruleSignal", label = 'longStop', type="chain",parent = 'longEntry',
         arguments=list(sigcol="longBegin", 
                        sigval=TRUE,
                        replace=FALSE, 
                        orderside="long",
                        orderqty="all",
                        tmult= T, 
                        threshold= stopThreshold,
                        ordertype="stoplimit", 
                        prefer="Open",
                        orderset ='ocolong')         
)

add.rule(strategy = myStrgy, name="ruleSignal", label = 'longtrailing', type="chain", parent = 'longEntry',
         arguments=list(sigcol="longBegin", 
                        sigval=TRUE,
                        replace=FALSE, 
                        orderside="long",
                        orderqty="all",
                        tmult= T, 
                        threshold= stopThreshold,
                        ordertype="stoptrailing", 
                        prefer="Open",
                        orderset ='ocolong')        
)



t1 <- Sys.time()
out <- applyStrategy(strategy=myStrgy,portfolios=myPort, verbose=T)
t2 <- Sys.time()
print(t2-t1)
updatePortf(myPort)
updateAcct(myAcct)
updateEndEq(myAcct) 




myTheme<-NULL
myTheme <- chart_theme()
myTheme$col$dn.col<-'bisque4'
myTheme$col$up.col <- 'coral3'
chart.Posn(myPort,Symbol,theme=myTheme)


waves<-generateWaves(s, r=r)
trends <- generateTrends(s,waves = waves, r= r)
trendLine <- getTrendLine(trends,s,range = range) 

if(isDraw){
  curves <- getWaveCurve(waves)
  upCurve <- curves[[1]]
  downCurve <- curves[[2]]
  
  myTheme<-chart_theme()
  myTheme$col$dn.col<-'red'
  myTheme$col$dn.border <- 'red'
  myTheme$col$up.col <- 'lightgray'
  myTheme$col$up.border <- 'lightgray'
  chartSeries(x=s,name='a')
  addTA(upCurve,on =1, col='yellow', lwd=0.5)
  addTA(downCurve,on =1, col='red', lwd=0.5)
  
  addTA(trendLine$up,on =1, col='cyan', lwd=2)
  addTA(trendLine$upR,on =1, col='cyan', lwd=2)
  addTA(trendLine$down,on =1, col='coral', lwd=2)
  addTA(trendLine$downR,on =1, col='coral', lwd=2)
  addTA(trendLine$dash,on =1, col='beige', lwd=2)
}

trends$startDate <- index(s)[trends$start]
trends$breakDate <- index(s)[trends$breakPoint]
trends$endDate <- index(s)[trends$end]


ts<-getTxns(Portfolio=myPort, Symbol=Symbol)

ob <- getOrderBook(portfolio = myPort)

View(ob$myPortfolio)
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
