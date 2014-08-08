require(PerformanceAnalytics)

isDraw <- T
range <- 0.002
r <- 0.0015
stopThreshold <- 0.005
speedRatio <- 0.2
#import data
path<- 'RawData//IFBQ4.csv'
formate = '%d/%m/%Y %H:%M'
# s<-getHistoryData(path, f ='%Y/%m/%d')
IFBQ4<-getHistoryData(path, f =formate) 
IFBQ4 <- IFBQ4[100:200,]
txnFees <- 19

#init portfolio
require(blotter)
Symbol<-'IFBQ4'
s<-get(Symbol)

CNY <- 'CNY'
currency(CNY)
get(CNY,envir=FinancialInstrument:::.instrument)
stock(primary_id = Symbol, currency = CNY, multiplier=300) 
get(Symbol,envir=FinancialInstrument:::.instrument)

Sys.setenv(TZ = 'UTC')
startDate <- '2001-01-01'

if (!exists('.blotter')) .blotter <- new.env()
rm(list=ls(envir=.blotter),envir=.blotter)
myAcct<-'myAcct'
myPort<-'myPortfolio'
initPortf(name=myPort,symbols=Symbol, initDate=startDate,currency=CNY)
initAcct(name= myAcct,portfolios=myPort,initDate=startDate,currency=CNY,initEq=100000)


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
add.indicator(strategy = myStrgy,name = 'TrendPoint',arguments = list(mkt = s, r = r, dayAdvance = 0, range = range, speedRatio=speedRatio))

#Short
#Enter
add.signal(strategy = myStrgy, name="sigComparison", 
           arguments=list(columns=c('down.TrendPoint.ind','Close'), relationship="gte"),
           label="underDown")
add.signal(strategy = myStrgy, name="sigThreshold", 
           arguments=list(threshold = 1, column='shortPoint.TrendPoint.ind', relationship="eq"),
           label="shortSig")

add.signal(strategy = myStrgy, name="sigThreshold", 
           arguments=list(threshold = 1, column='shortInitPoint.TrendPoint.ind', relationship="eq"),
           label="shortInitSig")
add.signal(strategy = myStrgy, name="sigThreshold", 
           arguments=list(threshold = -1, column='shortPoint.TrendPoint.ind', relationship="eq"),
           label="shortSig2")

# add.signal(strategy = myStrgy, name="sigFormula", 
#            arguments=list(formula="(shortInitSig | shortSig)",cross = F),
#            label="shortBegin")
add.signal(strategy = myStrgy, name="sigFormula", 
           arguments=list(formula="underDown & !shortSig2",cross = F),
           label="shortBegin")

add.rule(strategy = myStrgy , name="ruleSignal",label = 'shortEntry', 
         arguments=list(sigcol="shortBegin", sigval=TRUE, orderqty='', 
                        ordertype="limit", 
                        orderside="short",
                        replace=T, prefer="downPrice.TrendPoint.ind", osFUN = 'orderSizeIFB',
                        orderset ='ocoshort'), 
         type="enter")
#Re Enter

add.signal(strategy = myStrgy, name="sigComparison", 
           arguments=list(columns=c('downR.TrendPoint.ind','Close'), relationship="lte"),
           label="aboveDownR")


add.signal(strategy = myStrgy, name="sigFormula", 
           arguments=list(formula="(underDown & aboveDownR)",cross = F),
           label="shortAgain")
add.rule(strategy = myStrgy , name="ruleSignal",label = 'shortEntry', 
         arguments=list(sigcol="shortAgain", sigval=TRUE, orderqty='', 
                        ordertype="market", 
                        orderside="short",
                        replace=T, prefer="Close", osFUN = 'orderSizeIFB',
                        orderset ='ocoshort'), 
         type="enter")

#Exit


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
                        orderset ='ocoshort', TxnFees = 'getTxnFee')        
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
                        orderset ='ocoshort', TxnFees = 'getTxnFee')         
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
                        orderset ='ocoshort', TxnFees = 'getTxnFee')        
)

#long
#Enter
add.signal(strategy = myStrgy, name="sigThreshold", 
           arguments=list(threshold = 1, column='longPoint.TrendPoint.ind', relationship="eq"),
           label="longSig")

add.signal(strategy = myStrgy, name="sigThreshold", 
           arguments=list(threshold = 1, column='longInitPoint.TrendPoint.ind', relationship="eq"),
           label="longInitSig")

add.signal(strategy = myStrgy, name="sigThreshold", 
           arguments=list(threshold = -1, column='longPoint.TrendPoint.ind', relationship="eq"),
           label="longSig2")

add.signal(strategy = myStrgy, name="sigComparison", 
           arguments=list(columns=c('up.TrendPoint.ind','Close'), relationship="lte"),
           label="aboveUp")
# 
# add.signal(strategy = myStrgy, name="sigFormula", 
#            arguments=list(formula="(longInitSig | longSig)",cross = F),
#            label="longBegin")

add.signal(strategy = myStrgy, name="sigFormula", 
           arguments=list(formula="aboveUp & !longSig2",cross = F),
           label="longBegin")
add.rule(strategy = myStrgy , name="ruleSignal",label = 'longEntry', 
         arguments=list(sigcol="longBegin", sigval=TRUE, orderqty='', 
                        ordertype="limit", 
                        orderside="long",
                        replace=T, prefer="upPrice.TrendPoint.ind", osFUN = 'orderSizeIFB', orderset = 'ocolong'), 
         type="enter")
#Re Enter

add.signal(strategy = myStrgy, name="sigComparison", 
           arguments=list(columns=c('upR.TrendPoint.ind','Close'), relationship="gte"),
           label="underUpR")


add.signal(strategy = myStrgy, name="sigFormula", 
           arguments=list(formula="(aboveUp & underUpR)",cross = F),
           label="longAgain")
add.rule(strategy = myStrgy , name="ruleSignal",label = 'longEntry', 
         arguments=list(sigcol="longAgain", sigval=TRUE, orderqty='', 
                        ordertype="market", 
                        orderside="long",
                        replace=T, prefer="Close", osFUN = 'orderSizeIFB', orderset = 'ocolong'), 
         type="enter")
#Exit


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
                        orderset ='ocolong', TxnFees = 'getTxnFee')        
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
                        orderset ='ocolong', TxnFees = 'getTxnFee')         
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
                        orderset ='ocolong', TxnFees = 'getTxnFee')        
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
myTheme$col$dn.border <- 'bisque4'
myTheme$col$up.col <- 'coral3'
myTheme$col$up.border <- 'coral3'
chart.Posn(myPort,Symbol,theme=myTheme,TA = '') 
# 
# if(isDraw){
#   curves <- getWaveCurve(waves)
#   upCurve <- curves[[1]]
#   downCurve <- curves[[2]]
#   
#   myTheme<-chart_theme()
#   myTheme$col$dn.col<-'red'
#   myTheme$col$dn.border <- 'red'
#   myTheme$col$up.col <- 'lightgray'
#   myTheme$col$up.border <- 'lightgray'
#   chartSeries(x=s,name='a')
#   addTA(upCurve,on =1, col='yellow', lwd=0.5)
#   addTA(downCurve,on =1, col='red', lwd=0.5)
#   
#    addTA(trendLine$up,on =1, col='cyan', lwd=2)
# #   addTA(trendLine$upR,on =1, col='cyan', lwd=2)
#   addTA(trendLine$down,on =1, col='coral', lwd=2)
# #   addTA(trendLine$downR,on =1, col='coral', lwd=2)
#   addTA(trendLine$dash,on =1, col='beige', lwd=2)
# }
