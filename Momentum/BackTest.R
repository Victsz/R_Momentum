require(PerformanceAnalytics)
#import data
path<- 'RawData//002010.csv'
s<-getHistoryData(path, f ='%Y/%m/%d')
#s<-s[100:200,]
s$VOLUME<-NULL
waves<-generateWaves(s, r=0.015)
trends <- generateTrends(s,waves = waves, r=0.01/2)
trendLine <- getTrendLine(trends,s) 
isDraw <- F
if(isDraw){
curves <- getWaveCurve(waves)
upCurve <- curves[[1]]
downCurve <- curves[[2]]

trends$startDate <- index(s)[trends$start]
trends$breakDate <- index(s)[trends$breakPoint]
trends$endDate <- index(s)[trends$end]

myTheme<-chart_theme()
myTheme$col$dn.col<-'red'
myTheme$col$dn.border <- 'red'
myTheme$col$up.col <- 'lightgray'
myTheme$col$up.border <- 'lightgray'
chartSeries(x=s,name='a')
addTA(upCurve,on =1, col='yellow', lwd=0.5)
addTA(downCurve,on =1, col='red', lwd=0.5)

addTA(trendLine$up,on =1, col='cyan', lwd=2)
addTA(trendLine$down,on =1, col='coral', lwd=2)
addTA(trendLine$dash,on =1, col='beige', lwd=2)
}

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
txnFeesR <- 0.0


#Iteration
for( i in 2:nrow(s) )
{
  isSell <- F
  isBuy  <- T
  curDate <- as.POSIXct(index(s)[i])
  if(i < nrow(s))
  {
    nextDate <- as.POSIXct(index(s)[i+1])
    nextOp <- as.numeric(Op(s[i+1,]))
  }else
  {
    nextDate <- NULL
    nextOp <- NULL
  }

  equity = getEndEq(myAcct, curDate) 
  cl <- as.numeric(Cl(s[i,]))
  posn <- getPosQty(myPort, Symbol=Symbol, Date=curDate)
  
  isSell <- checkIsSell(posn =  posn, cl = cl, curDate = curDate,trends = trends,i = i)
  isBuy <- checkIsBuy(cl = cl, curDate = curDate,trends = trends,i = i)
  if(isSell & !is.null(nextDate))
  {
    addTxn(myPort, Symbol=Symbol, TxnDate=nextDate,
           TxnPrice=nextOp, TxnQty = -posn , TxnFees= -cl*posn*txnFeesR) 
  }
  if(posn <= 0 & isBuy & !is.null(nextDate))
  {
     addTxn(myPort, Symbol=Symbol, TxnDate=nextDate,
           TxnPrice=nextOp, TxnQty = 5 , TxnFees=0) 
  } 
#   updatePortf(myPort,Dates = curDate)
#   updateAcct(myAcct, Dates = curDate)
#   updateEndEq(myAcct, Dates = curDate)   
   
}
updatePortf(myPort)
updateAcct(myAcct)
updateEndEq(myAcct)  
chart.Posn(myPort,Symbol,theme=myTheme)

ts<-getTxns(Portfolio=myPort, Symbol=Symbol)


View(ts)
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


rets <- PortfReturns(Account=myAcct)
rownames(rets) <- NULL
tail(rets)
charts.PerformanceSummary(rets,colorset = bluefocus)
tab.perf <- table.Arbitrary(rets,
                            metrics=c(
                              "Return.cumulative",
                              "Return.annualized",
                              "SharpeRatio.annualized",
                              "CalmarRatio"),
                            metricsNames=c(
                              "Cumulative Return",
                              "Annualized Return",
                              "Annualized Sharpe Ratio",
                              "Calmar Ratio"))
tab.perf 

tab.risk <- table.Arbitrary(rets,
                            metrics=c(
                              "StdDev.annualized",
                              "maxDrawdown",
                              "VaR",
                              "ES"),
                            metricsNames=c(
                              "Annualized StdDev",
                              "Max DrawDown",
                              "Value-at-Risk",
                              "Conditional VaR"))
tab.risk

performance.stats.tab <- data.frame(
  rownames(tab.perf),tab.perf[,1],
  rownames(tab.risk),tab.risk[,1])
View(performance.stats.tab)
# 
# require(PerformanceAnalytics)
# rets <- PortfReturns(Account = myAcct)
# rownames(rets) <- NULL
# charts.PerformanceSummary(rets,colorset = bluefocus)
# 
# 
# tab.perf <- table.Arbitrary(rets,
#                             metrics=c(
#                               "Return.cumulative",
#                               "Return.annualized",
#                               "SharpeRatio.annualized",
#                               "CalmarRatio"),
#                             metricsNames=c(
#                               "Cumulative Return",
#                               "Annualized Return",
#                               "Annualized Sharpe Ratio",
#                               "Calmar Ratio")
# )
# 
# # tab.risk <- table.Arbitrary(rets,
# #                             metrics=c(
# #                               "StdDev.annualized",
# #                               "maxDrawdown",
# #                               "VaR",
# #                               "ES"),
# #                             metricsNames=c(
# #                               "Annualized StdDev",
# #                               "Max DrawDown",
# #                               "Value-at-Risk",
# #                               "Conditional VaR"))
