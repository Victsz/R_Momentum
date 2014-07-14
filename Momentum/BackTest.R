#import data
path<- 'RawData//399006.csv'
s<-getHistoryData(path, f ='%Y/%m/%d')
#s<-s['2014-01-27/']
s$VOLUME<-NULL
waves<-generateWave(s, r=0.02)
curves <- getCurves(waves)
upCurve <- curves[[1]]
downCurve <- curves[[2]]


myTheme<-chart_theme()
myTheme$col$dn.col<-'red'
myTheme$col$dn.border <- 'red'
myTheme$col$up.col <- 'lightgray'
myTheme$col$up.border <- 'lightgray'
chartSeries(x=s,name='a')
addTA(upCurve,on =1, col='yellow', lwd=2)
addTA(downCurve,on =1, col='red', lwd=2)




s$trend<-trend$value
s$dire<-trend$d
s$point<-trend$point
s$peak<-trend$peakV
require(blotter)
Symbol<-'s'
currency('CNY')
get('CNY',envir=FinancialInstrument:::.instrument)
stock(Symbol,currency = 'CNY', multiplier=1)
get(Symbol,envir=FinancialInstrument:::.instrument)
Sys.setenv(TZ = 'UTC')
startDate <- '2001-01-01'

if (!exists('.blotter')) .blotter <- new.env()
rm(list=ls(envir=.blotter),envir=.blotter)
myAcct<-'myAcct'
myPort<-'myPortfolio'
initPortf(name=myPort,symbols=Symbol, initDate=startDate,currency='CNY')
initAcct(name= myAcct,portfolios=myPort,initDate=startDate,currency='CNY',initEq=10000)



entryPrice<-0
isStoped<-F
pices<-1
dominator<-3
bottom<-unique(s[s$dire==1,c('point')])
#Iteration
for( i in 1:nrow(s) )
{
  # update values for this date  
  CurrentDate <- as.POSIXct(index(s)[i])
  print(CurrentDate)
  equity = getEndEq(myAcct, CurrentDate)
  ClosePrice <- as.numeric(Cl(s[i,]))
  OpenPrice <- as.numeric(Op(s[i,]))
  Posn <- getPosQty(myPort, Symbol=Symbol, Date=CurrentDate)
  
  direN <- as.numeric(s[i,'dire'])
  
  # change market position if necessary
  if(i>=2) # if the moving average has begun
  {
    direO <- as.numeric(s[i-1,'dire'])
    if(direN==1 & direO == -1)
    {
      print('Up Trend')
      if(Posn == 0 & isStoped)
      {  
        isStoped = F 
      }
    }
    if(direN== -1 & direO ==1)
    {
      print('Down Trend')
    }
    
  
    if( Posn == 0 & !isStoped) 
    { # No position, test to go Long
      if( direN == 1 ) 
      {
        lastBottom <- coredata(Cl(s[max(bottom[bottom<i]),]))
        if(as.numeric(s[i-1,'dire']) == -1){
        cBottom <-  coredata(Cl(s[i,]))
        if(lastBottom<cBottom)
        {
          pices = min(pices+1,dominator);
          
        }else{
          
          pices = 1;
        }}
        investMoney<-(equity*pices)/dominator
        
        if(investMoney > 0){
        fee<-investMoney*0.01
        
        UnitSize = as.numeric(trunc(investMoney/ClosePrice))
        addTxn(myPort, Symbol=Symbol, TxnDate=CurrentDate,
               TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=-fee, verbose =T) 
        entryPrice<- ClosePrice}
      }
    }else if(Posn!=0){ # Have a position, so check exit
      isExit<-F
      if( direN == -1)
      {
        isExit = T
        # exit position
        lastBottom <- coredata(Cl(s[max(bottom[bottom<i]),]))
        if(ClosePrice<lastBottom){
       #   print('Former bottom stop')
          isExit = T
        }
      } 
#       peak<- s$peak[i]
#       if(ClosePrice< peak*(1-0.015)){
#         #stop Lose
#         print('Drawback stop')
#         isExit = T
#       } else {
#         if(ClosePrice> entryPrice*(1+0.08)){
#           #stop Lose
#           print('Take profit')
#           isExit = T
#         }else if(coredata(Op(s[i,])) < (coredata(Cl(s[i-1,]))*(1-0.003))){
#           if(ClosePrice< coredata(Cl(s[i-1,]))){
#             isExit = T
#             print('Gap Stop')}
#           
#         }
#       }
      
      if(isExit){
        isStoped = T
        addTxn(myPort, Symbol=Symbol, TxnDate=CurrentDate,
               TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0, verbose =T  ) 
        isExit = F
      }
    }
    
   
    
    
 
    # Calculate P&L and resulting equity with blotter
    
    updatePortf(myPort,Dates = CurrentDate)
    updateAcct(myAcct, Dates = CurrentDate)
    updateEndEq(myAcct, Dates = CurrentDate)
  } # End dates loop
}

chart.Posn(myPort,Symbol,theme=myTheme)

ts<-getTxns(Portfolio=myPort, Symbol=Symbol)
View(ts)
tstats <- tradeStats(Portfolio=myPort, Symbol=Symbol)


# tab.trades <- cbind(
#   c("Trades","Win Percent","Loss Percent","W/L Ratio"),
#   c(tstats[,"Num.Trades"],tstats[,c("Percent.Positive","Percent.Negative")],
#     tstats[,"Percent.Positive"]/tstats[,"Percent.Negative"]))
# 
# tab.profit <- cbind(
#   c("Net Profit","Gross Profits","Gross Losses","Profit Factor"),
#   c(tstats[,c("Net.Trading.PL","Gross.Profits","Gross.Losses",
#               "Profit.Factor")]))
# 
# tab.wins <- cbind(
#   c("Avg Trade","Avg Win","Avg Loss","Avg W/L Ratio"),
#   c(tstats[,c("Avg.Trade.PL","Avg.Win.Trade","Avg.Losing.Trade",
#               "Avg.WinLoss.Ratio")]))
# trade.stats.tab <- data.frame(tab.trades,tab.profit,tab.wins)
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
