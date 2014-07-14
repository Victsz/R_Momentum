require("lubridate")
require(blotter)
#install.packages("quantstrat", repos="http://R-Forge.R-project.org")
require(quantstrat)

getHistoryData<- function(x, f = '%d/%m/%Y %H:%M:%S')
{
  z<-read.zoo(x,format = f,header = TRUE,index.column = 1, sep = ',', FUN=as.POSIXct)
  return(as.xts(z))
}

Trend<-
function(df,r = 0.001)
{
  peak<-0
  bottom<-0

  cl<-Cl(df)
  length <-length(cl)
  lo<-Lo(df)
  hi<-Hi(df)
  dire<- seq(length.out =length, by = 0)
  point <-seq(length.out =length, by = 0)
  value<- seq(length.out =length, by = 0)
  value[1] <- coredata(hi[1]); 
  point[1]<-1
  
  peak <- coredata(cl[1]); 
  bottom <- coredata(cl[1]); 
  for(i in 2:length(cl))
  {
    if(dire[i-1] == -1)
    {
      if(bottom>coredata(lo[i])){bottom = coredata(lo[i])}  
      l<-bottom*(1+r)
      clo<-coredata(cl[i])
      if(coredata(l)< coredata(clo))
      {
        #reverse
        dire[i] = 1;
        point[i] = i;
     
        start <- point[i-1]
        end<- i -1
        clo <- coredata(cl[end])
        if(start<end){
          startCl <-coredata(cl[start])
          nVal<- seq(from = startCl , to = clo, length.out  = (end - start)+1)
                     print(length(nVal))
          nlist<-c((start):(start+length(nVal) -1))
          print(nlist)
          value = replace(x = value, list = nlist, values =  nVal );
        }else{
          
          value[i-1] = coredata(cl[i-1]);
        }
        peak<-coredata(cl[i])
      }else
      {
        dire[i] = -1;   
        point[i] = point[i-1];
          
      }      
     
    }else{
      if(coredata(hi[i])>peak){peak = coredata(hi[i])}
      h<- peak*(1 - r)
      clo<-coredata(cl[i])
      if( coredata(h)>clo )
      {
        #Reverse
        dire[i] = -1;
        point[i] = i;
       
        
        start <- point[i-1] 
        end<- i - 1
        clo <- coredata(cl[end])
        if(start<end){
          startCl <-coredata(cl[start])
          nVal<- seq(from = startCl, to = clo, length.out  = (end - start)+1)
                     print(length(nVal))
          nlist<-c((start):(start+length(nVal) -1))
          print(nlist)
         value= replace(x = value, list = nlist, values =  nVal );
        }else
        {          
          value[i-1] = coredata(cl[i-1]);
        }
        bottom<-coredata(cl[i])
      }else
      {      
        dire[i] = 1;   
        point[i] = point[i-1];

      }     
      
    }
    
  }
  if(point[length]<=length){
  start <- point[length]
  end<- length
  print(end - (start+1) )
  print('length')
  if(start<end){
    
    start <- point[i-1] 
    end<- length
    clo <- coredata(cl[end])
    startCl <-coredata(cl[start])
   
    nVal<- seq(from = startCl, to = clo, length.out  = (end - start)+1)
    nlist<-c((start):(start+length(nVal) -1))
    value= replace(x = value, list = nlist, values =  nVal );
  }else
  {    
    value[length] =  coredata(cl[length]);
  }}
  trend<-data.frame(dire,point,value)
  return (trend)
  
}

# 
# 
path<- 'RawData//399006.csv'
s<-getHistoryData(path, f ='%Y/%m/%d')
# s<-s['2013-11-01/']
# path<- 'RawData//PALK5.csv'
# s<-getHistoryData(path)
# x<-to.minutes10(s)
# l<-dim(s)[1]
# s<-s[(l-200):l]
trend<-Trend(s, r=0.02)
n<-data.frame(row.names = index(s),trend$value,trend$dire)
# 
# 
# 
myTheme<-chart_theme()
myTheme$col$dn.col<-'red'
myTheme$col$dn.border <- 'red'
myTheme$col$up.col <- 'lightgray'
myTheme$col$up.border <- 'lightgray'
nu<-n[which(n$trend.dire==1),]
nd<-n[which(n$trend.dire!=1),]
nu$trend.dire<-NULL
nd$trend.dire<-NULL
chartSeries(x=s,name='a')
addTA(nu,on =1, col='green', lwd=2)
addTA(nd,on =1, col='red', lwd=2)
s$trend<-trend$value
s$dire<-trend$d
s$point<-trend$point
require(blotter)
currency('CNY')
get('CNY',envir=FinancialInstrument:::.instrument)
stock("SPY",currency = 'CNY', multiplier=1)
get('SPY',envir=FinancialInstrument:::.instrument)
Sys.setenv(TZ = 'UTC')
startDate <- '2001-01-01'
endDate <- '2014-03-01'
if (!exists('.blotter')) .blotter <- new.env()
rm(list=ls(envir=.blotter),envir=.blotter)
myAcct<-'myAcct'
myPort<-'myPortfolio'
initPortf(name=myPort,symbols='SPY', initDate=startDate,currency='CNY')
initAcct(name= myAcct,portfolios=myPort,initDate=startDate,currency='CNY',initEq=10000)


SPY<-s
entryPrice<-0
isStoped<-F
pices<-1
dominator<-4
bottom<-unique(s[s$dire==1,c('point')])
#Iteration
for( i in 1:nrow(s) )
{
  # update values for this date
  CurrentDate <- as.POSIXct(index(s)[i])
  
  equity = getEndEq(myAcct, CurrentDate)
  ClosePrice <- as.numeric(Cl(s[i,]))
  OpenPrice <- as.numeric(Op(s[i,]))
  Posn <- getPosQty(myPort, Symbol='SPY', Date=CurrentDate)
  
  direN <- as.numeric(s[i,'dire'])

  # change market position if necessary
  if(i>=2) # if the moving average has begun
  {
    direO <- as.numeric(s[i-1,'dire'])
    if( Posn == 0 & !isStoped) 
    { # No position, test to go Long
      if( direN == 1 ) 
      {
        lastBottom <- coredata(Cl(s[max(bottom[bottom<i]),]))
        cBottom <-  coredata(Cl(s[i,]))
        if(lastBottom<cBottom)
        {
          pices = min(pices+1,dominator);
          
        }else{
          
          pices = 1;
        }
        UnitSize = (as.numeric(trunc(equity/ClosePrice))*pices)/dominator
        addTxn(myPort, Symbol='SPY', TxnDate=CurrentDate,
               TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0) 
        entryPrice<- ClosePrice
      }
    }else{ # Have a position, so check exit
      if( direN == -1)
      {
        # exit position
        addTxn(myPort, Symbol='SPY', TxnDate=CurrentDate,
               TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0) 
      }else{
        if( ClosePrice< entryPrice){
        #stop Lose
        isStop = T
        addTxn(myPort, Symbol='SPY', TxnDate=CurrentDate,
               TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0) 
        print('Stop Lose')
      }
    }
  }
  
  if(Posn == 0 & isStoped)
  {
    
    if( direN == -1)
    {
      isStoped = F
      
    }
  }
  # Calculate P&L and resulting equity with blotter
  
  updatePortf(myPort,Dates = CurrentDate)
  updateAcct(myAcct, Dates = CurrentDate)
  updateEndEq(myAcct, Dates = CurrentDate)
} # End dates loop
}

chart.Posn(myPort,'SPY',theme=myTheme)

ts<-getTxns(Portfolio=myPort, Symbol="SPY")

tstats <- tradeStats(Portfolio=myPort, Symbol="SPY")
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

require(PerformanceAnalytics)
rets <- PortfReturns(Account = myAcct)
rownames(rets) <- NULL
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
                              "Calmar Ratio")
)

# tab.risk <- table.Arbitrary(rets,
#                             metrics=c(
#                               "StdDev.annualized",
#                               "maxDrawdown",
#                               "VaR",
#                               "ES"),
#                             metricsNames=c(
#                               "Annualized StdDev",
#                               "Max DrawDown",
#                               "Value-at-Risk",
#                               "Conditional VaR"))
