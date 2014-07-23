#install.packages("quantstrat", repos="http://R-Forge.R-project.org")
#install.packages("lubridate")
require("lubridate")
require(blotter)

require(quantstrat)

checkIsSell <- function(posn,cl,curDate,trends,i)
{
  for(t in 1 : nrow(trends) )
  {
    trend <- trends[t,]
    start <- trend$start
    end <- trend$end
    breakPoint <- trend$breakPoint
    if(trend$dire != 1){next}
    if(i == end)
    {      
      return (T)
    }
    if(i<start){return (F)}
  }
  return (F)
}

checkIsBuy <- function(cl,curDate,trends,i)
{
#   isBuy <- F
  for(t in 1 : nrow(trends) )
  {
    trend <- trends[t,]
    start <- trend$start
    end <- trend$end
    breakPoint <- trend$breakPoint
    if(trend$dire != 1){next}
    if(i>=breakPoint & i < end)
    {return (T)}
    if(i < start){return (F)}
  }
{return (F)}
#   lineVal <- upLine[as.character.Date(curDate)]
#   if(!is.na(lineVal) &  cl > lineVal)
#   { 
#     isBuy <- T
#   }
#   
#   return (isBuy)   
}

orderSize <- function(data, timestamp, orderqty, ordertype, orderside, portfolio, symbol,
                      ruletype, ...)
{
  if(orderside == 'long')
  {
    posn <- getPosQty(Portfolio = portfolio, Symbol=Symbol, Date=timestamp)
    if(!is.na(posn) & posn>0)
    { 
      return(0)
    }
    else
    {
      
      return (10000/as.numeric(Cl(data[timestamp])))
    } 
  }else
  {
    posn <- getPosQty(Portfolio = portfolio, Symbol=Symbol, Date=timestamp)
    print(posn)
    if(!is.na(posn) & posn>0)
    { 
      return(0)
    }
    else
    {
      
      return (10000/as.numeric(Cl(data[timestamp])))
    } 
  }

}
getLast <- function(x) { tail(x, n = 1) }

getHistoryData<- function(x, f = '%d/%m/%Y %H:%M:%S')
{
  z<-read.zoo(x,format = f,header = TRUE,index.column = 1, sep = ',', FUN=as.POSIXct)
  return(as.xts(z))
}
