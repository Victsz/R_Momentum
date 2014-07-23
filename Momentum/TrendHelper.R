getTrendLine <- function(trends,mkt)
{ 
  upTrendLine<- rep(NA, nrow(mkt))
  downTrendLine<- rep(NA, nrow(mkt))
  dashLine<- rep(NA, nrow(mkt))
  lastEnd <- 0
  trendCount <- nrow(trends)
  for(t in 1 : trendCount)
  {
    trend <- trends[t,]
    dire <- trend$dire    
    start <- trend$start    
    end <- trend$end
    if(is.na(end))
    {
      end <- nrow(mkt)
    }
    breakPoint <- trend$breakPoint
    step <- trend$step
    if(dire==1){
      upTrendLine<-calTrendValue(startPoint = mkt[start], trendLine = upTrendLine,start = start, end = end,dire = dire,step = step, lineStart = breakPoint)
    }else
    {
      downTrendLine<-calTrendValue(startPoint = mkt[start], trendLine = downTrendLine,start = start, end = end,dire = dire,step = step, lineStart = breakPoint)
    }
    dashLine <-calTrendValue(startPoint = mkt[start], trendLine = dashLine,start = start, end = breakPoint -1 ,dire = dire,step = step)
  }
  # connect from breakpoint to end
  names(upTrendLine) <- index(mkt)
  upTrendLine<-upTrendLine[!is.na(upTrendLine)]
  names(downTrendLine) <- index(mkt)
  downTrendLine<-downTrendLine[!is.na(downTrendLine)]
  names(dashLine) <- index(mkt)
  dashLine<-dashLine[!is.na(dashLine)]
  return (list(up = upTrendLine,down = downTrendLine,dash = dashLine))
}

calTrendValue <- function (startPoint,trendLine,start,end,dire,step,lineStart = -1) 
{
  inL<-length(trendLine)  
  
  if(lineStart <= 0)
  {
    lineStart <- start
  }
  startV <-ifelse(dire==1,coredata(Lo(startPoint)),coredata(Hi(startPoint)))
  nVal<- seq(from =startV ,length.out = end - start +1, by = step)
  
  
  if(lineStart  > start){
    subS <- lineStart - start + 1
    nVal <- nVal[subS: length(nVal)]  
  } 
  nlist<-c((lineStart):(lineStart+length(nVal) -1))
  
  trendLine <- replace(x = trendLine, list = nlist, values =  nVal )
  
  outL<-length(trendLine)
  
  if(inL!=outL)
  {
    print('Ex')
    print(inL)
    print(outL)
    print(start)
    print(lineStart)
    print(nlist)
  }
  return(trendLine)
  
}

findNewTrend <- function(curWave,prevWave1,prevWave2,s,r,i, lastDire, initStart)
{ 
  newTrend <- NULL
  breakPoint <- i
  findTrend <- F
  curV <- s[i]
  cl <- coredata(Cl(curV))
  dire <- NA
  extrema <- NA
  start <- NA
  end <- NA
  type <- NA
  curWaveStart <- curWave$waveStart
  curWaveEnd <- curWave$waveEnd
  waveEnd <- 0
  
  
  if(cl > (1 +r) * max(prevWave1$peak, prevWave2$peak))
  {
    #break, new up trend
    findTrend <- T 
    dire <- 1
    start <- curWaveStart
    extrema <- coredata(Hi(curV))[1]
    
    type <- ifelse(lastDire == dire,'Extend', 'Init')
    if(type == 'Init')
    {
      if(coredata(Lo(s[start])) > prevWave1$bottom * (1+r))
      {
        start <- prevWave1$bottomI[1]
      } 
      if(coredata(Lo(s[start])) > prevWave2$bottom* (1+r))
      {
        start <- prevWave2$bottomI[1]
      }
    }else
    {
      start <- initStart
    }
  }else if(cl < (1-r) * min(prevWave1$bottom, prevWave2$bottom))
  {
    #break, new down trend
    findTrend <- T
    dire <- -1
    start <- curWaveStart
    extrema <- coredata(Lo(curV))[1]
    type <- ifelse(lastDire == dire,'Extend', 'Init')
    if(type == 'Init')
    {
      if(coredata(Hi(s[start])) < prevWave1$peak* (1-r))
      {
        start <- prevWave1$peakI[1]
      }
      if(coredata(Hi(s[start])) < prevWave2$peak* (1-r))
      {
        start <- prevWave2$peakI[1]
      }
    }else
    {
      start <- initStart
    }
  }
  
  if(findTrend)
  {
    # filter trend with low degree
    step <- 0
    start <- start - 1 
    
    while (abs(step) < r * cl )
    {      
      start <- start +1
      if(start>= breakPoint)
      {
        return (newTrend)
      }
      step <- calTrendStep(trend = s[start:breakPoint], dire = dire)  
   
    }
    
    newTrend <- data.frame(dire, start,end, breakPoint, step, type, extrema = extrema) 
  }
  
  return (newTrend)
}

getFirstTrend <- function(s,waves,r)
{ 
  waveCount <- length(waves)
  findTrend <- F
  start <- NA
  end <- NA
  breakPoint <- NA
  dire <- NA
  extrema <- NA
  for(w in 3:waveCount)
  {
    prevWave1 <- waves[[w-1]]
    prevWave2 <- waves[[w-2]]
    curWave <- waves[[w]]   
    curWaveStart <- curWave$waveStart 
    curWaveEnd <- curWave$waveEnd
    firstTrend <- NULL
    for(i in curWaveStart : curWaveEnd)
    {
      breakPoint <- i
      cl <- coredata(Cl(s[i]))
      if(cl > (1 +r) * max(prevWave1$peak, prevWave2$peak))
      {
        #break, new up trend
        findTrend <- T 
        dire <- 1
        start <- curWaveStart
        extrema <- coredata(Hi(s[i]))[1]
        break
      }else if(cl < (1-r) * min(prevWave1$bottom, prevWave2$bottom))
      {
        #break, new down trend
        findTrend <- T
        dire <- -1
        start <- curWaveStart
        extrema <- coredata(Lo(s[i]))[1]
        break
      }
    }
    if(findTrend)
    {
      step <- 0
      start <- start - 1 
      # filter trend with low degree
      while (abs(step) < r * cl )
      {      
        start <- start +1
        if(start>= breakPoint)
        {
          findTrend <- F
          break
        }
        step <- calTrendStep(trend = s[start:breakPoint], dire = dire)  
        
      }
    }
    if(findTrend)
    {
      step <- calTrendStep(trend = s[start:breakPoint], dire = dire)
      
      firstTrend <- data.frame(dire, start,end, breakPoint, step, type = 'Init',extrema = extrema)
      break
    }
  }
  return (firstTrend)
}

generateTrends <- function(mkt,waves,r = 0.001)
{
  firstTrend <- getFirstTrend(mkt,waves,r)
  trends <- firstTrend
  waveCount <- length(waves)
  length <- nrow(mkt)
  w <- getWaveByIndex(firstTrend$breakPoint,waves)
  for(i in firstTrend$breakPoint : length)
  {
    cl <- coredata(Cl(mkt[i]))
    lastTrend <- getLast(trends)
    trendCount <- nrow(trends)
    curWave <- waves[[w]]
    if(!is.na(lastTrend$end))
    {
      #last Trend ended
      prevWave1 <- waves[[w-1]]
      prevWave2 <- waves[[w-2]]
      lastInitStart <- trends$start[max(which(x = trends$type=='Init'))]
      lastDire <- lastTrend$dire
      newTrend <- findNewTrend(curWave=curWave,prevWave1 =  prevWave1,prevWave2 =  prevWave2,s =  s,r = r,i = i,lastDire = lastDire,initStart = lastInitStart)
      if(!is.null(newTrend))
      {
        trends <- rbind(trends,newTrend)
      }
    }else 
    {     
      # test if trend end
      dire <- lastTrend$dire
      if(dire == 1)
      {
        # handle step is NA, ie start & break
        start <- lastTrend$start
        bottom <- coredata(Lo(mkt[start]))   
        if(is.na(lastTrend$step))
        {
          if(bottom>cl)
          {
            # end trend
            lastTrend$step <- 0
            lastTrend$end <- i
          }else
          {
            step <- calTrendStep(trend = mkt[start:i], dire = dire)
            lastTrend$step <- step
          }
        }else{
          
          step <- lastTrend$step
          
          curPoint <- bottom + step * (i - start)
          isContinusBreak <- cl<curPoint & coredata(Cl(mkt[i-1])) < curPoint - step &  coredata(Cl(mkt[i-2])) < curPoint - step * 2
          if(isContinusBreak | cl < curPoint * (1 - r))
          {
            #break Trend, end trend
            lastTrend$end <- i
          }else {
            # to do record trend High 
            hi <- coredata(Hi(mkt[i]))
            if(lastTrend$extrema < hi)
            {
              lastTrend$extrema <- hi
            }
          }
        }
        
        
        
      } else if(dire == -1)
      {
        start <- lastTrend$start
        
        peak <- coredata(Hi(mkt[start])) 
        if(is.na(lastTrend$step))
        {
          if(peak<cl)
          {
            # end trend
            lastTrend$step <- 0
            lastTrend$end <- i
          }else
          {
            step <- calTrendStep(trend = mkt[start:i], dire = dire)
            lastTrend$step <- step
          }
        }else
        {
          step <- lastTrend$step
          curPoint <- peak + step * (i - start)
          isContinusBreak <- cl>curPoint & coredata(Cl(mkt[i-1])) > curPoint - step &  coredata(Cl(mkt[i-2])) > curPoint - step * 2
          if(isContinusBreak | cl > curPoint * (1 + r))
          {
            #break Trend, end trend
            lastTrend$end <- i
          }else {
            # to do record trend low
            lo <- coredata(Lo(mkt[i]))
            if(lastTrend$extrema > lo)
            {
              lastTrend$extrema <- lo
            }
          }
          
        }
        
        
      }
      trends[trendCount,] <- lastTrend
      
    }
    
    if(i == curWave$waveEnd)
    {
      w <- w+1
    }
  }
  return(trends)
}

calTrendStep <- function(trend,dire, useClose = F)
{
  step <- NA
  
  rowCount <- nrow(trend)
  if(rowCount==1)
  {
    return (step)
  }
  startLo <- coredata(Lo(trend[1]))
  startHi <- coredata(Hi(trend[1]))
  for(i in 2: rowCount)
  {
    if(dire == 1)
    {
      if(is.na(step))
      {
        step <- 999999
      }
      nStep <-(coredata(Lo(trend[i])) - startLo)/i
      step <- min(step,nStep)
    }else {
      if(is.na(step))
      {
        step <- -999999
      }
      nStep <-(coredata(Hi(trend[i])) - startHi)/i
      step <- max(step,nStep)
      
    }
  }
  
  if(dire == 1 & step<0)
  {
    step <- 0    
  }
  
  if(dire == -1 & step > 0)
  {
    step <- 0    
  }
  
  return(step)
}

TrendLineIndicator <- function(trends,mkt)
{ 
  upTrendLine<- rep(9999, nrow(mkt))
  downTrendLine<- rep(NA, nrow(mkt))
  dashLine<- rep(NA, nrow(mkt))
  lastEnd <- 0
  trendCount <- nrow(trends)
  for(t in 1 : trendCount)
  {
    trend <- trends[t,]
    dire <- trend$dire    
    start <- trend$start    
    end <- trend$end
    if(is.na(end))
    {
      end <- nrow(mkt)
    }
    breakPoint <- trend$breakPoint
    step <- trend$step
    if(dire==1){
      upTrendLine<-calTrendValue(startPoint = mkt[start], trendLine = upTrendLine,start = start, end = end,dire = dire,step = step, lineStart = breakPoint)
    }else
    {
      downTrendLine<-calTrendValue(startPoint = mkt[start], trendLine = downTrendLine,start = start, end = end,dire = dire,step = step, lineStart = breakPoint)
    }
    dashLine <-calTrendValue(startPoint = mkt[start], trendLine = dashLine,start = start, end = breakPoint -1 ,dire = dire,step = step)
  }
  # connect from breakpoint to end
  #names(upTrendLine) <- index(mkt)
  #upTrendLine<-upTrendLine[!is.na(upTrendLine)]
#  names(downTrendLine) <- index(mkt)
#  downTrendLine<-downTrendLine[!is.na(downTrendLine)]
 # names(dashLine) <- index(mkt)
#  dashLine<-dashLine[!is.na(dashLine)]
  out <- cbind(upTrendLine, downTrendLine)
  colnames(out) <- c("up", "down")
  return (out)
}

TrendLine <- function(mkt, r)
{
  waves<-generateWaves(mkt = mkt, r=0.015)
  trends <- generateTrends(mkt = mkt,waves = waves, r=0.01/2)
  trendLine <- TrendPointIndicator(trends,nrow(mkt))
  trendLine <- xts(x = trendLine,index(s))  
  print(trendLine)
  return (trendLine)
}


TrendPointIndicator <- function(trends, dLength)
{
  print(dLength)
  longPoint <-  rep(0, dLength)
  shortPoint <-  rep(0, dLength)
  trendCount <- nrow(trends)
  for(t in 1 : trendCount)
  {
    trend <- trends[t,]
    dire <- trend$dire 
    end <- trend$end
    if(is.na(end))
    {
      end <- nrow(mkt)
    }
    breakPoint <- trend$breakPoint
    print(breakPoint)
    print(end)
    if(dire==1)
    {
      longPoint[breakPoint] <- 1
      longPoint[end] <- -1
    }else
    {
      shortPoint[breakPoint] <- 1
      shortPoint[end] <- -1
    }
  }
  out <- cbind(longPoint, shortPoint)
  colnames(out) <- c("longSig", "shortSig")
  return (out)
}