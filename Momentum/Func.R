#install.packages("quantstrat", repos="http://R-Forge.R-project.org")
#install.packages("lubridate")
require("lubridate")
require(blotter)

require(quantstrat)

last <- function(x) { tail(x, n = 1) }
calStep <- function(trend,dire, useClose = F)
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

drawTrend <- function (startPoint,trendLine,start,end,dire,step,lineStart = -1) 
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
getTrendLine <- function(trends,s)
{ 
  upTrendLine<- rep(NA, nrow(s))
  downTrendLine<- rep(NA, nrow(s))
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
      end <- nrow(s)
    }
    breakPoint <- trend$breakPoint
    step <- trend$step
    if(dire==1){
      upTrendLine<-drawTrend(startPoint = s[start], trendLine = upTrendLine,start = start, end = end,dire = dire,step = step)
    }else
    {
      downTrendLine<-drawTrend(startPoint = s[start], trendLine = downTrendLine,start = start, end = end,dire = dire,step = step)
    }
  }
  names(upTrendLine) <- index(s)
  upTrendLine<-upTrendLine[!is.na(upTrendLine)]
  names(downTrendLine) <- index(s)
  downTrendLine<-downTrendLine[!is.na(downTrendLine)]
  
  return (list(upTrendLine,downTrendLine))
}
getFirstWave <- function (cl,hi, lo, length, r) {
  waveEnd<-0
  initDire <- 0
  peak <- hi[1]
  peakI <- 1
  bottom <- lo[1]
  bottomI <- 1
  for(i in 2:length)
  {
    if(hi[i] > peak)
    {
      peak <- hi[i]
      peakI <- i
    } else if(hi[i] == peak)
    {
      peakI <- c(peakI,i)
    }
    
    if(lo[i] < bottom)
    {
      bottom <- lo[i]
      bottomI <- i
    } else if(lo[i] == bottom)
    {
      bottomI <- c(bottomI,i)
    }
    if((cl[i] - cl[1]) / cl[1] > r)
    {
      initDire <- 1
      waveEnd <- i      
      break
    } else if ( (cl[1] - cl[i]) / cl[1] > r)
    {
      initDire <- -1
      waveEnd <- i
      break
    }
  }
  
  firstWave <-list(dire = initDire,waveStart = 1,waveEnd = waveEnd, peak = peak,bottom = bottom, peakI = peakI, bottomI  = bottomI)
  return(firstWave)
}

reverseWave <- function(lastWave,curV,r, i)
  {
    cl<-coredata(Cl(curV))
    lo<-coredata(Lo(curV))
    hi<-coredata(Hi(curV))
    dropPerc <- (lastWave$peak - cl) /  lastWave$peak
    risePerc <- (cl - lastWave$bottom ) /  lastWave$bottom    
    newWave <- NULL
    if(lastWave$dire == 1 & dropPerc > r)
    {
      #new down wave  
      newWave <-list(dire = -1,waveStart = i,waveEnd = i,peak = hi, bottom = lo, peakI =i, bottomI = i)
    }
    
    if(lastWave$dire == -1 & risePerc > r)
    {
      #new up wave  
      newWave <-list(dire = 1,waveStart = i,waveEnd = i,peak = hi, bottom = lo, peakI =i, bottomI = i)  
    }
    
    return (newWave) 
  }
findNewTrend <- function(curWave,prevWave1,prevWave2,s,r,i, lastDire)
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
    }
  }
  
  if(findTrend)
  {
    step <- 0
    start <- start - 1
    print(cl)
    print( r * cl)
   
    while (abs(step) < r * cl )
    {      
      start <- start +1
      if(start>= breakPoint)
      {
        return (newTrend)
      }
      step <- calStep(trend = s[start:breakPoint], dire = dire)  
      print(abs(step))
      if(is.na(abs(step) < r * cl)){print('NA')}
    }
   
    newTrend <- data.frame(dire, start,end, breakPoint, step, type, extrema = extrema) 
  }
  
  return (newTrend)
}
generateWave <- function(s,r = 0.001)
{
  length <- nrow(s)
  cl <- coredata(Cl(s))
  lo <- coredata(Lo(s))
  hi <- coredata(Hi(s))
  firstWave <- getFirstWave(cl, hi, lo, length, r)
  waves <- list(firstWave)
  iStart <- firstWave$waveEnd + 1

  for(i in iStart : length)
  {
    #check for reverse
    waveCount <- length(waves)
    lastWave <- last(waves)[[1]]
    newWave <- reverseWave(lastWave,s[i],r,i)
   
    if(!is.null(newWave))
    {
      #new wave
      start <- lastWave$waveStart
      valueLength <- (i - start + 1)
      value <- seq(from = cl[start], to= cl[i], length.out =  valueLength)
      names(value) <- index(s)[start:i]
      curve <- data.frame(value)
      lastWave$curve<-curve
      waves[[waveCount]] <- lastWave
      waves <- append(waves,list(newWave))
      next
    }else{
      #update current wave
      lastWave$waveEnd <- i
      
      if(lastWave$peak < hi[i])
      {
        lastWave$peak <- hi[i]
        lastWave$peakI <- i
      } else if(lastWave$peak == hi[i])
      { 
        lastWave$peakI <- c(lastWave$peakI,i)
      }
      
      if(lastWave$bottom > lo[i])
      {
        lastWave$bottom <- lo[i]
        lastWave$bottomI <- i
      } else if(lastWave$bottom == lo[i])
      { 
        lastWave$bottomI <- c(lastWave$bottomI,i)
      }
      if(i==length)
      {
        start <- lastWave$waveStart
        valueLength <- (i - start + 1)
        value <- seq(from = cl[start], to= cl[i], length.out =  valueLength)
        names(value) <- index(s)[start:i]
        curve <- data.frame(value)
        lastWave$curve<-curve
      }
      waves[[waveCount]] <- lastWave
    }
  }  
  return(waves)
}

getCurves <- function(waves)
{
  upCurve <- NULL
  downCurve <- NULL
  
  length <- length(waves)

  for(i in 1:length)
  {
    wave <- waves[[i]]
    if(wave$dire == 1)
    {
      upCurve <- rbind(upCurve,wave$curve)
      
    }
    else
    {
      downCurve <- rbind(downCurve,wave$curve)
    }
  }
  return (list(upCurve = upCurve, downCurve = downCurve))
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
      step <- calStep(trend = s[start:breakPoint], dire = dire)
      firstTrend <- data.frame(dire, start,end, breakPoint, step, type = 'Init',extrema = extrema)
      break
    }
  }
  return (firstTrend)
}

generateTrends <- function(s,waves,r = 0.001)
{
  firstTrend <- getFirstTrend(s,waves,r)
  trends <- firstTrend
  waveCount <- length(waves)
  length <- nrow(s)
  w <- getWaveByIndex(firstTrend$breakPoint,waves)
  for(i in firstTrend$breakPoint : length)
  {
    cl <- coredata(Cl(s[i]))
    lastTrend <- last(trends)
    trendCount <- nrow(trends)
    curWave <- waves[[w]]
    if(!is.na(lastTrend$end))
    {
      #last Trend ended
      prevWave1 <- waves[[w-1]]
      prevWave2 <- waves[[w-2]]
    
      lastDire <- lastTrend$dire
      newTrend <- findNewTrend(curWave=curWave,prevWave1 =  prevWave1,prevWave2 =  prevWave2,s =  s,r = r,i = i,lastDire = lastDire)
      if(!is.null(newTrend))
      {
        trends <- rbind(trends,newTrend)
      }
    }else 
    {
     
      
      # test if tend end
      dire <- lastTrend$dire
      if(dire == 1)
      {
        # handle step is NA, ie start & break
        start <- lastTrend$start
        bottom <- coredata(Lo(s[start]))   
        if(is.na(lastTrend$step))
        {
          if(bottom>cl)
          {
            # end trend
            lastTrend$step <- 0
            lastTrend$end <- i
          }else
          {
            step <- calStep(trend = s[start:i], dire = dire)
            lastTrend$step <- step
          }
        }else {
         
          step <- lastTrend$step
             
          curPoint <- bottom + step * (i - start)
          isContinusBreak <- cl<curPoint & coredata(Cl(s[i-1])) < curPoint - step &  coredata(Cl(s[i-2])) < curPoint - step * 2
          if(isContinusBreak | cl < curPoint * (1 - r))
          {
            #break Trend, end trend
            lastTrend$end <- i
          }else {
            # to do record trend High 
            hi <- coredata(Hi(s[i]))
            if(lastTrend$extrema < hi)
            {
              lastTrend$extrema <- hi
            }
          }
        }
       
       
        
      } else if(dire == -1)
      {
        start <- lastTrend$start
       
        peak <- coredata(Hi(s[start])) 
        if(is.na(lastTrend$step))
        {
          if(peak<cl)
          {
            # end trend
            lastTrend$step <- 0
            lastTrend$end <- i
          }else
          {
            step <- calStep(trend = s[start:i], dire = dire)
            lastTrend$step <- step
          }
        }else
        {
          step <- lastTrend$step
          curPoint <- peak + step * (i - start)
          isContinusBreak <- cl>curPoint & coredata(Cl(s[i-1])) > curPoint - step &  coredata(Cl(s[i-2])) > curPoint - step * 2
          if(isContinusBreak | cl > curPoint * (1 + r))
          {
            #break Trend, end trend
            lastTrend$end <- i
          }else {
            # to do record trend low
            lo <- coredata(Lo(s[i]))
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

getWaveByIndex <- function(i,waves)
{
  waveCount <- length(waves)
  for(w in 1:waveCount)
  {    
    wave <- waves[[w]]     
    if(wave$waveEnd >= i & wave$waveStart <=i )
    {
      return (w)
    }  
  }
  return (NULL)
}
Trend<-
function(df,r = 0.001)
{
  curTrend <- 1
  isDraw <- F
  waveCount <- 1
  peakList<-NULL
  bottomList<-NULL
  waveDireList<-NULL
  
  peak<-0
  bottom<-0

  
  cl<-Cl(df)
  length <-length(cl)
  lo<-Lo(df)
  hi<-Hi(df)
  td<- seq(length.out =length, by = 0)
  dire<- seq(length.out =length, by = 0)
  point <-seq(length.out =length, by = 0)
  value<- seq(length.out =length, by = 0)
  trendLine<-seq(length.out =length, by = 0)
  print(length(trendLine))
  
  mainDire<- seq(length.out =length, by = 0)
  value[1] <- coredata(cl[1]) 
  
  point[1]<-1
  strength <- 1
  peak <- coredata(hi[1]); 
  bottom <- coredata(cl[1]); 
   
  
  dire[1] <- 1
  mainDire[1] <- 1
  waveDireList<-c('U',waveDireList)
  bottomList<-c(lo[1],bottomList)
  peakList<-c(hi[1],peakList)
  trendStartIndex <- 1
  for(i in 2:length(cl))
  { 
    isReverse <- F 
    if(dire[i-1] == -1)
    {
      if(bottom>coredata(lo[i]))
      {
        bottom <- coredata(lo[i])   
        bottomList[waveCount]<- lo[i]
      }   
      
      #strength<- as.numeric(1+(peak - bottom)/peak)
      l<-bottom*(1+r*strength)
      clo<-coredata(cl[i])
      if(coredata(l)< coredata(clo))
      {
        #reverse        
        waveDireList<-c(waveDireList,'U')
        if(coredata(lo[i]) <= coredata(bottomList[waveCount]))
        {
          bottomList <- c(bottomList,lo[i])          
        } else 
          {
          bottomList <- c(bottomList,bottomList[waveCount])
        } 
        peakList<-c(peakList, hi[i])
        waveCount <- waveCount + 1
          
        isReverse <- T
        dire[i] = 1
        point[i] = i
        
        start <- point[i-1]
        end<- i -1
        clo <- coredata(cl[end])
        if(start<end){
          startCl <-coredata(cl[start])
          nVal<- seq(from = startCl , to = clo, length.out  = (end - start)+1)
                  
          nlist<-c((start):(start+length(nVal) -1))
     
          value = replace(x = value, list = nlist, values =  nVal)
        }else{
          
          value[i-1] = coredata(cl[i-1])
        }
        peak<-coredata(hi[i])
        
        if(!isDraw)
        {
          isDraw <- T    
          
          
          start <- trendStartIndex 
          end<- i 
          subTrend <- df[start:end,]
          step<-calStep(subTrend,-1)   
            trendLine <- drawTrend(startPoint = df[start],trendLine = trendLine, 
                                   start = start, end = end,dire = -1, step = step, lineStart = i)
        }
        
      }else {
        dire[i] = -1
        point[i] = point[i-1]
        mainDire[i] = mainDire[i-1] 
        if(mainDire[i] == 1 )
        {        
          if(!is.na(trendLine[i]) & trendLine[i]> coredata(cl[i]))
          {
            #trend line break no longer valid
            trendLine[i:length(trendLine)]<-NA              
          }
          
     
        }else if(mainDire[i-1] == -1 )  { 
          if(is.na(trendLine[i]))
          {
          if(coredata(cl[i])<bottomList[waveCount-1])
          {
            
              # new low redraw trend line
              start <-  trendStartIndex
              end<- i 
              subTrend <- df[start:end,]
              step<-calStep(subTrend,-1)
              trendLine <- drawTrend(startPoint =  df[start],trendLine =  trendLine, start =  start,end =  end,dire =  -1,step =  step)             
            }        
            
          }
        }
      }
    } else
      {
      #dire == 1
      if(coredata(hi[i])>peak)
      {
        peak = coredata(hi[i])
        peakList[waveCount]<-hi[i]
      } 
      
     # strength <- as.numeric(peak/bottom)
      h<- peak*(1 - r* (strength))
      clo<-coredata(cl[i])
      
      if(coredata(h)>clo)
      {
        #Reverse
        waveDireList<-c(waveDireList,'U')
        if(coredata(hi[i]) >= coredata(peakList[waveCount]))
        {
          peakList <- c(bottomList,hi[i])          
        } else 
        {
          peakList <- c(peakList,peakList[waveCount])
        } 
        
        bottomList<-c(bottomList,lo[i]) 
        
        if(coredata(hi[i]) > coredata(peakList[waveCount]))
        {
          peakList<-c(peakList,hi[i])
        } else {
          peakList<-c(peakList,peakList[waveCount])
          
        }
      
        waveCount <- waveCount + 1
                        
        isReverse <- T
        dire[i] = -1
        point[i] = i
        
        start <- point[i-1] 
        end<- i 
        clo <- coredata(cl[end])
        if(start<end){
          startCl <-coredata(cl[start])
          nVal<- seq(from = startCl, to = clo, length.out  = (end - start)+1)
                   
          nlist<-c((start):(start+length(nVal) -1))
        
         value= replace(x = value, list = nlist, values =  nVal )
        }else
        {          
          value[end] = coredata(cl[end])
        }
        bottom<-coredata(lo[i])
        
        if(!isDraw)
        {
          isDraw <- T
          start <- trendStartIndex    
          end<- i 
          step <- calStep(df, 1)
          trendLine <- drawTrend (startPoint =  df[start],trendLine =  trendLine,start =  start,end =  end,dire =  1, step =  step) 
          
        }
      }else
      {      
        dire[i] = 1
        point[i] = point[i-1]
        mainDire[i] = mainDire[i-1] 
        if(mainDire[i] == -1 )
        {
          if(!is.na(trendLine[i]) & trendLine[i] < coredata(cl[i]))
          {
            #trend line break no longer valid
            trendLine[i:length(trendLine)]<-NA              
          }
        }else if(mainDire[i] == 1 )
        {
          
          if(is.na(trendLine[i]))
          {
          if(coredata(cl[i])>peakList[waveCount-1])
          {
            
              # new High redraw trend line
              start <-  trendStartIndex
              end <- i 
              subTrend <- df[start:end,]
              step<-calStep(subTrend,1)
              trendLine <- drawTrend(startPoint =  df[start],trendLine =  trendLine,start = start,end =  end,dire = 1, step =  step,lineStart =i )   
            }
          }
        }
      }     
      
    }
    
    if(mainDire[i-1] == -1 & waveCount>1)
    {
      lastPeak <- peakList[waveCount-1]
      if(coredata(cl[i]) > coredata(lastPeak))
      {
        # new Up Trend
        trendStartIndex <- point[i]
        
        isDraw <- F
        
        mainDire[i] <- 1
      } else {
        mainDire[i] <- -1
      }      
      
    }
    else  if(waveCount>1){
      #mainDire == 1
  
      lastBottom <- bottomList[waveCount-1]
      if(coredata(cl[i]) < coredata(lastBottom))
      {
        # new down Trend
        trendStartIndex <- point[i]
        
        isDraw <- F
        
        mainDire[i] <- -1
      } else {
        mainDire[i] <- 1
      }      
    }
  }
  # Tail
  if(point[length]<=length){
  start <- point[length]
  end<- length

  if(start<end){
    
    start <- point[i-1] 
    end<- length
    clo <- coredata(cl[end])
    startCl <-coredata(cl[start])
   
    nVal<- seq(from = startCl, to = clo, length.out  = (end - start)+1)
    nlist<-c((start):(start+length(nVal) -1))
    value= replace(x = value, list = nlist, values =  nVal )
  }else
  {    
    value[length] =  coredata(cl[length])
  }}
  print(length(trendLine))
  
  trend<-data.frame(dire,point,value,trendLine)
  
  return (trend)
  
}

getHistoryData<- function(x, f = '%d/%m/%Y %H:%M:%S')
{
  z<-read.zoo(x,format = f,header = TRUE,index.column = 1, sep = ',', FUN=as.POSIXct)
  return(as.xts(z))
}

# drawTrend <- function (trend,s) {
#   n<-data.frame(row.names = index(s),trend$value,trend$dire)
#   nu<-n[which(n$trend.dire==1),]
#   nd<-n[which(n$trend.dire!=1),]
#   nu$trend.dire<-NULL
#   nd$trend.dire<-NULL
#   myTheme<-chart_theme()
#   myTheme$col$dn.col<-'red'
#   myTheme$col$dn.border <- 'red'
#   myTheme$col$up.col <- 'lightgray'
#   myTheme$col$up.border <- 'lightgray'
#   chartSeries(x=s,name='a')
#   addTA(nu,on =1, col='yellow', lwd=2)
#   addTA(nd,on =1, col='red', lwd=2)
# }