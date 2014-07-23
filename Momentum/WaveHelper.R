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

findReverseWave <- function(lastWave,curV,r, i)
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

generateWaves <- function(mkt,r = 0.001)
{
  length <- nrow(mkt)
  cl <- coredata(Cl(mkt))
  lo <- coredata(Lo(mkt))
  hi <- coredata(Hi(mkt))
  firstWave <- getFirstWave(cl, hi, lo, length, r)
  waves <- list(firstWave)
  iStart <- firstWave$waveEnd + 1
  
  for(i in iStart : length)
  {
    #check for reverse
    waveCount <- length(waves)
    lastWave <- getLast(waves)[[1]]
    newWave <- findReverseWave(lastWave,mkt[i],r,i)
    
    if(!is.null(newWave))
    {
      #new wave
      # 1 cal wave curve
      start <- lastWave$waveStart
      valueLength <- (i - start + 1)
      value <- seq(from = cl[start], to= cl[i], length.out =  valueLength)
      names(value) <- index(mkt)[start:i]
      curve <- data.frame(value)
      lastWave$curve<-curve
      waves[[waveCount]] <- lastWave
      # 2 append new wave
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
        # handle the last
        start <- lastWave$waveStart
        valueLength <- (i - start + 1)
        value <- seq(from = cl[start], to= cl[i], length.out =  valueLength)
        names(value) <- index(mkt)[start:i]
        curve <- data.frame(value)
        lastWave$curve<-curve
      }
      waves[[waveCount]] <- lastWave
    }
  }  
  return(waves)
}

getWaveCurve <- function(waves)
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