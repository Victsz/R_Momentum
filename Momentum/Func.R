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
    {return (T)}
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
#   lineVal <- upLine[as.character.Date(curDate)]
#   if(!is.na(lineVal) &  cl > lineVal)
#   { 
#     isBuy <- T
#   }
#   
#   return (isBuy)   
}


getLast <- function(x) { tail(x, n = 1) }

getHistoryData<- function(x, f = '%d/%m/%Y %H:%M:%S')
{
  z<-read.zoo(x,format = f,header = TRUE,index.column = 1, sep = ',', FUN=as.POSIXct)
  return(as.xts(z))
}

# 
# 
# Trend<-
# function(df,r = 0.001)
# {
#   curTrend <- 1
#   isDraw <- F
#   waveCount <- 1
#   peakList<-NULL
#   bottomList<-NULL
#   waveDireList<-NULL
#   
#   peak<-0
#   bottom<-0
# 
#   
#   cl<-Cl(df)
#   length <-length(cl)
#   lo<-Lo(df)
#   hi<-Hi(df)
#   td<- seq(length.out =length, by = 0)
#   dire<- seq(length.out =length, by = 0)
#   point <-seq(length.out =length, by = 0)
#   value<- seq(length.out =length, by = 0)
#   trendLine<-seq(length.out =length, by = 0)
#   print(length(trendLine))
#   
#   mainDire<- seq(length.out =length, by = 0)
#   value[1] <- coredata(cl[1]) 
#   
#   point[1]<-1
#   strength <- 1
#   peak <- coredata(hi[1]); 
#   bottom <- coredata(cl[1]); 
#    
#   
#   dire[1] <- 1
#   mainDire[1] <- 1
#   waveDireList<-c('U',waveDireList)
#   bottomList<-c(lo[1],bottomList)
#   peakList<-c(hi[1],peakList)
#   trendStartIndex <- 1
#   for(i in 2:(cl))
#   { 
#     isReverse <- F 
#     if(dire[i-1] == -1)
#     {
#       if(bottom>coredata(lo[i]))
#       {
#         bottom <- coredata(lo[i])   
#         bottomList[waveCount]<- lo[i]
#       }   
#       
#       #strength<- as.numeric(1+(peak - bottom)/peak)
#       l<-bottom*(1+r*strength)
#       clo<-coredata(cl[i])
#       if(coredata(l)< coredata(clo))
#       {
#         #reverse        
#         waveDireList<-c(waveDireList,'U')
#         if(coredata(lo[i]) <= coredata(bottomList[waveCount]))
#         {
#           bottomList <- c(bottomList,lo[i])          
#         } else 
#           {
#           bottomList <- c(bottomList,bottomList[waveCount])
#         } 
#         peakList<-c(peakList, hi[i])
#         waveCount <- waveCount + 1
#           
#         isReverse <- T
#         dire[i] = 1
#         point[i] = i
#         
#         start <- point[i-1]
#         end<- i -1
#         clo <- coredata(cl[end])
#         if(start<end){
#           startCl <-coredata(cl[start])
#           nVal<- seq(from = startCl , to = clo, length.out  = (end - start)+1)
#                   
#           nlist<-c((start):(start+length(nVal) -1))
#      
#           value = replace(x = value, list = nlist, values =  nVal)
#         }else{
#           
#           value[i-1] = coredata(cl[i-1])
#         }
#         peak<-coredata(hi[i])
#         
#         if(!isDraw)
#         {
#           isDraw <- T    
#           
#           
#           start <- trendStartIndex 
#           end<- i 
#           subTrend <- df[start:end,]
#           step<-calStep(subTrend,-1)   
#             trendLine <- drawTrend(startPoint = df[start],trendLine = trendLine, 
#                                    start = start, end = end,dire = -1, step = step, lineStart = i)
#         }
#         
#       }else {
#         dire[i] = -1
#         point[i] = point[i-1]
#         mainDire[i] = mainDire[i-1] 
#         if(mainDire[i] == 1 )
#         {        
#           if(!is.na(trendLine[i]) & trendLine[i]> coredata(cl[i]))
#           {
#             #trend line break no longer valid
#             trendLine[i:length(trendLine)]<-NA              
#           }
#           
#      
#         }else if(mainDire[i-1] == -1 )  { 
#           if(is.na(trendLine[i]))
#           {
#           if(coredata(cl[i])<bottomList[waveCount-1])
#           {
#             
#               # new low redraw trend line
#               start <-  trendStartIndex
#               end<- i 
#               subTrend <- df[start:end,]
#               step<-calStep(subTrend,-1)
#               trendLine <- drawTrend(startPoint =  df[start],trendLine =  trendLine, start =  start,end =  end,dire =  -1,step =  step)             
#             }        
#             
#           }
#         }
#       }
#     } else
#       {
#       #dire == 1
#       if(coredata(hi[i])>peak)
#       {
#         peak = coredata(hi[i])
#         peakList[waveCount]<-hi[i]
#       } 
#       
#      # strength <- as.numeric(peak/bottom)
#       h<- peak*(1 - r* (strength))
#       clo<-coredata(cl[i])
#       
#       if(coredata(h)>clo)
#       {
#         #Reverse
#         waveDireList<-c(waveDireList,'U')
#         if(coredata(hi[i]) >= coredata(peakList[waveCount]))
#         {
#           peakList <- c(bottomList,hi[i])          
#         } else 
#         {
#           peakList <- c(peakList,peakList[waveCount])
#         } 
#         
#         bottomList<-c(bottomList,lo[i]) 
#         
#         if(coredata(hi[i]) > coredata(peakList[waveCount]))
#         {
#           peakList<-c(peakList,hi[i])
#         } else {
#           peakList<-c(peakList,peakList[waveCount])
#           
#         }
#       
#         waveCount <- waveCount + 1
#                         length
#         isReverse <- T
#         dire[i] = -1
#         point[i] = i
#         
#         start <- point[i-1] 
#         end<- i 
#         clo <- coredata(cl[end])
#         if(start<end){
#           startCl <-coredata(cl[start])
#           nVal<- seq(from = startCl, to = clo, length.out  = (end - start)+1)
#                    
#           nlist<-c((start):(start+length(nVal) -1))
#         
#          value= replace(x = value, list = nlist, values =  nVal )
#         }else
#         {          
#           value[end] = coredata(cl[end])
#         }
#         bottom<-coredata(lo[i])
#         
#         if(!isDraw)
#         {
#           isDraw <- T
#           start <- trendStartIndex    
#           end<- i 
#           step <- calStep(df, 1)
#           trendLine <- drawTrend (startPoint =  df[start],trendLine =  trendLine,start =  start,end =  end,dire =  1, step =  step) 
#           
#         }
#       }else
#       {      
#         dire[i] = 1
#         point[i] = point[i-1]
#         mainDire[i] = mainDire[i-1] 
#         if(mainDire[i] == -1 )
#         {
#           if(!is.na(trendLine[i]) & trendLine[i] < coredata(cl[i]))
#           {
#             #trend line break no longer valid
#             trendLine[i:length(trendLine)]<-NA              
#           }
#         }else if(mainDire[i] == 1 )
#         {
#           
#           if(is.na(trendLine[i]))
#           {
#           if(coredata(cl[i])>peakList[waveCount-1])
#           {
#             
#               # new High redraw trend line
#               start <-  trendStartIndex
#               end <- i 
#               subTrend <- df[start:end,]
#               step<-calStep(subTrend,1)
#               trendLine <- drawTrend(startPoint =  df[start],trendLine =  trendLine,start = start,end =  end,dire = 1, step =  step,lineStart =i )   
#             }
#           }
#         }
#       }     
#       
#     }
#     
#     if(mainDire[i-1] == -1 & waveCount>1)
#     {
#       lastPeak <- peakList[waveCount-1]
#       if(coredata(cl[i]) > coredata(lastPeak))
#       {
#         # new Up Trend
#         trendStartIndex <- point[i]
#         
#         isDraw <- F
#         
#         mainDire[i] <- 1
#       } else {
#         mainDire[i] <- -1
#       }      
#       
#     }
#     else  if(waveCount>1){
#       #mainDire == 1
#   
#       lastBottom <- bottomList[waveCount-1]
#       if(coredata(cl[i]) < coredata(lastBottom))
#       {
#         # new down Trend
#         trendStartIndex <- point[i]
#         
#         isDraw <- F
#         
#         mainDire[i] <- -1
#       } else {
#         mainDire[i] <- 1
#       }      
#     }
#   }
#   # Tail
#   if(point[length]<=length){
#   start <- point[length]
#   end<- length
# 
#   if(start<end){
#     
#     start <- point[i-1] 
#     end<- length
#     clo <- coredata(cl[end])
#     startCl <-coredata(cl[start])
#    
#     nVal<- seq(from = startCl, to = clo, length.out  = (end - start)+1)
#     nlist<-c((start):(start+length(nVal) -1))
#     value= replace(x = value, list = nlist, values =  nVal )
#   }else
#   {    
#     value[length] =  coredata(cl[length])
#   }}
#   print(length(trendLine))
#   
#   trend<-data.frame(dire,point,value,trendLine)
#   
#   return (trend)
#   
# }



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