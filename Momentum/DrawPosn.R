

myTheme<-NULL
myTheme <- chart_theme()
myTheme$col$dn.col<-'bisque4'
myTheme$col$dn.border <- 'bisque4'
myTheme$col$up.col <- 'coral3'
myTheme$col$up.border <- 'coral3'
chart.Posn(myPort,Symbol,theme=myTheme,TA = '') 

add_TA(x = mktdata$up.TrendPoint.ind,on =1, col='forestgreen', lwd=2)
add_TA(x = mktdata$down.TrendPoint.ind,on =1, col='firebrick1', lwd=2)
add_TA(x = mktdata$dash.TrendPoint.ind,on =1, col='darkslateblue', lwd=2,lty='dashed')

# 
# add_TA(x = mktdata$upR.TrendPoint.ind,on =1, col='cyan', lwd=2)
# add_TA(x = mktdata$downR.TrendPoint.ind,on =1, col='coral', lwd=2)

# 
# waves<-generateWaves(s, r=r)
# trends <- generateTrends(s,waves = waves, r= r)
# trendLine <- getTrendLine(trends,s,range = range) 
# 
# 
# trends$startDate <- index(s)[trends$start]
# trends$breakDate <- index(s)[trends$breakPoint]
# trends$endDate <- index(s)[trends$end]