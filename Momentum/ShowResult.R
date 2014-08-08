name <- Symbol
root <- 'Result'

Txns <- 'Txns.csv'

Summary <-  'Summary.csv'

OB <- 'OrderBook.csv'

Txnspath <- paste(root,name,Txns,sep = '//')
Summarypath <- paste(root,name,Summary,sep = '//')
ObPath <- paste(root,name,OB,sep = '//')
ts<-getTxns(Portfolio=myPort, Symbol=Symbol)
out<-cbind(as.character(index(ts)),as.vector( ts$Txn.Price),as.vector(ts$Txn.Qty),as.vector(ts$Txn.Fees),as.vector(ts$Net.Txn.Realized.PL))
colnames(out)<-c('Time','EntryPoint','Size','Fees','Net PL')
write.table(out,file=Txnspath,quote=F,sep=',',row.names=F) 

tstats <- tradeStats(Portfolio=myPort, Symbol=Symbol)
summary <- c('Name', name)
summary <- rbind(summary,c('Trades', tstats$Num.Trades))
summary <- rbind(summary,c('Net.Trading.PL', tstats$Net.Trading.PL))
summary <- rbind(summary,c('Largest.Winner', tstats$Largest.Winner))
summary <- rbind(summary,c('Largest.Loser', tstats$Largest.Loser))
summary <- rbind(summary,c('Avg.Trade.PL', tstats$Avg.Trade.PL))
summary <- rbind(summary,c('Win.Rate', tstats$Percent.Positive))
summary <- rbind(summary,c('Loss.Rate', tstats$Percent.Negative))
summary <- rbind(summary,c('Profit.Factor', tstats$Profit.Factor))
summary <- rbind(summary,c('Avg.Win.Trade', tstats$Avg.Win.Trade))
summary <- rbind(summary,c('Avg.Losing.Trade', tstats$Avg.Losing.Trade))
summary <- rbind(summary,c('Avg.WinLoss.Ratio', tstats$Avg.WinLoss.Ratio))
summary <- rbind(summary,c('Max.Profit.Equity', tstats$Max.Equity))
summary <- rbind(summary,c('Min.Losing.Equity', tstats$Min.Equity))

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


# suppressWarnings((trade.stats.tab <- data.frame(tab.trades,tab.profit)))
write.table(summary,file=Summarypath,quote=F,sep=',',row.names=F) 
#  suppressWarnings(View(trade.stats.tab))




ob <- getOrderBook(portfolio = myPort)
write.table(ob$myPortfolio,file=ObPath,quote=F,sep=',',row.names=F) 

# View(ob$myPortfolio)