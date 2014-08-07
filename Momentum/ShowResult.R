

ts<-getTxns(Portfolio=myPort, Symbol=Symbol)

ob <- getOrderBook(portfolio = myPort)


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

suppressWarnings((trade.stats.tab <- data.frame(tab.trades,tab.profit,tab.wins)))
suppressWarnings(View(trade.stats.tab))
View(ob$myPortfolio)
View(ts)