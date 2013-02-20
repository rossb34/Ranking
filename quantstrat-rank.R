# quantstrat-rank.R

#rm(list=ls())

suppressWarnings(rm("order_book.Rank",pos=.strategy))
suppressWarnings(rm("account.Rank","portfolio.Rank",pos=.blotter))
suppressWarnings(rm("account.st","port.st","stock.str","stratRank",
                    "initDate","initEq",'start_t','end_t'))

library(quantstrat)
library(PerformanceAnalytics)

setwd("/Users/rossbennett/Documents/R Projects/Ranking")

source("/Users/rossbennett/Documents/R Projects/Ranking/r-functions/Rank.R")
source("/Users/rossbennett/Documents/R Projects/Ranking/r-functions/monthly-fun.R")

currency("USD")
symbols <- c("XLY", "XLP", "XLE", "AGG", "IVV")
stock(symbols, currency="USD")

# get data for the symbols
getSymbols(symbols, from="2012-01-01", to="2012-12-31")

# temporarily store the symbols in a list
# this is used to merge the symbol data into a single xts object of 
# adjusted close prices
list.sym <- list()
for(i in 1:length(symbols)) {
   list.sym[[symbols[i]]] <- get(symbols[i])
}

# create an xts object of monthly adjusted close prices
symbols.close <- do.call(merge, lapply(list.sym, monthlyAd))

rm(list.sym)

# monthly returns
monthly.returns <- ROC(x = symbols.close, n = 1, type = "discrete",
                       na.pad = TRUE)

# rank is an xts object of the ranks
ret.rank <- rowRank(monthly.returns)
colnames(ret.rank) <- gsub(".Adjusted", ".Rank", colnames(ret.rank))

# ensure the order of symbols is equal to the columns in symbols.close
stopifnot(all.equal(gsub(".Adjusted", "", colnames(symbols.close)), symbols))

# loop through symbols
# convert the data to monthly and cbind the data to the rank
for(i in 1:length(symbols)) {
  x <- get(symbols[i])
  x <- to.monthly(x,indexAt='lastof',drop.time=TRUE)
  indexFormat(x) <- '%Y-%m-%d'
  colnames(x) <- gsub("x",symbols[i],colnames(x))
  x <- cbind(x, ret.rank[,i])
  assign(symbols[i],x)
}

#

# use method from rsi.R and low-volatility post to use this in quantstrat

# quantstrat
initDate <- "1900-01-01"
initEq <- 100000
port.st <- "Rank"
account.st <- "Rank"

initPortf(port.st, symbols=symbols, initDate=initDate)
initAcct(account.st, portfolios=port.st, initDate=initDate,initEq=initEq)
initOrders(portfolio=port.st, initDate=initDate)

# initialize a strategy object
stratRank <- strategy("Rank")

# There are two signals
# The first is when Rank is less than or equal to N
# (i.e. trades the #1 ranked symbol if N=1)
stratRank <- add.signal(strategy=stratRank, name="sigThreshold", 
                        arguments=list(threshold=3, column="Rank", 
                                       relationship="lte", cross=FALSE), 
                        label="Rank.lte.N")

# The second is when Rank is greter than or equal to N
# (i.e. trades the #1 ranked symbol if N=1)
stratRank <- add.signal(strategy=stratRank, name="sigThreshold", 
                        arguments=list(threshold=3, column="Rank", 
                                       relationship="gt", cross=FALSE), 
                        label="Rank.gt.N")

# add buy rule
stratRank <- add.rule(strategy=stratRank, name='ruleSignal', 
                      arguments = list(sigcol="Rank.lte.N", sigval=TRUE, 
                                       orderqty=1000, ordertype='market', 
                                       orderside='long', pricemethod='market', 
                                       replace=FALSE, osFUN=osMaxPos), 
                      type='enter', path.dep=TRUE)

# add exit rule
stratRank <- add.rule(strategy = stratRank, name='ruleSignal', 
                      arguments = list(sigcol="Rank.gt.N", sigval=TRUE, 
                                       orderqty='all', ordertype='market', 
                                       orderside='long', pricemethod='market', 
                                       replace=FALSE), 
                      type='exit', path.dep=TRUE)

for(symbol in symbols){ addPosLimit(port.st, symbol, initDate, 1000, 5) } #set max pos 


print("setup completed")

# Apply the strategy to the portfolio
start_t <- Sys.time()
out <- try(applyStrategy(strategy=stratRank, portfolios=port.st))
end_t <- Sys.time()
print(end_t-start_t)

# Update Portfolio
start_t <- Sys.time()
updatePortf(Portfolio=port.st, Dates=paste('::', as.Date(Sys.time()), sep=''))
end_t <- Sys.time()
print("trade blotter portfolio update:")
print(end_t - start_t)

# Update Account
updateAcct(account.st)

# Update Ending Equity
updateEndEq(account.st)

# get ending equity
getEndEq(account.st, Sys.Date()) + initEq

# View order book to confirm trades
# print(getOrderBook(port.st))

ret1 <- PortfReturns(port.st)
ret1$total <- rowSums(ret1, na.rm=TRUE)
ret1

# chart of equity
charts.PerformanceSummary(ret1$total,geometric=FALSE,wealth.index=TRUE)


###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2012
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
###############################################################################
