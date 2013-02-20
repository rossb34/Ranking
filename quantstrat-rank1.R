# quantstrat-rank.R

rm(list=ls())

library(quantstrat)
library(PerformanceAnalytics)

# Rank.R contains functions for different ranking algorithms
source("/Users/rossbennett/Documents/R Projects/Ranking/r-functions/Rank.R")

# monthly-fun.R contains functions for prepping monthly data
source("/Users/rossbennett/Documents/R Projects/Ranking/r-functions/monthly-fun.R")

# qstratRank.R contains the function to run the Rank backtest using the
# quantstrat framework
source("/Users/rossbennett/Documents/R Projects/Ranking/r-functions/qstratRank.R")

currency("USD")
symbols <- c("XLY", "XLP", "XLE", "AGG", "IVV")
stock(symbols, currency="USD")

# get data for the symbols
getSymbols(symbols, from="2012-01-01", to="2012-12-31")

# create an xts object of monthly adjusted close prices
symbols.close <- monthlyPrices(symbols)

# create an xts object of the symbol ranks
sym.rank <- applyRank(x=symbols.close, rankFun=ave3ROC, n=c(2, 4, 6))

# this is an important step in naming the columns, e.g. XLY.Rank
# the "Rank" column is used as the trade signal (similar to an indicator)
# in the qstratRank function
colnames(sym.rank) <- gsub(".Adjusted", ".Rank", colnames(sym.rank))

# ensure the order of order symbols is equal to the order of columns 
# in symbols.close
stopifnot(all.equal(gsub(".Adjusted", "", colnames(symbols.close)), symbols))

# bind the rank column to the appropriate symbol market data
# loop through symbols, convert the data to monthly and cbind the data
# to the rank
for(i in 1:length(symbols)) {
  x <- get(symbols[i])
  x <- to.monthly(x,indexAt='lastof',drop.time=TRUE)
  indexFormat(x) <- '%Y-%m-%d'
  colnames(x) <- gsub("x",symbols[i],colnames(x))
  x <- cbind(x, sym.rank[,i])
  assign(symbols[i],x)
}

bt <- qstratRank(symbols=symbols, init.equity=100000, top.N=2,
                  max.size=1000, max.levels=1)
bt.stats <- <-bt$stats
# chart of equity
charts.PerformanceSummary(bt1$returns[,"total"], geometric=FALSE, 
                          wealth.index=TRUE, main="Total Performance")


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
