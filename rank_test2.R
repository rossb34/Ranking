# rank_test2.R

# script to run a simple backtest comparing different ranking methods
# for a momentum based trading system

# remove objects from workspace
rm(list = ls())

# Rank.R file contains functions that will be used
source(file = "/Users/rossbennett/Documents/R Projects/Ranking/Rank.R")
source(file = "/Users/rossbennett/Documents/R Projects/Ranking/backtest.R")

# load required packages
library(FinancialInstrument)
library(TTR)
library(PerformanceAnalytics)

MonthlyAd <- function(x){
  # Converts daily data to monthly and returns only the monthly close 
  # Note: only used with Yahoo Finance data so far
  # Thanks to Joshua Ulrich for the Monthly Ad function
  # 
  # args:
  #   x = daily price data from Yahoo Finance
  #
  # Returns:
  #   xts object with the monthly adjusted close prices
  
  sym <- sub("\\..*$", "", names(x)[1])
  Ad(to.monthly(x, indexAt = 'lastof', drop.time = TRUE, name = sym))
}

CAGR <- function(x, m){
  # Function to compute the CAGR given simple returns
  #
  # args:
  #  x = xts of simple returns
  #  m = periods per year (i.e. monthly = 12, daily = 252)
  #
  # Returns the Compound Annual Growth Rate
  x <- na.omit(x)
  cagr <- apply(x, 2, function(x, m) prod(1 + x)^(1 / (length(x) / m)) - 1, m = m)
  return(cagr)
}

currency("USD")
symbols <- c("XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLK", "XLB", "XLU", "EFA")
stock(symbols, currency = "USD", multiplier = 1)

# create new environment to store symbols
symEnv <- new.env()

# getSymbols and assign the symbols to the symEnv environment
getSymbols(symbols, from = '2002-09-01', to = '2012-10-20', env = symEnv)

# xts object of the monthly adjusted close prices
symbols.close <- do.call(merge, eapply(symEnv, MonthlyAd))

# monthly returns
monthly.returns <- ROC(x = symbols.close, n = 1, type = "discrete", na.pad = TRUE)

#############################################################################
# rate of change and rank based on a single period for 6, 9, and 12 months
#############################################################################

roc.six <- ROC(x = symbols.close , n = 6, type = "discrete")
rank.six <- RankRB(roc.six)

roc.nine <- ROC(x = symbols.close , n = 9, type = "discrete")
rank.nine <- RankRB(roc.nine)

roc.twelve <- ROC(x = symbols.close , n = 12, type = "discrete")
rank.twelve <- RankRB(roc.twelve)

#############################################################################
# rate of change and rank based on averaging 6, 9, and 12 month returns
#############################################################################
roc.ave <- WeightAve3ROC(x = symbols.close, n = c(6, 9, 12), 
                         weights = c(1/3, 1/3, 1/3))
rank.ave <- RankRB(roc.ave)

roc.weight.ave <- WeightAve3ROC(x = symbols.close, n = c(6, 9, 12), 
                                weights = c(1/6, 2/3, 1/6))
rank.weight.ave <- RankRB(roc.weight.ave)

#############################################################################
# run the backtest
#############################################################################

num.assets <- 4

# simple momentum test based on 6 month ROC to rank
case1 <- SimpleMomentumTest(xts.ret = monthly.returns, xts.rank = rank.six,
                            n = num.assets, ret.fill.na = 15)

# simple momentum test based on 9 month ROC to rank
case2 <- SimpleMomentumTest(xts.ret = monthly.returns, xts.rank = rank.nine,
                            n = num.assets, ret.fill.na = 15)

# simple momentum test based on 12 month ROC to rank
case3 <- SimpleMomentumTest(xts.ret = monthly.returns, xts.rank = rank.twelve,
                            n = num.assets, ret.fill.na = 15)

# simple momentum test based on average of 6, 9, and 12 month ROC to rank
case4 <- SimpleMomentumTest(xts.ret = monthly.returns, xts.rank = rank.ave,
                            n = num.assets, ret.fill.na = 15)

# simple momentum test based on weighted average of 6, 9, and 12 month ROC to rank
case5 <- SimpleMomentumTest(xts.ret = monthly.returns, xts.rank = rank.weight.ave,
                            n = num.assets, ret.fill.na = 15)

returns <- cbind(case1$ret, case2$ret, case3$ret, case4$ret, case5$ret)
colnames(returns) <- c("6-Month", "9-Month", "12-Month", "Ave", "Weighted Ave")

charts.PerformanceSummary(R = returns, Rf = 0, geometric = TRUE, 
                          main = "Momentum Cumulative Return: Top 4 Assets")

table.Stats(returns)

cagr <- CAGR(returns, m = 12)
max.dd <- maxDrawdown(returns)
print(cagr)
print(max.dd)

print("End")