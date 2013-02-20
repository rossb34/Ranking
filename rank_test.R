# functions for ranking
rm(list = ls())

#source(file = "/Users/rossbennett/Documents/R Projects/Ranking/Rank.R")
library(FinancialInstrument)
library(TTR)
library(PerformanceAnalytics)

RankRB <- function(x){
  # Computes the rank of an xts object of ranking factors
  # ranking factors are the factors that are ranked (i.e. asset returns)
  #
  # args:
  #   x = xts object of ranking factors
  #
  # Returns:
  #   Returns an xts object with ranks
  #   (e.g. for ranking asset returns, the asset with the greatest return
  #    receives a  rank of 1)
  
  r <- as.xts(t(apply(-x, 1, rank, na.last = "keep")))
  return(r)
}

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

SimpleMomentumTest <- function(xts.ret, xts.rank, n = 1, ret.fill.na = 3){
  # returns a list containing a matrix of individual asset returns
  # and the comnbined returns
  # args:
  #  xts.ret = xts of one period returns
  #  xts.rank = xts of ranks
  #  n = number of top ranked assets to trade
  #  ret.fill.na = number of return periods to fill with NA
  #
  # Returns:
  #  returns an xts object of simple returns
  
  # trade the top n asset(s)
  # if the rank of last period is less than or equal to n,
  # then I would experience the return for this month.
  
  # lag the rank object by one period to avoid look ahead bias
  lag.rank <- lag(xts.rank, k = 1, na.pad = TRUE)
  n2 <- nrow(lag.rank[is.na(lag.rank[,1]) == TRUE])
  z <- max(n2, ret.fill.na)
  
  # for trading the top ranked asset, replace all ranks above n
  # with NA to set up for element wise multiplication to get
  # the realized returns
  lag.rank <- as.matrix(lag.rank)
  lag.rank[lag.rank > n] <- NA
  # set the element to 1 for assets ranked <= to rank
  lag.rank[lag.rank <= n] <- 1
  
  # element wise multiplication of the
  # 1 period return matrix and lagged rank matrix
  mat.ret <- as.matrix(xts.ret) * lag.rank
  
  # average the rows of the mat.ret to get the
  # return for that period
  vec.ret <- rowMeans(mat.ret, na.rm = TRUE)
  vec.ret[1:z] <- NA
  
  # convert to an xts object
  vec.ret <- xts(x = vec.ret, order.by = index(xts.ret))
  f <- list(mat = mat.ret, ret = vec.ret, rank = lag.rank)
  return(f)
}

currency("USD")
symbols <- c("XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLK", "XLB", "XLU", "EFA")#, "TLT", "IEF", "SHY")
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
# rate of change and rank based on a single period for 3, 6, 9, and 12 months
#############################################################################

roc.three <- ROC(x = symbols.close , n = 3, type = "discrete")
rank.three <- RankRB(roc.three)

roc.six <- ROC(x = symbols.close , n = 6, type = "discrete")
rank.six <- RankRB(roc.six)

roc.nine <- ROC(x = symbols.close , n = 9, type = "discrete")
rank.nine <- RankRB(roc.nine)

roc.twelve <- ROC(x = symbols.close , n = 12, type = "discrete")
rank.twelve <- RankRB(roc.twelve)

num.assets <- 4

# simple momentum test based on 3 month ROC to rank
case1 <- SimpleMomentumTest(xts.ret = monthly.returns, xts.rank = rank.three,
                            n = num.assets, ret.fill.na = 15)

# simple momentum test based on 6 month ROC to rank
case2 <- SimpleMomentumTest(xts.ret = monthly.returns, xts.rank = rank.six,
                            n = num.assets, ret.fill.na = 15)

# simple momentum test based on 9 month ROC to rank
case3 <- SimpleMomentumTest(xts.ret = monthly.returns, xts.rank = rank.nine,
                            n = num.assets, ret.fill.na = 15)

# simple momentum test based on 12 month ROC to rank
case4 <- SimpleMomentumTest(xts.ret = monthly.returns, xts.rank = rank.twelve,
                            n = num.assets, ret.fill.na = 15)

returns <- cbind(case1$ret, case2$ret, case3$ret, case4$ret)
colnames(returns) <- c("3-Month", "6-Month", "9-Month", "12-Month")

charts.PerformanceSummary(R = returns, Rf = 0, geometric = TRUE, 
                          main = "Momentum Cumulative Return: Top 4 Assets")

table.Stats(returns)

CAGR(returns, m = 12)

print("End")