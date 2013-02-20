# Backtest of ETF Replay Ranking Algorithm

# clear the workspace
rm(list=ls())

# step-by-step example of ETF Replay Ranking Algorithm

##### etfReplayRank Function #####
# we can use the same step-by-step approach outlined above to write a function
# to rank instruments based on ETF Replay

etfReplayRank <- function(x, periods=c(6, 3, 3), w=c(0.4, 0.3, 0.3)) {
  # x       : xts object of close prices
  # periods : vector of periods used for ret1, ret2, vol
  # w       : vector of weights
  
  # xts objects of returns and volatility
  # for this example, volatility is the standard deviation of 1-month returns
  ret1 <- ROC(x, n=periods[1], type="discrete")
  ret2 <- ROC(x, n=periods[2], type="discrete")
  tmp.ret <- ROC(x, n=1, type="discrete")
  vol <- as.xts(apply(tmp.ret, 2, runSD, n=periods[3]), order.by=index(x))
  
  # apply the rank function row-wise
  ret1.rank <- as.xts(t(apply(-ret1, 1, rank, na.last="keep")))
  ret2.rank <- as.xts(t(apply(-ret2, 1, rank, na.last="keep")))
  vol.rank <- as.xts(t(apply(vol, 1, rank, na.last="keep")))
  
  # multiply the factor weights into the rank objects
  tmp1 <- ret1.rank * w[1]
  tmp2 <- ret2.rank * w[2]
  tmp3 <- vol.rank * w[3]
  
  # add the tmp objects to get the weighted factor rank
  wf.rank <- tmp1 + tmp2 + tmp3
  
  # overall rank
  out.rank <- as.xts(t(apply(wf.rank, 1, rank, na.last="keep")))
  out.rank
}

##### MonthlyAd function #####
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

##### SimpleMomentumTest function #####
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

# test out the etfReplayRank function on real assets

library(FinancialInstrument)

currency("USD")
symbols <- c("XLY", "XLP", "XLE", "AGG", "IVV")
stock(symbols, currency="USD")

# new environment for symbols
symEnv <- new.env()

getSymbols(symbols, from="2012-01-01", to="2012-12-31", env=symEnv)

# create an xts object of monthly adjusted close prices
symbols.close <- do.call(merge, eapply(symEnv, MonthlyAd))

# monthly returns
monthly.returns <- ROC(x = symbols.close, n = 1, type = "discrete", 
                       na.pad = TRUE)

# call the ranking function
etf.rank <- etfReplayRank(x=symbols.close, periods=c(6, 3, 3), 
                          w=c(0.4, 0.3, 0.3))

# simple momentum test based on etfReplayRank
case1 <- SimpleMomentumTest(xts.ret = monthly.returns, xts.rank = etf.rank,
                            n = 1, ret.fill.na = 7)

# the SimpleMomentumTest returns a list of 3 objects
# you can explore them to understand the function
str(case1)
# matrix of returns
case1$mat
# matrix of rank
case1$rank
# xts vector of returns
case1$ret

