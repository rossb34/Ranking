# Ranking Algorithm for ETF Replay

# Rank based on 3 factors
# 1. ReturnA    (ret1)
# 2. ReturnB    (ret2)
# 3. Volatility (vol)

# Returns are ranked from high to low
# Volatility is ranked from low to high

# clear the workspace
rm(list=ls())

# step-by-step example of ETF Replay Ranking Algorithm

# returns and volatility used in sample from ETF replay
ret1 <- c(0.07, 0.24, 0.06)
ret2 <- c(0.17, 0.11, 0.05)
vol <- c(0.16, 0.23, 0.05)

# rank the 3 factors
# note the negative sign when ranking returns. This is so that the highest
# return receives a rank of 1
ret1.rank <- rank(-ret1)
ret2.rank <- rank(-ret2)
vol.rank <- rank(vol)

# vector of factor weights
w <- c(0.4, 0.3, 0.3)

# temporary variables that multiply the ranks by factor weights
tmp1 <- ret1.rank * w[1]
tmp2 <- ret2.rank * w[2]
tmp3 <- vol.rank * w[3]

# weighted factor rank
wf.rank <- tmp1 + tmp2 + tmp3

# final rank based on the weighted factor rank
out.rank <- rank(wf.rank)

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
#####

# test out the etfReplayRank function on real assets

library(FinancialInstrument)

currency("USD")
symbols <- c("XLY", "XLP", "XLE")
stock(symbols, currency="USD")

# new environment for symbols
symEnv <- new.env()

getSymbols(symbols, from="2012-01-01", to="2012-12-31", env=symEnv)

# function to get monthly adjusted closing prices
MonthlyAd <- function(x) {
  sym <- sub("\\..*$", "", names(x)[1])
  Ad(to.monthly(x, indexAt = 'lastof', drop.time = TRUE, name = sym))
}

# create an xts object of monthly adjusted close prices
symbols.close <- do.call(merge, eapply(symEnv, MonthlyAd))

# call the ranking function
etfReplayRank(x=symbols.close, periods=c(6, 3, 3), w=c(0.4, 0.3, 0.3))

