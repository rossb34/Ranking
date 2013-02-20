# Functions for ways to rank assets based on rate of change
# TODO - add functions to rank based on other factors

# The functions defined below depend on functions in the xts and TTR packages
# library(TTR)

##### applyRank #####
applyRank <- function(x, rankFun, ...) {
  # symbols : character vector of symbols
  # rankFun : function that returns the rank
  # rankFun should be ave3ROC, weightAve3ROC, strengthROC, strengthAve3ROC,
  # etfReplayRank, or strengthSMA.
  # x       : xts object of prices
  # ...     : arguments to rankFun
  
  FUN <- match.fun(rankFun)
  FUN(x, ...)
}

##### symbolRank #####
symbolRank <- function(symbols, rank.obj) {
  # loop through symbols
  # convert the market data to monthly periodicity 
  # cbind the appropriate column from rank.obj to the market data
  # makes the assumption that the order symbols and rank.obj are equal
  
  # symbols  : character vector of symbols
  # rank.obj : xts object of ranks of each symbol
  
  for(i in 1:length(symbols)) {
    x <- get(symbols[i])
    x <- to.monthly(x,indexAt='lastof',drop.time=TRUE)
    indexFormat(x) <- '%Y-%m-%d'
    colnames(x) <- gsub("x", symbols[i], colnames(x))
    x <- cbind(x, rank.obj[,i])
    assign(symbols[i],x)
  }
}

##### rowRank #####
rowRank <- function(x){
	# Computes the rank of an xts object of ranking factors
	# ranking factors are the factors that are ranked (i.e. asset returns)
  #
	#  x : xts object of ranking factors
	#
	#   Returns an xts object with ranks
	#   For ranking asset returns, the asset with the greatest return
	#   receives a  rank of 1
	
	as.xts(t(apply(-x, 1, rank, na.last = "keep")))
}

#Use the supplied TTR::ROC function for a straight ROC computation

##### ave3ROC #####
ave3ROC <- function(x, n=c(1, 3, 6)){
	# Computes the average rate of change based on averaging 3 periods
  #
	#  x   : xts object of prices
	#  n   : vector of periods to use n = (period1, period2, period3)
	#  ave : xts object of asset rate of change by averaging 3 periods
	
  roc1 <- ROC(x, n = n[1], type = "discrete")
  roc2 <- ROC(x, n = n[2], type = "discrete")
  roc3 <- ROC(x, n = n[3], type = "discrete")
  ave <- (roc1 + roc2 + roc3)/3
  rowRank(ave)
}

##### weightAve3ROC #####
weightAve3ROC <- function(x, n = c(1, 3, 6), weights = c(1/3, 1/3, 1/3)){
	# Computes the weighted average rate of change based on a vector of periods
	# and a vector of weights
	#
	#  x       : xts object of prices
	#  n       : vector of periods to use n = (period1, period2, period3)
	#  weights : a vector of weights for computing the weighted average
	#
	# Returns:
	#   xts object of weighted average asset rate of change
	
  if((sum(weights) != 1) || (length(n) != 3) || (length(weights) != 3)){
    stop("The sum of the weights must equal 1 and the length of n and weights must be 3")
  } else {
    roc1 <- ROC(x, n = n[1], type = "discrete")
    roc2 <- ROC(x, n = n[2], type = "discrete")
    roc3 <- ROC(x, n = n[3], type = "discrete")
    wave <- (roc1 * weights[1] + roc2 * weights[2] + roc3 * weights[3]) / sum(weights)
    rowRank(wave)
  }
}

##### strengthROC #####
strengthROC <- function(x, roc_n = 3, sd_n = 3){
	# Computes the strength of asset returns
	# strength is defined as ROC / SD
	#
	#   x     : xts object prices
	#   roc_n : number of periods to use for ROC
	#   sd_n  : number of periods to use for runSD
	#   out     : xts object of the strength of asset rate of change
	
  roc <- ROC(x, n = roc_n, type = "discrete")
  sd <- apply(x, 2, runSD, n = sd_n)
  strength <- roc/sd
  rowRank(strength)
}

 ##### strengthAve3ROC #####
# strengthAve3ROC <- function(x, n = c(1, 3, 6), weights = c(1/3, 1/3, 1/3), sd_n = 3){
# 	# Computes the strength of asset returns based on weighted average ROC
# 	# strength is defined as ROC / SD
# 	#
# 	#   x       : xts object of prices
# 	#   n       : vector of periods to use n = (period1, period2, period3)
# 	#   weights : a vector of weights for computing the weighted average
# 	#   sd_n    : number of periods to use for runSD
# 	#   out     : xts object of asset strength based on weighted average
#   #             rate of change
# 	
#   if((sum(weights) != 1) || (length(n) != 3) || (length(weights) != 3)){
#     stop("The sum of the weights must equal 1 and the length of n and weights must be 3")
#   } else{
#     wave <- weightAve3ROC(x, n, weights)
#     sd <- apply(x, 2, runSD, n = sd_n)
#     reclass(sd, x)
#     out <- wave$x / sd
#     rank.obj <- rowRank(out)
#     return(list(x=out, rank=rank.obj))
#   }
# }

##### etfReplayRank #####
etfReplayRank <- function(x, n=c(1, 3, 6), w=c(0.4, 0.3, 0.3)) {
  # function to rank assets based on the ETF Replay ranking algorithm
  #
  # x : xts object of close prices
  # n : vector of n used for ret1, ret2, vol
  # w : vector of weights
  
  # xts objects of returns and volatility
  # for this example, volatility is the standard deviation of 1-month returns
  ret1 <- ROC(x, n=n[1], type="discrete")
  ret2 <- ROC(x, n=n[2], type="discrete")
  tmp.ret <- ROC(x, n=1, type="discrete")
  vol <- as.xts(apply(tmp.ret, 2, runSD, n=n[3]), order.by=index(x))
  
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

##### strengthSMA #####
strengthSMA <- function(x) {
  # function to rank assets based on Price, SMA, and sd of returns
  # (Price - SMA) / sigma
  # x : xts object of prices
  
  ret <- ROC(x, n=1, type="discrete")
  sigma <- apply(ret, 2, runSD, n=5)
  sma <- apply(x, 2, SMA, n=10)
  out <- (x - sma) / sigma
  rowRank(out)
}


