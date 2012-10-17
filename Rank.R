# Functions for ways to rank assets based on rate of change
# TODO - add functions to rank based on other factors

# The functions defined below depend on functions in the xts and TTR packages
library(TTR)

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
	
	r <- as.xts(t(apply(-x, 1, rank)))
	return(r)
}

#Use the supplied TTR::ROC function for a straight ROC computation

#average rate of change across multiple periods
Ave3ROC <- function(x, n1, n2, n3){
	# Computes the average rate of change based on averaging 3 periods; n1, n2, and n3.
	#
	# args:
	#   x = xts object of simple returns
	#   n1 = number of periods to use for ROC
	#   n2 = number of periods to use for ROC
	#   n3 = number of periods to use for ROC
	#
	# Returns:
	#   xts object of asset rate of change by averaging 3 periods
	
  roc1 <- ROC(x, n = n1, type = "discrete")
  roc2 <- ROC(x, n = n2, type = "discrete")
  roc3 <- ROC(x, n = n3, type = "discrete")
  ave <- (roc1 + roc2 + roc3)/3
  return(ave)
}

#weighted average rate of change across multiple periods
WeightAve3ROC <- function(x, n = c(1,3,6), weights = c(1/3, 1/3, 1/3)){
	# Computes the weighted average rate of change based on a vector of periods
	# and a vector of weights
	#
	# args:
	#   x = xts object of simple returns
	#   n = vector of periods to use n = (period1, period2, period3)
	#   weights = a vector of weights for computing the weighted average
	#
	# Returns:
	#   xts object of weighted average asset rate of change
	
  if((sum(weights) != 1) || (length(n) != 3) || (length(weights) != 3)){
    stop("The sum of the weights must equal 1 and the length of n and weights must be 3")
  } else{
    roc1 <- ROC(x, n = n[1], type = "discrete")
    roc2 <- ROC(x, n = n[2], type = "discrete")
    roc3 <- ROC(x, n = n[3], type = "discrete")
    wave <- (roc1*weights[1] + roc2*weights[2] + roc3*weights[3])/sum(weights)
    return(wave)
  }
}

StrengthROC <- function(x, roc_n = 3, sd_n = 3){
	# Computes the strength of asset returns
	# strength is defined as ROC / SD
	#
	# args:
	#   x = xts object of simple returns
	#   roc_n = number of periods to use for ROC
	#   sd_n = number of periods to use for runSD
	#
	# Returns:
	#   xts object of the strength of asset rate of change
	
  roc <- ROC(x, n = roc_n, type = "discrete")
  sd <- apply(x, 2, runSD, n = sd_n)#, sample = TRUE, cumulative = FALSE)
  reclass(sd, x)
  s <- roc/sd
  return(s)
}

StrengthAve3ROC <- function(x, n = c(1,3,6), weights = c(1/3, 1/3, 1/3), sd_n = 3){
	# Computes the strength of asset returns based on weighted average ROC
	# strength is defined as ROC / SD
	#
	# args:
	#   x = xts object of simple returns
	#   n = vector of periods to use n = (period1, period2, period3)
	#   weights = a vector of weights for computing the weighted average
	#   sd_n = number of periods to use for runSD
	#
	# Returns:
	#   xts object of asset strength based on weighted average rate of change
	
  if((sum(weights) != 1) || (length(n) != 3) || (length(weights) != 3)){
    stop("The sum of the weights must equal 1 and the length of n and weights must be 3")
  } else{
#    roc1 <- ROC(x, n = n[1], type = "discrete")
#    roc2 <- ROC(x, n = n[2], type = "discrete")
#    roc3 <- ROC(x, n = n[3], type = "discrete")
#    wave <- (roc1*weights[1] + roc2*weights[2] + roc3*weights[3])/sum(weights)
    wave <- WeightAve3ROC(x, n, weights)
    sd <- apply(x, 2, runSD, n = sd_n)
    reclass(sd, x)
    swave <- wave/sd
    return(swave)
  }
}
