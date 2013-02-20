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