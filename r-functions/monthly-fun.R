##### monthlyAd function #####
monthlyAd <- function(x){
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

##### monthlyReturns function #####
monthlyReturns <- function(symbols) {
  # The function takes a character vector of symbols loaded into
  # the environment and returns an xts object of simple returns
  # Currently this is only for prepping monthly data
  
  # symbols : character vector of symbols
  
  ROC(x = monthlyPrices(symbols), n = 1, type = "discrete", na.pad = TRUE)
}

##### monthlyPrices function #####
monthlyPrices <- function(symbols) {
  # The function takes a character vector of symbols loaded into
  # the environment and returns an xts object of Adjusted close prices
  # Currently this is only for prepping monthly data
  
  # symbols         : character vector of symbols
  # list.sym        : list of symbols with market data
  
  list.sym <- list()
  for(i in 1:length(symbols)) {
    list.sym[[symbols[i]]] <- get(symbols[i])
  }
  
  do.call(merge, lapply(list.sym, monthlyAd))
}
