# qstratRank.R
qstratRank <- function(symbols, init.equity=100000, top.N=1, 
                       max.size=1000, max.levels=1) {
  # The qstratRank function uses the quantstrat framework to backtest a
  # ranking or relative strength strategy
  #
  # args
  # symbols     : character vector of symbols
  # init.equity : initial equity
  # top.N       : trade the top N ranked assets
  # max.size    : maximum position size
  # max.levels  : maximum levels to scale in a trade
  # max.size and max.levels are passed to addPosLimit
  #
  # return value
  # returns a list: end.eq, returns, book, stats
  
  # remove variables
  suppressWarnings(rm("order_book.Rank", pos=.strategy))
  suppressWarnings(rm("account.Rank", "portfolio.Rank", pos=.blotter))
  suppressWarnings(rm("account.st", "port.st", "stock.str", "stratRank",
                      "initDate", "initEq", 'start_t', 'end_t'))
  
  
  # set initial variables
  initDate <- "1900-01-01"
  initEq <- init.equity
  port.st <- "Rank"
  account.st <- "Rank"
  
  # trade the top "N" ranked symbols
  N <- top.N
  
  # initialize quantstrat objects
  initPortf(port.st, symbols=symbols, initDate=initDate)
  initAcct(account.st, portfolios=port.st, initDate=initDate,initEq=initEq)
  initOrders(portfolio=port.st, initDate=initDate)
  
  # initialize a strategy object
  stratRank <- strategy("Rank")
  
  # there are two signals
  # the first signal is when Rank is less than or equal to N
  # (i.e. trades the #1 ranked symbol if N=1)
  stratRank <- add.signal(strategy=stratRank, name="sigThreshold", 
                          arguments=list(threshold=N, column="Rank", 
                                         relationship="lte", cross=FALSE), 
                          label="Rank.lte.N")
  
  # the second signal is when Rank is greter than or equal to N
  # (i.e. trades the #1 ranked symbol if N=1)
  stratRank <- add.signal(strategy=stratRank, name="sigThreshold", 
                          arguments=list(threshold=N, column="Rank", 
                                         relationship="gt", cross=FALSE), 
                          label="Rank.gt.N")
  
  # add buy rule
  stratRank <- add.rule(strategy=stratRank, name='ruleSignal', 
                        arguments = list(sigcol="Rank.lte.N", sigval=TRUE, 
                                         orderqty=max.size, ordertype='market', 
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
  
  #set max position size and levels
  for(symbol in symbols){ addPosLimit(port.st, symbol, initDate, max.size, max.levels) }
  
  print("setup completed")
  
  # apply the strategy to the portfolio
  start_t <- Sys.time()
  out <- try(applyStrategy(strategy=stratRank, portfolios=port.st))
  end_t <- Sys.time()
  print(end_t-start_t)
  
  # update Portfolio
  start_t <- Sys.time()
  updatePortf(Portfolio=port.st, Dates=paste('::', as.Date(Sys.time()), sep=''))
  end_t <- Sys.time()
  print("trade blotter portfolio update:")
  print(end_t - start_t)
  
  # update account
  updateAcct(account.st)
  
  # update ending equity
  updateEndEq(account.st)
  
  # get ending equity
  eq <- getEndEq(account.st, Sys.Date()) + initEq
  
  # view order book to confirm trades
  order.book <- getOrderBook(port.st)
  
  # get trade statistics
  stats <- tradeStats(port.st)
  
  # portfolio returns
  ret1 <- PortfReturns(port.st)
  ret1$total <- rowSums(ret1, na.rm=TRUE)
  
  return(list(end.eq=eq, returns=ret1, book=order.book, stats=stats))
}