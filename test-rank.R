# test-rank.R

# test the ranking functions

rm(list=ls())

library(FinancialInstrument)

setwd("/Users/rossbennett/Documents/R Projects/Ranking")

source("/Users/rossbennett/Documents/R Projects/Ranking/r-functions/Rank.R")
source("/Users/rossbennett/Documents/R Projects/Ranking/r-functions/monthly-fun.R")

##### load data to use for testing #####
currency("USD")
symbols <- c("XLY", "XLP", "XLE", "AGG", "IVV")
stock(symbols, currency="USD")

# get data for the symbols
getSymbols(symbols, from="2012-01-01", to="2012-12-31")

##### monthlyPrices test #####

list.sym <- list()
for(i in 1:length(symbols)) {
  list.sym[[symbols[i]]] <- get(symbols[i])
}

# create an xts object of monthly adjusted close prices
symbols.close1 <- do.call(merge, lapply(list.sym, monthlyAd))
symbols.close2 <- monthlyPrices(symbols)
stopifnot(all.equal(symbols.close1, symbols.close2))

#####

##### monthlyReturns test #####
m.ret1 <- ROC(x = symbols.close1, n = 1, type = "discrete", na.pad = TRUE)
m.ret2 <- monthlyReturns(symbols)
stopifnot(all.equal(m.ret1, m.ret2))

#####

##### applyRank test #####
# ROC
r1a <- ROC(symbols.close1, n=3, type="discrete", na.pad=TRUE)
r1b <- applyRank(symbols.close1, rankFun=ROC, n=3, type="discrete", na.pad=TRUE)
stopifnot(all.equal(r1a, r1b))

# aveROC
stopifnot(all.equal(ave3ROC(symbols.close1, n=c(1, 3, 6)),
                    applyRank(symbols.close1, ave3ROC, n=c(1, 3, 6))))

stopifnot(all.equal(ave3ROC(symbols.close1, n=c(4, 6, 8)),
                    applyRank(symbols.close1, ave3ROC, n=c(4, 6, 8))))

# weightAve3ROC
stopifnot(all.equal(weightAve3ROC(symbols.close1),
                    applyRank(symbols.close1, weightAve3ROC)))

stopifnot(all.equal(weightAve3ROC(symbols.close1, n=c(2, 4, 6), weights=c(1/3, 1/3, 1/3)),
                    applyRank(symbols.close1, weightAve3ROC, n=c(2, 4, 6), weights=c(1/3, 1/3, 1/3))))

stopifnot(all.equal(weightAve3ROC(symbols.close1, n=c(2, 3, 4), weights=c(1/4, 1/4, 1/2)),
                    applyRank(symbols.close1, weightAve3ROC, n=c(2, 3, 4), weights=c(1/4, 1/4, 1/2))))

# strengthROC
stopifnot(all.equal(strengthROC(x=symbols.close1), 
                    applyRank(x=symbols.close1, rankFun=strengthROC)))

stopifnot(all.equal(strengthROC(x=symbols.close1, roc_n=2, sd_n=4), 
                    applyRank(x=symbols.close1, rankFun=strengthROC, roc_n=2, sd_n=4)))

stopifnot(all.equal(strengthROC(x=symbols.close1, roc_n=3, sd_n=2), 
                    applyRank(x=symbols.close1, rankFun=strengthROC, roc_n=3, sd_n=2)))

# strengthAve3ROC
# stopifnot(all.equal(strengthAve3ROC(x=symbols.close1),
#                     applyRank(x=symbols.close1, rankFun=strengthAve3ROC)))
# 
# stopifnot(all.equal(strengthAve3ROC(x=symbols.close1, n=c(2,4,6), weights=c(.3, .4, .3), sd_n=2),
#                     applyRank(x=symbols.close1, rankFun=strengthAve3ROC, n=c(2,4,6), weights=c(.3, .4, .3), sd_n=2)))

# etfReplayRank
stopifnot(all.equal(etfReplayRank(x=symbols.close1),
                    applyRank(x=symbols.close1, rankFun=etfReplayRank)))

stopifnot(all.equal(etfReplayRank(x=symbols.close1, n=c(2, 4, 6), w=c(0.2, 0.4, 0.4)),
                    applyRank(x=symbols.close1, rankFun=etfReplayRank, n=c(2, 4, 6), w=c(0.2, 0.4, 0.4))))

# strengthSMA
stopifnot(all.equal(strengthSMA(x=symbols.close1),
                    applyRank(x=symbols.close1, rankFun=strengthSMA)))

##### End ##### 

