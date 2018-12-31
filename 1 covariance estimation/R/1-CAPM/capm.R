library(data.table)
library(rmgarch)
library(doParallel)

# load excess returns data
fitData <- fread('../../../0 data/CRSP/sp500_stock_ex_returns.csv', header=T, sep=';')
head(fitData[, 1:10], n=3)

dates <- as.vector(fitData$date)
market <- as.matrix(fitData[, 'market']) * 100 # percentage returns to avoid numerical issues
stocks <- as.matrix(fitData[, !c('date', 'market')]) * 100 # percentage returns to avoid numerical issues
stocks.cols <- colnames(stocks)

head(dates, n=3)
head(market, n=3)
head(stocks[, 1:10], n=3)


# rolling window configuration
windowSize <- 252
from <- which(dates %in% '1996-01-02')
to <- which(dates %in% '2013-12-31')
steps <- to - from + 1


# function to compute estimated and forecasted covariances
computeStep <- function(i) {
  # latest date for window
  date <- dates[i]
  
  # output directories
  outDir1 <- paste0('out/', date, '_pending')
  outDir2 <- paste0('out/', date)
  
  # delete _pending folder if it exists already
  if (dir.exists(outDir1))
    unlink(outDir1, recursive=T, force=T)
  
  # skip if final output directory already exists
  if (dir.exists(outDir2)) {
    # already computed - ignore this step
    cat('ignoring ', date, '- already computed \n')
    
    if (dir.exists(outDir1))
      unlink(outDir1, recursive=T, force=T)
    
    return(T)
  }
  
  dir.create(outDir1, recursive=T, showWarnings=F)
  
  # start timer
  cat(paste('step', i - from + 1, 'of', steps, '(', date, ')'))
  
  # extract window data
  fit.market <- market[(i - windowSize + 1):i, ]
  fit.stocks <- stocks[(i - windowSize + 1):i, ]
  
  # omit columns in window with NA values
  fit.stocks <- fit.stocks[, colSums(is.na(fit.stocks)) == 0]
  
  # Fama-French market excess returns variances
  vars.market <- matrix(NA, nrow=2, ncol=1, dimnames=list(c('estimated', '1stepahead'), c('median')))
  
  # market to stocks covariances
  covs <- matrix(NA, nrow=2, ncol=ncol(fit.stocks), dimnames=list(c('estimated', '1stepahead'), colnames(fit.stocks)))
  
  # stock variances
  vars.stocks <- matrix(NA, nrow=2, ncol=ncol(fit.stocks), dimnames=list(c('estimated', '1stepahead'), colnames(fit.stocks)))
  
  for (step in 1:ncol(fit.stocks)) {
	  # calculate covariance matrix
    estimated.covs <- cov(cbind(fit.market, fit.stocks[, step]))
    
    # save results
    vars.market['estimated', 'median'] <- estimated.covs[1, 1]
    vars.market['1stepahead', 'median'] <- estimated.covs[1, 1]
    
    vars.stocks['estimated', step] <- estimated.covs[2, 2]
    vars.stocks['1stepahead', step] <- estimated.covs[2, 2]
    
    covs['estimated', step] <- estimated.covs[1, 2]
    covs['1stepahead', step] <- estimated.covs[1, 2]
  }
  
  fwrite(as.data.frame(vars.market), paste0(outDir1, '/vars.market.csv'), sep=';', row.names=T)
  fwrite(as.data.frame(vars.stocks), paste0(outDir1, '/vars.stocks.csv'), sep=';', row.names=T)
  fwrite(as.data.frame(covs), paste0(outDir1, '/covs.stocks.csv'), sep=';', row.names=T)
  
  # rename directory (remove `_pending`)
  if (dir.exists(outDir2))
    unlink(outDir2, recursive=T, force=T)
  
  file.rename(outDir1, outDir2)
  
  return(T)
}


# create parallel cluster
cl <- makeCluster(detectCores(), outfile='parallel-log.txt')
registerDoParallel(cl, cores=detectCores())

predictedCovs <- foreach(i=from:to, .packages=c('rmgarch', 'data.table'), .verbose=F) %dopar% {
  computeStep(i)
}

stopCluster(cl)


# for (i in from:to) {
#   computeStep(i)
# }














