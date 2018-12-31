library(data.table)
library(matrixStats)

source('../../../functions.R')

consolidated.path <- './consolidated'
out.path <- './out'


path.out.make <- function(file) {
  paste0(out.path, '/', file)
}

path.consolidated.make <- function(file) {
  paste0(consolidated.path, '/', file)
}


# available dates
returns <- fread('../../../0 data/CRSP/sp500_stock_ex_returns_demeaned_scaled.csv', header=T, sep=';')

# read files of interest
files <- list.files(path.out.make(''), pattern='*.csv')
stocks.cols <- paste0('x', colnames(returns[, !c('date', 'market')]))

covs.1stepahead <- matrix(NA, nrow=nrow(returns), ncol=length(stocks.cols), dimnames=list(c(), stocks.cols))
vars.1stepahead.market <- matrix(NA, nrow=nrow(returns), ncol=length(stocks.cols), dimnames=list(c(), stocks.cols))
vars.1stepahead.stocks <- matrix(NA, nrow=nrow(returns), ncol=length(stocks.cols), dimnames=list(c(), stocks.cols))

for (file in files) {
  file.path <- path.out.make(file)
  
  name <- regmatches(file, regexpr('x[0-9]{5}', file))
  
  if (file.info(file.path)$size == 0) {
    cat('File ', file, ' empty. Skipping. \n')
    next
  }
  
  cat('Processing file ', name, '... \n')
  
  vars.covs.stocks.market <- fread(path.out.make(file), header=T, sep=';')
  
  vars.market <- vars.covs.stocks.market$var_market
  vars.stock <- vars.covs.stocks.market$var_stock
  covs.stocks <- vars.covs.stocks.market$cov
  
  idxs.date <- which(returns$date %in% vars.covs.stocks.market$date)
  
  
  # covariances stocks
  covs.1stepahead[idxs.date, name] <- covs.stocks
  
  # variances market
  vars.1stepahead.market[idxs.date, name] <- vars.market
  
  # variances stocks
  vars.1stepahead.stocks[idxs.date, name] <- vars.stock
}

# add date as first column
covs.1stepahead <- cbind(data.table(date=returns$date), covs.1stepahead)
vars.1stepahead.market <- cbind(data.table(date=returns$date), vars.1stepahead.market)
vars.1stepahead.stocks <- cbind(data.table(date=returns$date), vars.1stepahead.stocks)

# market variance average and median
vars.1stepahead.market$average <- rowMeans(vars.1stepahead.market[, ..stocks.cols], na.rm=T)
vars.1stepahead.market$median <- rowMedians(as.matrix(vars.1stepahead.market[, ..stocks.cols]), na.rm=T)



# remove rows with NAs
na.rows <- which(rowSums(is.na(covs.1stepahead[, ..stocks.cols])) == length(stocks.cols))

covs.1stepahead <- covs.1stepahead[-na.rows, ]
vars.1stepahead.market <- vars.1stepahead.market[-na.rows, ]
vars.1stepahead.stocks <- vars.1stepahead.stocks[-na.rows, ]



# write consolidated data to disk
if (!dir.exists(consolidated.path))
  dir.create(consolidated.path)

# covs.1stepahead.csv
fwrite(covs.1stepahead, path.consolidated.make('covs.1stepahead.csv'), sep=';')

# vars.1stepahead.market.csv
fwrite(vars.1stepahead.market, path.consolidated.make('vars.1stepahead.market.csv'), sep=';')

# vars.1stepahead.stocks.csv
fwrite(vars.1stepahead.stocks, path.consolidated.make('vars.1stepahead.stocks.csv'), sep=';')




# plot some data
matplot(vars.1stepahead.market[, sample(colnames(vars.1stepahead.market), 20), with=F], type='l', main='Market variances')

matplot(vars.1stepahead.market[, c('average', 'median')], type='l', col=c('red', 'black'), main='Average and median market variance')
legend('topleft', legend=c('average', 'median'), fill=c('red', 'black'), border='white')

matplot(covs.1stepahead[, sample(colnames(covs.1stepahead), 20), with=F], type='l', main='Stock/market covariances')







