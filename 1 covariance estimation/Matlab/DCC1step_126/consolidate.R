library(data.table)

source('../../../functions.R')

consolidated.path <- './consolidated'
out.path <- './out'
returns.path <- '../../../0 data/CRSP/sp500_stock_ex_returns.csv'


path.out.make <- function(date, file) {
  paste0(out.path, '/', date, '/', file)
}

path.consolidated.make <- function(file) {
  paste0(consolidated.path, '/', file)
}


# load excess returns data
returns <- fread(returns.path, header=T, sep=';')
head(returns[, 1:20], n=3)


# extract all computed dates
dates.all <- list.dirs(path=out.path, full.names=F, recursive=F)
dates <- Filter(function(x) nchar(x) == 10, dates.all) # ignore pending entries
dates <- sort(dates)


# read files of interest
stocks.cols <- colnames(returns)[-c(1:2)]

covs.estimated <- matrix(NA, nrow=length(dates), ncol=ncol(returns) - 2, dimnames=list(c(), stocks.cols))
covs.1stepahead <- matrix(NA, nrow=length(dates), ncol=ncol(returns) - 2, dimnames=list(c(), stocks.cols))

vars.estimated.market <- matrix(NA, nrow=length(dates), ncol=1, dimnames=list(c(), c('median')))
vars.1stepahead.market <- matrix(NA, nrow=length(dates), ncol=1, dimnames=list(c(), c('median')))

for (i in 1:length(dates)) {
  date <- dates[i]
  
  
  # covariances
  tmp.covs.stocks <- first.col.2.rownames(fread(path.out.make(date, 'covs.stocks.csv'), header=T, sep=';', data.table=F))
  tmp.covs.cols <- gsub('x', '', colnames(tmp.covs.stocks)) 
  
  # covs.estimated
  covs.estimated[i, tmp.covs.cols] <- unlist(tmp.covs.stocks['estimated', ])
  
  # covs.1stepahead
  covs.1stepahead[i, tmp.covs.cols] <- unlist(tmp.covs.stocks['1stepahead', ])
  
  
  # variances
  tmp.vars.market <- first.col.2.rownames(fread(path.out.make(date, 'vars.market.csv'), header=T, sep=';', data.table=F))
  
  # vars.estimated
  vars.estimated.market[i, 'median'] <- median(unlist(tmp.vars.market['estimated', ]), na.rm=T)
  
  # vars.1stepahead
  vars.1stepahead.market[i, 'median'] <- median(unlist(tmp.vars.market['1stepahead', ]), na.rm=T)
  
  
  # log progress
  if (i %% 50 == 0)
    cat(date, format(round(100.0 * i / length(dates), 1), nsmall=1), '% \n')
}


# add date as first column
covs.estimated <- cbind(data.table(date=dates), covs.estimated)
covs.1stepahead <- cbind(data.table(date=dates), covs.1stepahead)
vars.estimated.market <- cbind(data.table(date=dates), vars.estimated.market)
vars.1stepahead.market <- cbind(data.table(date=dates), vars.1stepahead.market)



# write consolidated data to disk
if (!dir.exists(consolidated.path))
  dir.create(consolidated.path)

# covs.estimated.csv
fwrite(covs.estimated, path.consolidated.make('covs.estimated.csv'), sep=';')

# covs.1stepahead.csv
fwrite(covs.1stepahead, path.consolidated.make('covs.1stepahead.csv'), sep=';')

# vars.estimated.market.csv
fwrite(vars.estimated.market, path.consolidated.make('vars.estimated.market.csv'), sep=';')

# vars.1stepahead.market.csv
fwrite(vars.1stepahead.market, path.consolidated.make('vars.1stepahead.market.csv'), sep=';')















