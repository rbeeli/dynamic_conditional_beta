library(data.table)

source('../../../functions.R')

consolidated.path <- './consolidated'
out.path <- './out'
returns.path <- '../../../0 data/CRSP/ex_returns.csv'


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
covs.cols <- colnames(returns)[-c(1:2)]

covs.estimated <- matrix(NA, nrow=length(dates), ncol=ncol(returns) - 2, dimnames=list(c(), covs.cols))
covs.1stepahead <- matrix(NA, nrow=length(dates), ncol=ncol(returns) - 2, dimnames=list(c(), covs.cols))

vars.estimated.market <- matrix(NA, nrow=length(dates), ncol=1, dimnames=list(c(), c('market')))
vars.1stepahead.market <- matrix(NA, nrow=length(dates), ncol=1, dimnames=list(c(), c('market')))

infos <- matrix(NA, nrow=length(dates), ncol=2, dimnames=list(c(), c('errors', 'runtime')))
infocriterias.akaike <- matrix(NA, nrow=length(dates), ncol=500, dimnames=list(c(), 1:500))
infocriterias.bayes <- matrix(NA, nrow=length(dates), ncol=500, dimnames=list(c(), 1:500))

coefs.dcc.market.cols <- c(
  'dcca1', 'dccb1',
  '[market].mu', '[market].omega', '[market].alpha1', '[market].beta1')
coefs.dcc.market <- matrix(NA, nrow=length(dates), ncol=2 + 1 * 4, dimnames=list(c(), coefs.dcc.market.cols))

# [PERMNO].mu, [PERMNO].omega, [PERMNO].alpha1, [PERMNO].beta1
coefs.stocks.cols <- c(apply(as.matrix(covs.cols), 1, function(x) c(paste0('[', x, '].mu'), paste0('[', x, '].omega'), paste0('[', x, '].alpha1'), paste0('[', x, '].beta1'))))
coefs.stocks <- matrix(NA, nrow=length(dates), ncol=length(coefs.stocks.cols), dimnames=list(c(), coefs.stocks.cols))

for (i in 1:length(dates)) {
  date <- dates[i]
  
  
  # covariances
  tmp.covs.stocks <- first.col.2.rownames(fread(path.out.make(date, 'covs.stocks.csv'), header=T, sep=';', data.table=F))
  tmp.covs.cols <- colnames(tmp.covs.stocks)
  
  # covs.estimated
  covs.estimated[i, tmp.covs.cols] <- unlist(tmp.covs.stocks['estimated', ])
  
  # covs.1stepahead
  covs.1stepahead[i, tmp.covs.cols] <- unlist(tmp.covs.stocks['1stepahead', ])
  
  
  # variances
  tmp.vars.market <- first.col.2.rownames(fread(path.out.make(date, 'vars.market.csv'), header=T, sep=';', data.table=F))
  
  # vars.estimated
  vars.estimated.market[i, 'market'] <- median(unlist(tmp.vars.market['estimated', ]), na.rm=T)
  
  # vars.1stepahead
  vars.1stepahead.market[i, 'market'] <- median(unlist(tmp.vars.market['1stepahead', ]), na.rm=T)
  
  
  # coefs.dcc.market
  tmp.coefs.dcc.market <- fread(path.out.make(date, 'coefs.dcc.market.csv'), header=T, sep=';', data.table=F)
  coefs.dcc.market[i, ] <- colMeans(tmp.coefs.dcc.market)
  
  # coefs.stocks
  tmp.coefs.stocks <- fread(path.out.make(date, 'coefs.stocks.csv'), header=T, sep=';', data.table=F)
  coefs.stocks[i, colnames(tmp.coefs.stocks)] <- unlist(tmp.coefs.stocks[1, ])
  
  
  # info
  tmp.info <- fread(path.out.make(date, 'info.csv'), header=T, sep=';', data.table=F)
  infos[i, ] <- unlist(tmp.info[1, ])
  
  # infocriteris
  tmp.ic <- fread(path.out.make(date, 'infocriterias.csv'), header=T, sep=';', data.table=F)
  infocriterias.akaike[i, 1:nrow(tmp.ic)] <- unlist(tmp.ic[, 'Akaike'])
  infocriterias.bayes[i, 1:nrow(tmp.ic)] <- unlist(tmp.ic[, 'Bayes'])
  
  
  # log progress
  if (i %% 50 == 0)
    cat(date, format(round(100.0 * i / length(dates), 1), nsmall=1), '% \n')
}

# add date as first column
covs.estimated <- cbind(data.table(date=dates), covs.estimated)
covs.1stepahead <- cbind(data.table(date=dates), covs.1stepahead)
vars.estimated.market <- cbind(data.table(date=dates), vars.estimated.market)
vars.1stepahead.market <- cbind(data.table(date=dates), vars.1stepahead.market)
infos <- cbind(data.table(date=dates), infos)
coefs.dcc.market <- cbind(data.table(date=dates), coefs.dcc.market)
coefs.stocks <- cbind(data.table(date=dates), coefs.stocks)
infocriterias.akaike <- cbind(data.table(date=dates), infocriterias.akaike)
infocriterias.bayes <- cbind(data.table(date=dates), infocriterias.bayes)


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

# infos.csv
fwrite(infos, path.consolidated.make('infos.csv'), sep=';')

# coefs.dcc.market.csv
fwrite(coefs.dcc.market, path.consolidated.make('coefs.dcc.market.csv'), sep=';')

# coefs.stocks.csv
fwrite(coefs.stocks, path.consolidated.make('coefs.stocks.csv'), sep=';')

# infocriterias.akaike.csv
fwrite(infocriterias.akaike, path.consolidated.make('infocriterias.akaike.csv'), sep=';')

# infocriterias.bayes.csv
fwrite(infocriterias.bayes, path.consolidated.make('infocriterias.bayes.csv'), sep=';')















