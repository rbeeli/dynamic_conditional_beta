library(data.table)
library(plyr)
library(tictoc)
library(RColorBrewer)
library(zoo)

# break on warnings
options(warn=2)

name <- 'sp500'



# load all S&P500 stock returns
sp500.stocks <- fread(paste0('CRSP/', name, '_stocks.csv'), header=T)
setindex(sp500.stocks, permno)
setindex(sp500.stocks, date)
head(sp500.stocks)





# load Fama-French factors
ff <- fread('Kenneth French/F-F_Research_Data_5_Factors_2x3_daily.csv', header=T)
setindex(ff, date)

ff <- ff[which(ff$date %in% sp500.stocks$date), ]




# load S&P500 index returns
index <- fread('CRSP/sp500_index.csv', header=T)
index <- data.table(date=index$date, ret=index$vwretd)
index.ex <- data.table(date=index$date, ret=index$ret)
index.ex$ret <- index.ex$ret - ff$rf

fwrite(index, 'CRSP/sp500_index_vw_returns.csv', sep=';')
fwrite(index.ex, 'CRSP/sp500_index_ex_vw_returns.csv', sep=';')

head(index)
head(index.ex)


# sanity checks
stopifnot(all.equal(unique(sp500.stocks$date), ff$date))
stopifnot(all.equal(index.ex$date, ff$date))




####################################################
# correct CRSP delisting bias
####################################################

#  When a stock is delisted, we use the delisting return from CRSP, if available.
#  Otherwise, we assume the delisting return is -100%, unless the reason for delisting is
#  coded as 500 (reason unavailable), 520 (went to over-the-counter), 551-573, 580 (various reasons),
#  574 (bankruptcy), or 584 (does not meet exchange financial guidelines).
#  For these observations, we assume that the delisting return is -30%.

m30.codes <- c(500, 520, 551:573, 574, 580, 584)
m30.counter <- 0
m100.counter <- 0

# correct delisting bias in CRSP data because of missing delisting return
for (i in which(!is.na(sp500.stocks$dlstcd) & is.na(sp500.stocks$dlret))) {
  if (sp500.stocks[i]$dlstcd %in% m30.codes) {
    # use -30% delisting return
    sp500.stocks[i]$dlret <- -0.3
    m30.counter <- m30.counter + 1
  }
  else {
    # use -100% delisting return
    sp500.stocks[i]$dlret <- -1.0
    m100.counter <- m100.counter + 1
  }
}

paste('-30% delisting corrections: ', m30.counter)
paste('-100% delisting corrections: ', m100.counter)

# apply delisting return to last trading day return
delisted <- which(!is.na(sp500.stocks$dlret))
sp500.stocks[delisted]$ret <- (1 + sp500.stocks[delisted]$ret) * (1 + sp500.stocks[delisted]$dlret) - 1




####################################################
# returns
####################################################

# reshape data to wide format
stocks.ret <- dcast(sp500.stocks, date ~ permno, value.var=c('ret'))

# add Fama-french market returns
stocks.ret$market <- ff$mktrf + ff$rf
cols <- colnames(stocks.ret)
setcolorder(stocks.ret, c('date', 'market', cols[!cols %in% c('date', 'market')]))

head(stocks.ret[, 1:20])
tail(stocks.ret[, 1:20])

# sanity checks
stopifnot(all.equal(stocks.ret$date, ff$date))

# write to CSV file
fwrite(stocks.ret, paste0('CRSP/', name, '_stock_returns.csv'), sep=';')




####################################################
# excess returns
####################################################

# reshape data to wide format
stocks.exret <- dcast(sp500.stocks, date ~ permno, value.var=c('ret'))

# add Fama-french market excess returns
stocks.exret$market <- ff$mktrf
cols <- colnames(stocks.exret)
setcolorder(stocks.exret, c('date', 'market', cols[!cols %in% c('date', 'market')]))

head(stocks.exret[, 1:20])
tail(stocks.exret[, 1:20])

# sanity checks
stopifnot(all.equal(stocks.exret$market, ff$mktrf))

# calculate excess returns (matrix substraction for efficiency)
stocks.exret[, 3:ncol(stocks.exret)] <- stocks.exret[, 3:ncol(stocks.exret)] - matrix(rep(ff$rf, ncol(stocks.exret)-2), ncol=ncol(stocks.exret)-2)

head(stocks.exret[, 1:20])
tail(stocks.exret[, 1:20])

# write to CSV file
fwrite(stocks.exret, paste0('CRSP/', name, '_stock_ex_returns.csv'), sep=';')




####################################################
# demeaned and scaled excess returns
####################################################
stocks.exret.scaled <- stocks.exret[, -c(1)]
stocks.exret.scaled <- sweep(stocks.exret.scaled, 2, apply(stocks.exret.scaled, 2, function(x) mean(x, na.rm=T)))

# convert to percentage returns to circumvent convergence issues
stocks.exret.scaled <- stocks.exret.scaled * 100

# add date column again
stocks.exret.scaled <- cbind(stocks.exret$date, stocks.exret.scaled)
colnames(stocks.exret.scaled) <- c('date', colnames(stocks.exret.scaled)[-1])

# write to CSV and binary file
fwrite(stocks.exret.scaled, paste0('CRSP/', name,'_stock_ex_returns_demeaned_scaled.csv'), sep=';')




####################################################
# dataset of index weights for each stock
####################################################

# reshape data to wide format
sp500.weights <- dcast(sp500.stocks, date ~ permno, value.var=c('cap'))

totalMarketCap <- rowSums(sp500.weights[, -1], na.rm=T)

# calculate relative market cap --> results in index weights
sp500.weights[, 2:ncol(sp500.weights)] <- sp500.weights[, -1] / totalMarketCap

# sanity checks
stopifnot(all.equal(sp500.weights$date, stocks.exret$date))
stopifnot(all.equal(rowSums(sp500.weights[, -1], na.rm=T), rep(1, nrow(sp500.weights))))

# make sure there always is a weight value
test.weights.dt <- data.table(which(is.na(sp500.weights[, -c(1)]), arr.ind=T))
setkey(test.weights.dt, row, col)

test.returns.dt <- data.table(which(is.na(stocks.exret[, -c(1,2)]), arr.ind=T))
setkey(test.returns.dt, row, col)

stopifnot(nrow(test.weights.dt[!test.returns.dt]) == 0) # returns without an index weight

# write to CSV file
fwrite(sp500.weights, paste0('CRSP/', name, '_weights.csv'), sep=';')




####################################################
# dataset of market cap. for each stock
####################################################

# reshape data to wide format
sp500.cap <- dcast(sp500.stocks, date ~ permno, value.var=c('cap'))

# sanity checks
stopifnot(all.equal(sp500.cap$date, stocks.exret$date))

# make sure there always is a market cap value
test.caps.dt <- data.table(which(is.na(sp500.cap[, -c(1)]), arr.ind=T))
setkey(test.caps.dt, row, col)

test.returns.dt <- data.table(which(is.na(stocks.exret[, -c(1,2)]), arr.ind=T))
setkey(test.returns.dt, row, col)

stopifnot(nrow(test.caps.dt[!test.returns.dt]) == 0) # returns without an index weight

# write to CSV file
fwrite(sp500.cap, paste0('CRSP/', name, '_cap.csv'), sep=';')




####################################################
# dataset of volumes for each stock
####################################################

# reshape data to wide format
sp500.vols <- dcast(sp500.stocks, date ~ permno, value.var=c('vol'))

# sanity checks
stopifnot(all.equal(sp500.vols$date, stocks.exret$date))

# interpolate missing volume values up to 4 days
sp500.vols[, 2:ncol(sp500.vols)] <- as.data.table(na.approx(sp500.vols[, 2:ncol(sp500.vols)], maxgap=4))

# make sure there always is a volume value
test.vols.dt <- data.table(which(is.na(sp500.vols[, -c(1)]), arr.ind=T))
setkey(test.vols.dt, row, col)

test.returns.dt <- data.table(which(is.na(stocks.exret[, -c(1,2)]), arr.ind=T))
setkey(test.returns.dt, row, col)

stopifnot(nrow(test.vols.dt[!test.returns.dt]) == 0) # returns without an index weight

# write to CSV file
fwrite(sp500.vols, paste0('CRSP/', name, '_vol.csv'), sep=';')




####################################################
# dataset of prices for each stock
####################################################

# reshape data to wide format
sp500.prcs <- dcast(sp500.stocks, date ~ permno, value.var=c('prc'))

# sanity checks
stopifnot(all.equal(sp500.prcs$date, stocks.exret$date))

# make sure there always is a volume value
test.prcs.dt <- data.table(which(is.na(sp500.prcs[, -c(1)]), arr.ind=T))
setkey(test.prcs.dt, row, col)

test.returns.dt <- data.table(which(is.na(stocks.exret[, -c(1,2)]), arr.ind=T))
setkey(test.returns.dt, row, col)

stopifnot(nrow(test.prcs.dt[!test.returns.dt]) == 0) # returns without an index weight

# write to CSV file
fwrite(sp500.prcs, paste0('CRSP/', name, '_prc.csv'), sep=';')





####################################################
# dataset of shares outstanding for each stock
####################################################

# reshape data to wide format
sp500.shrsout <- dcast(sp500.stocks, date ~ permno, value.var=c('shrout'))

# sanity checks
stopifnot(all.equal(sp500.shrsout$date, stocks.exret$date))

# make sure there always is a volume value
test.shrsout.dt <- data.table(which(is.na(sp500.shrsout[, -c(1)]), arr.ind=T))
setkey(test.shrsout.dt, row, col)

test.returns.dt <- data.table(which(is.na(stocks.exret[, -c(1,2)]), arr.ind=T))
setkey(test.returns.dt, row, col)

stopifnot(nrow(test.shrsout.dt[!test.returns.dt]) == 0) # returns without an index weight

# write to CSV file
fwrite(sp500.shrsout, paste0('CRSP/', name, '_shrout.csv'), sep=';')





####################################################
# plots
####################################################

# plot market cap over time
plot(totalMarketCap, type='l', main="Index market capitalization", ylab='market capitalization')

# plot returns of S&P 500 over time
series <- cbind(1 + cumsum(ff$mktrf + ff$rf), 1 + cumsum(ff$rf))
cols <- brewer.pal(3, 'Set1')
matplot(series, type='l', main='Market index returns', ylab='return', col=cols)
legend('topleft', legend=c('Market index cum. returns', 'Fama-French risk-free returns'),col=cols, fill=cols)

# plot number of index constituents over time
sp500.constituents <- rep(ncol(stocks.exret) - 2, nrow(stocks.exret)) - rowSums(is.na(stocks.exret))
plot(sp500.constituents, type='l', xaxt='n', ylim=c(95, 105), main='Number of index constituents over time', xlab="date", ylab="number of listed stocks")

# plot average daily return over time (equal weighted)
avg.daily.ret.eq <- rowSums(stocks.exret[, -c(1,2)], na.rm=T) / sp500.constituents
avg.daily.ret.eq.mean <- mean(avg.daily.ret.eq)
plot(avg.daily.ret.eq, type='l', main='average daily return (equal weighted)', col='black')
abline(h=avg.daily.ret.eq.mean, col='red')
legend('topright', legend=c('daily average', sprintf('overall average %.4f %%', avg.daily.ret.eq.mean * 100)), col=c('black', 'red'), fill=c('black', 'red'))

# plot average daily return over time (value weighted)
avg.daily.ret.vw <- rowSums(stocks.exret[, -c(1,2)] * sp500.weights[, -c(1)], na.rm=T)
avg.daily.ret.vw.mean <- mean(avg.daily.ret.vw)
plot(avg.daily.ret.vw, type='l', main='average daily return (value weighted)', col='black')
abline(h=avg.daily.ret.vw.mean, col='red')
legend('topright', legend=c('daily average', sprintf('overall average %.4f %%', avg.daily.ret.vw.mean * 100)), col=c('black', 'red'), fill=c('black', 'red'))














