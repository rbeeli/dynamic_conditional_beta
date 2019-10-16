library(data.table)
library(tictoc)
library(RColorBrewer)
library(matrixStats)
library(dplyr)
library(pracma)
library(readr)
library(tikzDevice)
library(lmtest)
library(sandwich)
library(bit64)

source('../functions.R')


# sample period
date.from <- '1996-01-02'
date.to <- '2013-12-31'

# equal- or value-weighted portfolio returns
value.weighted <- T

# how many tiles (deciles = 10)
ntiles <- 10

# EWMA smoothing of covariances
ma <- list(lag=7, type='e')

# output figures as .tex files
tex.output <- T

# output folder for .tex files
tex.dir <- './results/'

# name of stock sample
sample.name <- ifelse(value.weighted, 'vw', 'eq')


ff <- fread('../0 data/Kenneth French/F-F_Research_Data_5_Factors_2x3_daily.csv', header=T, sep=';', integer64='numeric')
returns.stocks <- fread('../0 data/CRSP/sp500_stock_ex_returns.csv', header=T, sep=';', integer64='numeric')
weights <- fread('../0 data/CRSP/sp500_weights.csv', header=T, sep=';', integer64='numeric')
vol <- fread('../0 data/CRSP/sp500_vol.csv', header=T, sep=';', integer64='numeric')
prc <- fread('../0 data/CRSP/sp500_prc.csv', header=T, sep=';', integer64='numeric')
shrout <- fread('../0 data/CRSP/sp500_shrout.csv', header=T, sep=';', integer64='numeric')
cap <- fread('../0 data/CRSP/sp500_cap.csv', header=T, sep=';', integer64='numeric')

# limit time period
dates <- returns.stocks$date[which(returns.stocks$date == date.from):which(returns.stocks$date == date.to)]

# merge all data tables
ff <- merge.by.date(dates, ff)
returns.stocks <- merge.by.date(dates, returns.stocks)
weights <- merge.by.date(dates, weights)
vol <- merge.by.date(dates, vol)
prc <- merge.by.date(dates, prc)
shrout <- merge.by.date(dates, shrout)
cap <- merge.by.date(dates, cap)

stopifnot(nrow(ff) == length(dates))
stopifnot(nrow(ff) == nrow(returns.stocks))
stopifnot(nrow(ff) == nrow(weights))
stopifnot(nrow(ff) == nrow(vol))
stopifnot(nrow(ff) == nrow(prc))
stopifnot(nrow(ff) == nrow(shrout))
stopifnot(nrow(ff) == nrow(cap))


backtest <- function(label, dates, ff, ret.market, ret.stocks, weights, vol, prc, shrout, cap, covs.stocks, var.market, ma) {
  # sanity checks
  stopifnot(all.equal(colnames(ret.stocks), colnames(covs.stocks)))
  stopifnot(all.equal(colnames(ret.stocks), colnames(weights)))
  stopifnot(all.equal(colnames(ret.stocks), colnames(vol)))
  stopifnot(all.equal(colnames(ret.stocks), colnames(prc)))
  stopifnot(all.equal(colnames(ret.stocks), colnames(shrout)))
  stopifnot(all.equal(colnames(ret.stocks), colnames(cap)))
  
  # convert to matrix for efficiency
  var.market <- as.matrix(var.market)
  ret.market <- as.matrix(ret.market)
  returns <- as.matrix(ret.stocks)
  covs.stocks <- as.matrix(covs.stocks)
  weights <- as.matrix(weights)
  vol <- as.matrix(vol)
  prc <- as.matrix(prc)
  shrout <- as.matrix(shrout/1000)*1000
  cap <- as.matrix(cap/1000)*1000
  
  stopifnot(matequal(is.na(weights), is.na(cap)))
  stopifnot(matequal(is.na(weights), is.na(prc)))
  
  # smoothing of covariances
  if (ma$lag > 1) {
    var.market <- movavg(var.market, n=ma$lag, type=ma$type)
    
    for (col in 1:ncol(covs.stocks)) {
      covs.stocks[, col] <- movavg(covs.stocks[, col], n=ma$lag, type=ma$type)
    }
  }
  
  # calculate conditional beta (covariance stock to market portfolio divided by variance of market portfolio)
  betas <- covs.stocks / matrix(rep(var.market, ncol(covs.stocks)), ncol=ncol(covs.stocks))
  
  # calculate deciles of betas and returns
  deciles.rets <- matrix(ncol=ntiles, nrow=length(dates)-1, dimnames=list(c(), apply(as.matrix(1:ntiles), 1, toString)))
  deciles.betas <- matrix(ncol=ntiles, nrow=length(dates)-1, dimnames=list(c(), apply(as.matrix(1:ntiles), 1, toString)))
  deciles.size <- matrix(ncol=ntiles, nrow=length(dates)-1, dimnames=list(c(), apply(as.matrix(1:ntiles), 1, toString)))
  deciles.mktshr <- matrix(ncol=ntiles, nrow=length(dates)-1, dimnames=list(c(), apply(as.matrix(1:ntiles), 1, toString)))
  deciles.turn <- matrix(ncol=ntiles, nrow=length(dates)-1, dimnames=list(c(), apply(as.matrix(1:ntiles), 1, toString)))
  deciles.illiq <- matrix(ncol=ntiles, nrow=length(dates)-1, dimnames=list(c(), apply(as.matrix(1:ntiles), 1, toString)))
  deciles.dates <- dates[-1]
  backtest.ff <- ff[-1, ]
  
  # keep track of stocks in deciles of previous period to calculate turnover
  stocks.prev.deciles <- list()
  for (i in 1:10) stocks.prev.deciles[[i]] <- list()
  
  for (row in 1:(nrow(covs.stocks) - 1)) {
    row.covs <- covs.stocks[row, ]
    row.betas <- betas[row, ]
    row.rets <- returns[row + 1, ]    # using returns of next day, covariance estimated at day of data!
    row.weights <- weights[row + 1, ] # index weight of next day to match returns
    row.caps <- cap[row + 1, ]        # market cap. of next day to match returns
    row.vols <- vol[row + 1, ]        # volume of next day to match returns
    row.prcs <- prc[row + 1, ]        # price of next day to match returns
    
    # ensure all fields have a value (no NAs)
    row.no.na.idxs <- intersect(intersect(which(!is.na(row.covs)), which(!is.na(row.rets))), which(!is.na(row.weights)))
    
    if (length(row.no.na.idxs) < 10) {
      stop(paste('Not enough data for row', row))
    }
    
    row.rets <- row.rets[row.no.na.idxs]
    row.betas <- row.betas[row.no.na.idxs]
    row.covs <- row.covs[row.no.na.idxs]
    row.weights <- row.weights[row.no.na.idxs]
    row.caps <- row.caps[row.no.na.idxs]
    row.vols <- row.vols[row.no.na.idxs]
    row.prcs <- row.prcs[row.no.na.idxs]
    
    stopifnot(length(which(is.na(row.rets))) == 0)
    stopifnot(length(which(is.na(row.betas))) == 0)
    stopifnot(length(which(is.na(row.covs))) == 0)
    stopifnot(length(which(is.na(row.weights))) == 0)
    stopifnot(length(which(is.na(row.caps))) == 0)
    stopifnot(length(which(is.na(row.vols))) == 0)
    stopifnot(length(which(is.na(row.prcs))) == 0)
    
    # create deciles
    row.betas.ranks <- ntile(row.betas, ntiles)
    
    for (i in 1:ntiles) {
      decile.idxs <- which(row.betas.ranks == i)
      
      decile.betas <- row.betas[decile.idxs]
      decile.covs <- row.covs[decile.idxs]
      decile.ret <- row.rets[decile.idxs]
      decile.weights <- row.weights[decile.idxs]
      decile.caps <- row.caps[decile.idxs]
      
      # weighted average returns per decile
      if (value.weighted) {
        deciles.rets[row, i] <- weightedMean(decile.ret, decile.weights)
        deciles.betas[row, i] <- weightedMean(decile.betas, decile.weights)
      } else {
        deciles.rets[row, i] <- mean(decile.ret)
        deciles.betas[row, i] <- mean(decile.betas)
      }
      
      # average market cap
      deciles.size[row, i] <- mean(decile.caps)
      
      # market share of decile stocks
      deciles.mktshr[row, i] <- sum(decile.caps) / sum(row.caps)
        
      # turnover and illiquidity measure
      decile.vols <- row.vols[decile.idxs]
      decile.prc <- row.prcs[decile.idxs]
      
      has.values <- intersect(which(!is.na(decile.vols)), which(decile.vols > 0))
      decile.vols <- decile.vols[has.values]
      decile.prc <- decile.prc[has.values]
      decile.ret <- decile.ret[has.values]

      deciles.turn[row, i] <- length(which(!decile.idxs %in% stocks.prev.deciles[[i]])) / length(decile.idxs)
      deciles.illiq[row, i] <- mean((1e8 * abs(decile.ret)) / (decile.vols * decile.prc))
        
      stocks.prev.deciles[[i]] <- decile.idxs
    }
  }
  
  # long in high beta stocks, short in low beta stocks
  pf.rets <- deciles.rets[, ntiles] - deciles.rets[, 1]
  
  # Fama-French 5 factor regression
  req.ff.lm <- lm(pf.rets ~ mktrf + smb + hml + rmw + cma, data=backtest.ff)
  reg.ff <- summary(req.ff.lm)
  reg.ff.coeftest <- coeftest(req.ff.lm, vcov=NeweyWest(req.ff.lm)) # Newey-West t-tests
  
  
  # CAPM regression
  reg.capm.lm <- lm(pf.rets ~ mktrf, data=backtest.ff)
  reg.capm <- summary(reg.capm.lm)
  reg.capm.coeftest <- coeftest(reg.capm.lm, vcov=NeweyWest(reg.capm.lm)) # Newey-West t-tests
  
  # mean-return regression
  reg.mean.ret.lm <- lm(pf.rets ~ 1)
  reg.mean.ret <- summary(reg.mean.ret.lm)
  reg.mean.ret.coeftest <- coeftest(reg.mean.ret.lm, vcov=NeweyWest(reg.mean.ret.lm)) # Newey-West t-tests
  
  pf.stats <- list(
    rets=pf.rets,
    mean.ret=reg.mean.ret.coeftest['(Intercept)', 'Estimate'],
    mean.ret.ttest=reg.mean.ret.coeftest['(Intercept)', 't value'],
    alpha.ff=reg.ff.coeftest['(Intercept)', 'Estimate'],
    alpha.ff.ttest=reg.ff.coeftest['(Intercept)', 't value'],
    capm.alpha=reg.capm.coeftest['(Intercept)', 'Estimate'],
    capm.alpha.ttest=reg.capm.coeftest['(Intercept)', 't value'],
    capm.beta=reg.capm.coeftest['mktrf', 'Estimate']
  )
  
  return(list(
    dates=deciles.dates,
    ff=backtest.ff,
    market.ret=ret.market[-1],
    deciles.betas=deciles.betas,
    deciles.rets=deciles.rets,
    deciles.size=deciles.size,
    deciles.illiq=deciles.illiq,
    deciles.turn=deciles.turn,
    deciles.mktshr=deciles.mktshr,
    pf.stats=pf.stats,
    ma=ma
  ))
}

backtest.consolidated <- function(label, dirPath, texMarker, ma) {
  cat('> ', label, '\n')
  
  var.market <- remove.colnames.prefixes(fread(paste0(dirPath, 'vars.1stepahead.market.csv'), header=T, sep=';'), 'x')
  covs.stocks <- remove.colnames.prefixes(fread(paste0(dirPath, 'covs.1stepahead.csv'), header=T, sep=';'), 'x')
  
  # sanity checks
  stopifnot(all.equal(returns.stocks$date, weights$date))
  stopifnot(all.equal(covs.stocks$date, var.market$date))
  
  # merge datasets by date
  dates <- intersect(returns.stocks$date, covs.stocks$date)
  
  ff <- merge.by.date(dates, ff)
  ret.market <- merge.by.date(dates, returns.stocks)[, 'market']
  var.market <- merge.by.date(dates, var.market)[, 'median']
  ret.stocks <- merge.by.date(dates, returns.stocks)
  
  stocks <- intersect(colnames(ret.stocks), colnames(covs.stocks))
  stocks <- stocks[which(!stocks %in% c('date', 'market'))]
  
  ret.stocks <- ret.stocks[, ..stocks]
  covs.stocks <- merge.by.date(dates, covs.stocks)[, ..stocks]
  weights <- merge.by.date(dates, weights)[, ..stocks]
  vol <- merge.by.date(dates, vol)[, ..stocks]
  prc <- merge.by.date(dates, prc)[, ..stocks]
  shrout <- merge.by.date(dates, shrout)[, ..stocks]
  cap <- merge.by.date(dates, cap)[, ..stocks]
  
  backtest.res <- backtest(label, dates, ff, ret.market, ret.stocks, weights, vol, prc, shrout, cap, covs.stocks, var.market, ma)
  backtest.res[['label']] <- label
  backtest.res[['texMarker']] <- texMarker
  
  return(backtest.res)
}



# # optimize smoother parameters
# opt.combinations <- expand.grid(c('e'), c(seq(2, 16, by=1)), stringsAsFactors=F)
# for (i in 1:nrow(opt.combinations)) {
#   ma <- list(type=opt.combinations[i, 1], lag=opt.combinations[i, 2])
# 
#   b1 <- backtest.consolidated('DCC', '../1 covariance estimation/Matlab/DCC1step/consolidated/', 'D', ma)
#   b2 <- backtest.consolidated('COMFORT-DCC', '../1 covariance estimation/Matlab/COMFORT-DCC/consolidated/', 'CD', ma)
# 
#   cat(sprintf("Smoothing lag            : %i \n", ma$lag))
#   cat(sprintf("Smoothing type           : %s \n", ma$type))
# 
#   cat(sprintf("Sharpe DCC               : %.4f \n", sharpe.ratio(b1$pf.stats$rets, 0, 252)))
#   cat(sprintf("Monthly DCC              : %.4f%% \n", return.d2m(b1$pf.stats$mean.ret) * 100))
#   cat(sprintf("Sharpe CDCC              : %.4f \n", sharpe.ratio(b2$pf.stats$rets, 0, 252)))
#   cat(sprintf("Monthly CDCC             : %.4f%% \n", return.d2m(b2$pf.stats$mean.ret) * 100))
#   cat('------------------------------------ \n')
# }
# stop()



results <- list(
  backtest.consolidated('CAPM-252', '../1 covariance estimation/R/CAPM/consolidated_252/', 'C', ma),
  backtest.consolidated('DCC-252', '../1 covariance estimation/Matlab/DCC1step_252/consolidated/', 'D', ma),
  backtest.consolidated('DCC-1000', '../1 covariance estimation/Matlab/DCC1step_1000/consolidated/', 'D1k', ma),
  backtest.consolidated('CDCC-1000', '../1 covariance estimation/Matlab/COMFORT-DCC/consolidated/', 'CD', ma)
)





###################################################
# Plots
###################################################

cols <- c('#e93030', '#0073d5', '#379b26', '#ff8f00', '#03b5b5', '#f7c853', '#8e7d00', '#09d293', '#403b6e', '#cf69e1')
lty <- c(1, 2, 3, 4)
leg <- unlist(lapply(results, function(x) x$label))


# Portfolio returns
par(mfrow=c(1,1))

for (i in 1:length(results)) {
  result <- results[[i]]
  dates <- result$dates
  market.ret <- result$market.ret
  pf.stats <- result$pf.stats
  
  cat("-------------------------------------\n")
  cat(paste(result$label, '\n\n'))
  cat(sprintf("Smoothing lag            : %i \n", result$ma$lag))
  cat(sprintf("Smoothing type           : %s \n", result$ma$type))
  cat(sprintf("Sharpe market            : %.3f \n", sharpe.ratio(market.ret, result$ff$rf, 252)))
  cat(sprintf("Annual volatility market : %.3f \n", sd(market.ret) * sqrt(252)))
  cat(sprintf("Monthly return market    : %.2f%% \n\n", return.d2m(mean(market.ret)) * 100))
  
  cat(sprintf("> Portfolio (excess returns) \n"))
  cat(sprintf("Annual volatility PF     : %.3f \n", sd(pf.stats$rets) * sqrt(252)))
  cat(sprintf("Sharpe PF                : %.3f \n", sharpe.ratio(pf.stats$rets, result$ff$rf, 252)))
  cat(sprintf("Monthly                  : %.2f%% \n", return.d2m(pf.stats$mean.ret) * 100))
  cat(sprintf("Monthly (t-test)         : %.2f \n", pf.stats$mean.ret.ttest))
  cat(sprintf("FF alpha                 : %.2f%% \n", return.d2m(pf.stats$alpha.ff) * 100))
  cat(sprintf("FF alpha (t-test)        : %.3f \n", pf.stats$alpha.ff.ttest))
  cat(sprintf("CAPM alpha               : %.2f%% \n", return.d2m(pf.stats$capm.alpha) * 100))
  cat(sprintf("CAPM beta                : %.2f \n", pf.stats$capm.beta))
  cat("-------------------------------------\n")
  
  
  # plot cum. return per decile
  plot.a4 <- colCumsums(result$deciles.rets)
  
  labels.x <- sapply(dates, function(d) format(as.Date(d), '%m.%Y'))
  labels.x.at <- rev(seq(nrow(plot.a4), 1, by=-365))
  
  labels.y <- seq(-20, 20, by=0.5)
  labels.y.at <- seq(-20, 20, by=0.5)
  
  matplot(plot.a4, type='l', xaxt='n', yaxt='n', ylab='', lty=1, lwd=1, main=paste('Decile returns', result$label), col=cols, xaxs='i')
  axis(1, at=labels.x.at, labels=F)
  text(labels.x.at, par("usr")[3] - 0.18, labels=labels.x[labels.x.at], cex=.85, srt=45, adj=1, xpd=T)
  axis(2, at=labels.y.at, lab=paste0(labels.y * 100, '%'), las=TRUE, cex.axis=.85)
  legend('topleft', col=c(cols, 'black'), border='white', lty=1, lwd=2, legend=paste0('Decile ', 1:10), bty='n')
  
  # save tex
  if (tex.output) {
    tikz(file=paste0(tex.dir, sample.name, '_cum_ret_deciles_', result$label, '.tex'), width=6, height=4)
    plot.a4.dev <- matplot(colCumsums(result$deciles.rets), type='l', xaxt='n', yaxt='n', ylab='', lty=1, lwd=1, col=cols, xaxs='i')
    axis(1, at=labels.x.at, labels=F)
    text(labels.x.at, par("usr")[3] - 0.18, labels=labels.x[labels.x.at], cex=.85, srt=45, adj=1, xpd=T)
    axis(2, at=labels.y.at, lab=paste0(labels.y * 100, '\\%'), las=TRUE, cex.axis=.85)
    legend('topleft', col=c(cols, 'black'), border='white', lty=1, lwd=2, legend=paste0('Decile ', 1:10), bty='n', cex=.8)
    print(plot.a4.dev)
    dev.off()
  }
}




# cumulative high-low portfolio returns
plot.dates <- results[[1]]$dates
market.ret <- results[[1]]$market.ret
pf.cum.ex.rets <- matrix(NA, nrow=length(market.ret), ncol=length(results) + 1)
pf.cum.ex.rets[, 1] <- cumsum(market.ret)

for (i in 1:length(results)) {
  pf.cum.ex.rets[, i+1] <- cumsum(results[[i]]$pf.stats$rets)
}

cols.1 <- c('gray', cols)

labels.x <- sapply(plot.dates, function(d) format(as.Date(d), '%m.%Y'))
labels.x.at <- rev(seq(nrow(pf.cum.ex.rets), 1, by=-365))
labels.y <- seq(-20, 20, by=0.25)
labels.y.at <- seq(-20, 20, by=0.25)

ylim <- c(min(pf.cum.ex.rets), max(pf.cum.ex.rets))
ylim.dist <- ylim[2] - ylim[1]

matplot(pf.cum.ex.rets, type='l', xaxt='n', yaxt='n', ylab='', col=cols.1, main='Cumulative returns of High-Low portfolios', lty=1, xaxs='i')
axis(1, at=labels.x.at, labels=F)
text(labels.x.at, par('usr')[3] - 0.05 * ylim.dist, labels=labels.x[labels.x.at], cex=.85, srt=45, adj=1, xpd=T)
axis(2, at=labels.y.at, lab=paste0(labels.y * 100, '%'), las=TRUE, cex.axis=.85)
legend('topleft', legend=c('Market portfolio', leg), col=cols.1, border='white', lty=1, lwd=2, bty='n')

# save tex
if (tex.output) {
  tikz(file=paste0(tex.dir, sample.name, '_cum_ret_high-low.tex'), width=6, height=4)
  plot.a3.dev <- matplot(pf.cum.ex.rets, type='l', xaxt='n', yaxt='n', ylab='', lwd=.4, col=cols.1, lty=1, xaxs='i')
  axis(1, at=labels.x.at, labels=F)
  text(labels.x.at, par('usr')[3] - 0.06 * ylim.dist, labels=labels.x[labels.x.at], cex=.85, srt=45, adj=1, xpd=T)
  axis(2, at=labels.y.at, lab=paste0(labels.y * 100, '\\%'), las=TRUE, cex.axis=.85)
  legend('topleft', legend=c('Market portfolio', leg), col=cols.1, border='white', lty=1, lwd=2, bty='n', cex=.8)
  print(plot.a3.dev)
  dev.off()
}





# Average returns
deciles.avg.rets <- matrix(NA, nrow=ntiles, ncol=length(results))
for (i in 1:length(results)) {
  deciles.avg.rets[, i] <- colMeans(results[[i]]$deciles.rets)
}


par(mfrow=c(1,1))


# Average return per decile [%]
plot.2.data <- return.d2m(deciles.avg.rets) * 100

matplot(plot.2.data, type='l', xaxs='i', main="Average return per decile [%]", xlab='Decile', ylab='', lwd=2, lty=lty, xaxt='n', yaxt='n', col=cols, ylim=c(0, max(plot.2.data)))
legend('topleft', legend=leg, col=cols, border='white', bty='n', cex=.8, lty=lty)
axis(1, at=1:10, labels=1:10, cex.axis=.85)
axis(2, at=seq(0, 4, 0.4), lab=paste0(seq(0, 4, 0.4), '%'), las=TRUE, cex.axis=.85)


# save tex
if (tex.output) {
  plot.c4.data <- return.d2m(deciles.avg.rets) * 100
  
  tikz(file=paste0(tex.dir, sample.name, '_avg_ret_per_decile.tex'), width=6, height=4)
  plot.c4 <- matplot(plot.c4.data, type='l', xaxs='i', xlab='', ylab='', lwd=2, lty=lty, xaxt='n', yaxt='n', col=cols, ylim=c(0, max(plot.c4.data)))
  axis(1, at=1:10, labels=1:10, cex.axis=.85)
  axis(2, at=seq(0, 4, 0.4), lab=paste0(seq(0, 4, 0.4), '\\%'), las=TRUE, cex.axis=.85)
  legend('topleft', legend=leg, col=cols, lty=lty, lwd=2, border='white', bty='n', cex=.8)
  print(plot.c4)
  dev.off()
}



fit.1 <- summary(lm(results[[2]]$pf.stats$rets ~ market.ret))
return.d2m(coef(fit.1)['(Intercept)', 'Estimate'])


# # Average decile betas
# deciles.betas <- matrix(NA, nrow=ntiles, ncol=length(results))
# for (i in 1:length(results)) {
#   deciles.betas[, i] <- colMeans(results[[i]]$deciles.betas)
# }
# colnames(deciles.betas) <- leg
# 
# 
# d.cols <- brewer.pal(3, 'Set1')
# plot <- matplot(deciles.betas, main="Average value-weighted beta per decile", xlab="Decile", lty=1, ylab="Average Beta", type="l", col=d.cols)
# legend('topleft', legend=leg, col=d.cols, border='white', fill=d.cols, bty='n')
# 
# # save to .tex file
# if (tex.output) {
#   par(mar=c(2.5, 2.5, 0.5, 0.5))
# 
#   tikz(file=paste0(tex.dir, sample.name, '_avg_deciles_beta.tex'), width=5, height=4)
#   plot <- matplot(deciles.betas, xlab='', lty=1, ylab='', type='l', lwd=0.4, col=d.cols, xaxt='n')
#   axis(side=1, at=1:nrow(deciles.betas), labels=1:nrow(deciles.betas))
#   legend('topleft', legend=leg, col=d.cols, border='white', fill=d.cols, bty='n', cex=.8)
#   print(plot)
#   dev.off()
# 
#   par(mar=c(5, 4, 4, 2) + 0.1)
# }


# Decile betas 1 and 10
par(mfrow=c(4, 1))

for (i in 1:length(results)) {
  result <- results[[i]]
  
  plot.data <- result$deciles.betas[, c(1,10)]
  ylim <- c(min(plot.data), max(plot.data))
  ylim.dist <- ylim[2] - ylim[1]
  
  matplot(plot.data, type='l', xaxt='n', ylab='', xaxs='i', col=cols, lwd=1, lty=1, main=paste(leg[i], "average beta of deciles 1 and 10"), ylim=ylim)
  legend('topleft', legend=c('Decile 1', 'Decile 10'), col=cols, lty=1, lwd=2, bty='n', border='white')
  labels.x <- sapply(result$dates, function(d) format(as.Date(d), '%m.%Y'))
  labels.x.at <- rev(seq(nrow(plot.data), 1, by=-365))
  axis(1, at=labels.x.at, labels=F)
  text(labels.x.at, par('usr')[3] - 0.1 * ylim.dist, labels=labels.x[labels.x.at], cex=1, srt=45, adj=1, xpd=T)
}

par(mfrow=c(1,1))


# export as .tex
if (tex.output) {
  for (i in 1:length(results)) {
    result <- results[[i]]
    
    plot.data <- result$deciles.betas[, c(1,10)]
    ylim <- c(min(plot.data), max(plot.data))
    ylim.dist <- ylim[2] - ylim[1]
    
    tikz(file=paste0(tex.dir, sample.name, '_avg_beta_deciles_1_10_', result$label, '.tex'), width=5.5, height=2.8)
    d.plot <- matplot(plot.data, type='l', xaxt='n', ylab='', xaxs='i', col=cols, lwd=1, lty=1, ylim=ylim)
    legend('topleft', legend=c('Decile 1', 'Decile 10'), col=cols, lty=1, lwd=2, bty='n', border='white', cex=.8)
    labels.x <- sapply(result$dates, function(d) format(as.Date(d), '%m.%Y'))
    labels.x.at <- rev(seq(nrow(plot.data), 1, by=-365))
    axis(1, at=labels.x.at, labels=F)
    text(labels.x.at, par('usr')[3] - 0.12 * ylim.dist, labels=labels.x[labels.x.at], cex=.85, srt=45, adj=1, xpd=T)
    print(d.plot)
    dev.off()
  }
}



# # Decile betas 1 to 10 - one plot
# par(mfrow=c(1, 1))
# cols <- brewer.pal(ntiles, 'Paired')
# 
# for (i in 1:length(results)) {
#   result <- results[[i]]
#   dates <- result$dates
#   ylim <- c(min(result$deciles.betas), max(result$deciles.betas))
# 
#   for (decile in 1:ntiles) {
#     if (decile == 1)
#       plot(result$deciles.betas[, as.character(decile)], type='l', col=cols[decile], lwd=0.4, ylim=ylim, ylab="beta", main=paste(leg[i], " - decile 1 to 10"))
#     else
#       lines(result$deciles.betas[, as.character(decile)], col=cols[decile], lwd=0.4)
#   }
# }









# .tex summary table
if (tex.output) {
  tex.uni.pf <- read_file('tpls/tpl_univariate_portfolio.tex')
  tex.beta.sd <- read_file('tpls/tpl_beta_std_dev.tex')
  
  for (i in 1:length(results)) {
    result <- results[[i]]
    dates <- result$dates
    pf.stats <- result$pf.stats
    
    format.per <- function(v, digits=2) sprintf(paste0('%.', digits,'f'), v * 100) # percentage
    format.ret <- function(ret) format.per(ret) # percentage
    format.beta <- function(beta) sprintf('%.2f', beta)
    format.t <- function(t) paste0('(', sprintf('%.2f', t), ')')
    
    betas.avg <- colMeans(result$deciles.betas)
    ret.avg <- colMeans(result$deciles.rets)
    size.avg <- colMeans(result$deciles.size)
    illiq.avg <- colMeans(result$deciles.illiq)
    turn.avg <- colMeans(result$deciles.turn)
    mkt.share <- colMeans(result$deciles.mktshr)
    
    
    ###############################
    # tpl_univariate_portfolio.tex
    ###############################
    for (d in 1:10) {
      tex.uni.pf <- gsub(paste0('R', result$texMarker, sprintf('%02d', d)), format.ret(return.d2m(ret.avg[d])), tex.uni.pf)
      tex.uni.pf <- gsub(paste0('B', result$texMarker, sprintf('%02d', d)), format.beta(betas.avg[d]), tex.uni.pf)
    }
    
    # High-low PF returns
    tex.uni.pf <- gsub(paste0('R', result$texMarker, 'HLr'), format.ret(return.d2m(pf.stats$mean.ret)), tex.uni.pf)
    
    # High-low t-test
    tex.uni.pf <- gsub(paste0('R', result$texMarker, 'HLt'), format.t(pf.stats$mean.ret.ttest), tex.uni.pf)
    
    # Fama-French 5 factor regression
    tex.uni.pf <- gsub(paste0('R', result$texMarker, 'av'), format.ret(return.d2m(pf.stats$alpha.ff)), tex.uni.pf)
    tex.uni.pf <- gsub(paste0('R', result$texMarker, 'at'), format.t(pf.stats$alpha.ff.ttest), tex.uni.pf)
    
    # CAPM regression
    tex.uni.pf <- gsub(paste0('R', result$texMarker, 'a2v'), format.ret(return.d2m(pf.stats$capm.alpha)), tex.uni.pf)
    tex.uni.pf <- gsub(paste0('R', result$texMarker, 'a2t'), format.t(pf.stats$capm.alpha.ttest), tex.uni.pf)
    
    # Sharpe ratios
    tex.uni.pf <- gsub(paste0('SR_', result$texMarker, '_'), sprintf('%.2f', sharpe.ratio(pf.stats$rets, result$ff$rf, 252)), tex.uni.pf)
    
    
    ###################################
    # tpl_portfolio_characteristics.tex
    ###################################
    tex.pf.char <- read_file('tpls/tpl_portfolio_characteristics.tex')
    
    for (d in 1:10) {
      tex.pf.char <- gsub(paste0('RET', sprintf('%02d', d)), format.ret(return.d2m(ret.avg[d])), tex.pf.char)
      tex.pf.char <- gsub(paste0('BE', sprintf('%02d', d)), format.beta(betas.avg[d]), tex.pf.char)
      tex.pf.char <- gsub(paste0('SI', sprintf('%02d', d)), format(round(size.avg[d] / 1e6), big.mark=','), tex.pf.char)
      tex.pf.char <- gsub(paste0('IL', sprintf('%02d', d)), sprintf('%.3f', illiq.avg[d]), tex.pf.char)
      tex.pf.char <- gsub(paste0('TU', sprintf('%02d', d)), format.per(turn.avg[d], 1), tex.pf.char)
      tex.pf.char <- gsub(paste0('SH', sprintf('%02d', d)), format.per(mkt.share[d], 1), tex.pf.char)
    }
    
    write_file(tex.pf.char, paste0(tex.dir, sample.name, '_', result$label, '_portfolio_characteristics.tex'))
    
    
    ######################
    # tpl_beta_std_dev.tex
    ######################
    
    for (d in 1:10) {
      tex.beta.sd <- gsub(paste0('SD', i, sprintf('%02d', d)), sprintf('%.2f', sd(results[[i]]$deciles.betas[, d])), tex.beta.sd)
    }
  }
  
  write_file(tex.uni.pf, paste0(tex.dir, sample.name, '_univariate_portfolio_analysis.tex'))
  write_file(tex.beta.sd, paste0(tex.dir, sample.name, '_beta_std_dev.tex'))
}








