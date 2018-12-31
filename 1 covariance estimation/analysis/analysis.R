library(data.table)
library(grDevices)
library(tikzDevice)

source('../../functions.R')

out.dir <- 'output/'

covs.stocks.DCC1step <- remove.colnames.prefixes(fread('../Matlab/DCC1step/consolidated/covs.1stepahead.csv', header=T, sep=';'), 'x')
vars.market.DCC1step <- remove.colnames.prefixes(fread('../Matlab/DCC1step/consolidated/vars.1stepahead.market.csv', header=T, sep=';'), 'x')
covs.stocks.COMFORT.memless <- remove.colnames.prefixes(fread('../Matlab/COMFORT-DCC/consolidated/covs.1stepahead.csv', header=T, sep=';'), 'x')
vars.market.COMFORT.memless <- remove.colnames.prefixes(fread('../Matlab/COMFORT-DCC/consolidated/vars.1stepahead.market.csv', header=T, sep=';'), 'x')

dates <- intersect(covs.stocks.COMFORT.memless$date, covs.stocks.DCC1step$date)

covs.stocks.DCC1step <- covs.stocks.DCC1step[which(covs.stocks.DCC1step$date %in% dates), ]
vars.market.DCC1step <- vars.market.DCC1step[which(vars.market.DCC1step$date %in% dates), ]

covs.stocks.COMFORT.memless <- covs.stocks.COMFORT.memless[which(covs.stocks.COMFORT.memless$date %in% dates), ]
vars.market.COMFORT.memless <- vars.market.COMFORT.memless[which(vars.market.COMFORT.memless$date %in% dates), ]

covs.stocks.DCC1step <- covs.stocks.DCC1step[, !"date", with=F]
vars.market.DCC1step <- vars.market.DCC1step[, !"date", with=F]

covs.stocks.COMFORT.memless <- covs.stocks.COMFORT.memless[, !"date", with=F]
vars.market.COMFORT.memless <- vars.market.COMFORT.memless[, !"date", with=F]


plot.idxs <- which(colSums(!is.na(covs.stocks.COMFORT.memless)) > 0)
plot.cols <- colnames(covs.stocks.COMFORT.memless)[plot.idxs]

stocks.estimation.errors <- c('59176', '59408', '66800', '68144',
                              '71563', '82775', '84129', '86314')

plot.cols <- stocks.estimation.errors


pdf(paste0(out.dir, 'beta_SP500_stocks.pdf'))

par(mfrow=c(1,1))
par(mar=c(5, 3, 3, 2.1))

labels <- sapply(dates, function(d) format(as.Date(d), '%m.%Y'))
years <- unique(labels)
years.month <- paste0('01.', years)
at <- c(seq(from=1, to=nrow(covs.stocks.DCC1step)-1, by=nrow(covs.stocks.DCC1step) / 10), nrow(covs.stocks.DCC1step))
labels <- labels[at]

for (p in plot.cols) {
  beta.DCC1step <- as.matrix(covs.stocks.DCC1step[, ..p]) / as.matrix(vars.market.DCC1step$median)
  beta.COMFORT.median <- as.matrix(covs.stocks.COMFORT.memless[, ..p]) / as.matrix(vars.market.COMFORT.memless$median)
  p.data <- cbind(beta.DCC1step, beta.COMFORT.median)
  
  cols <- c('red', adjustcolor('blue', alpha.f=.4))
  matplot(p.data, type='l', xaxt='n', lty=1, lwd=0.25, xaxs='i', col=cols, main=paste0('Beta of S&P 500 stock #', p, ' with market'))
  axis(1, at=at, labels=labels, cex.axis=.7, tck=-0.03)
  grid(length(labels)-1, NA, lty=1, col=adjustcolor('gray', alpha.f=.4))
  legend('topleft', legend=c('DCC-GARCH', 'COMFORT-DCC'), col=cols, lty=1, cex=.7, bg="transparent", bty='n')
}

dev.off()






pdf(paste0(out.dir, 'covariance_SP500_stocks_with_market.pdf'))

par(mfrow=c(1,1))
par(mar=c(5, 3, 3, 2.1))

labels <- sapply(dates, function(d) format(as.Date(d), '%m.%Y'))
years <- unique(labels)
years.month <- paste0('01.', years)
at <- c(seq(from=1, to=nrow(covs.stocks.DCC1step)-1, by=nrow(covs.stocks.DCC1step) / 10), nrow(covs.stocks.DCC1step))
labels <- labels[at]

stocks <- c()

for (p in plot.cols) {
  data.plot.DCC1step <- as.matrix(covs.stocks.DCC1step[, ..p])
  data.plot.COMFORT.memless <- as.matrix(covs.stocks.COMFORT.memless[, ..p])
  
  cols <- c( adjustcolor('blue', alpha.f=.8), adjustcolor('red', alpha.f=.6))
  matplot(cbind(data.plot.DCC1step, data.plot.COMFORT.memless), type='l', xaxt='n', ylab='', lty=1, lwd=0.25, xaxs='i',
          col=cols, main=paste0('Covariance of S&P 500 stock #', p, ' with market'))
  axis(1, at=at, labels=labels, cex.axis=.7, tck=-0.03)
  legend('topleft', legend=c('DCC-GARCH', 'COMFORT-DCC'), col=cols, lty=1, cex=.8, bg="transparent", bty='n')
  legend('top', legend=c(paste0('PERMNO ', p)),  bty='n')
  
  # action <- readline(prompt="Consider stock? (0=yes, X=no) ")
  # if (as.integer(action) == 1) {
  #   stocks <- c(stocks, p)
  # }
}

dev.off()



# .tex output
tikz(file=paste0(out.dir, 'erroneous_covariances.tex'), width=6, height=7.5)

par.orig <- par(no.readonly=T)

par(mfrow=c(4, 2), mar=c(0, 0, 0, 0))

labels <- sapply(dates, function(d) format(as.Date(d), '%m.%Y'))
years <- unique(labels)
years.month <- paste0('01.', years)
at <- c(seq(from=1, to=nrow(covs.stocks.DCC1step)-1, by=nrow(covs.stocks.DCC1step) / 10), nrow(covs.stocks.DCC1step))
labels <- labels[at]

stocks <- c()


for (p in plot.cols) {
  data.plot.DCC1step <- as.matrix(covs.stocks.DCC1step[, ..p])
  data.plot.COMFORT.memless <- as.matrix(covs.stocks.COMFORT.memless[, ..p])
  
  cols <- c( adjustcolor('blue', alpha.f=.8), adjustcolor('red', alpha.f=.6))
  matplot(cbind(data.plot.DCC1step, data.plot.COMFORT.memless), type='l', xaxt='n', yaxt='n', ylab='', lty=1, lwd=0.25, col=cols, xaxs='i')
  legend('topleft', legend=c('DCC-GARCH', 'COMFORT-DCC'), col=cols, lty=1, cex=1, bg="transparent", bty='n')
  legend('top', legend=c(paste0('PERMNO ', p)), bty='n', cex=1.1)
}

dev.off()

par(par.orig)



pdf(paste0(out.dir, 'variance_Fama-French_market.pdf'))

par(mfrow=c(1,1))
par(mar=c(5, 3, 3, 2.1))

labels <- sapply(dates, function(d) format(as.Date(d), '%m.%Y'))
years <- unique(labels)
years.month <- paste0('01.', years)
at <- c(seq(from=1, to=nrow(data.plot.DCC1step)-1, by=nrow(data.plot.DCC1step) / 10), nrow(data.plot.DCC1step))
labels <- labels[at]

data.plot.DCC1step <- as.matrix(vars.market.DCC1step[, 'median'])
data.plot.COMFORT.memless <- as.matrix(vars.market.COMFORT.memless[, 'median'])

cols <- c('red', adjustcolor('blue', alpha.f=.4), adjustcolor('black', alpha.f=.4))

# plot full ylim
matplot(cbind(data.plot.DCC1step, data.plot.COMFORT.memless), type='l', xaxt='n', main=paste0('Variance of market (Fama-French)'),
        lty=1, lwd=0.25, xaxs='i', col=cols)
axis(1, at=at, labels=labels, cex.axis=.7, tck=-0.03)
grid(length(labels)-1, NA, lty=1, col=adjustcolor('gray', alpha.f=.4)) 
legend('topleft', legend=c('DCC-GARCH', 'COMFORT memoryless'), col=cols, lty=1, cex=.7, bg="transparent", bty='n')

dev.off()




