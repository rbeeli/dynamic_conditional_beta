library(data.table)
library(rmgarch)
library(doParallel)
library(tictoc)
library(graphics)

returns <- fread('../../../0 data/CRSP/excess_returns.csv', header=T)

# remove date
returns <- as.matrix(returns[, -c(1)])

# demean data
returns <- sweep(returns, 2, apply(returns, 2, function(x) mean(x, na.rm=T)))

# scale to avoid numerical optimization issues (convergence)
returns <- returns * 1000



for (wnd in 1:5) {
  
  # get one random window
  pos <- as.integer(runif(1, min=1, max=nrow(returns) - 1000))
  data <- as.matrix(returns[pos:(pos + 252 - 1), ])
  
  # omit columns with NAs
  data <- data[, colSums(is.na(data)) == 0]
  
  
  # extract market and one fixed asset time series
  market <- data[, 1]
  assetX <- data[, ncol(data)]
  
  data <- data[, 2:(ncol(data)-1)]
  
  
  
  # DCC-GARCH model
  uspec2 <- ugarchspec(mean.model=list(armaOrder=c(0, 0), include.mean=F), variance.model=list(model="sGARCH", garchOrder=c(1, 1), variance.targeting=F), distribution.model="norm")
  
  steps.calc <- function(n, size) {
    ceiling(n / size)
  }
  
  stepSizes <- c(8, 16, 32, 48, 64, 96, 128)
  coefsA1 <- matrix(NA, ncol=steps.calc(ncol(data), min(stepSizes)), nrow=length(stepSizes))
  coefsA2 <- matrix(NA, ncol=steps.calc(ncol(data), min(stepSizes)), nrow=length(stepSizes))
  vars <- matrix(NA, ncol=steps.calc(ncol(data), min(stepSizes)), nrow=length(stepSizes))
  covars <- matrix(NA, ncol=steps.calc(ncol(data), min(stepSizes)), nrow=length(stepSizes))
  times <- rep(0, length(stepSizes))
  
  for (i in 1:length(stepSizes)) {
    stepSize <- stepSizes[i]
    steps <- steps.calc(ncol(data), stepSize)
    
    cat(sprintf("\nstep size: %i (%i steps) \n", stepSize, steps))
    
    tic('time')
    
    for (step in 1:steps) {
      cat(sprintf("%i ", step))
      
      from <- (step - 1) * stepSize + 1
      to <- min(from + stepSize - 1, ncol(data))
      
      fitData <- cbind(market, assetX, data[, from:to])
      
      # fit
      spec <- dccspec(uspec=multispec(replicate(ncol(fitData), uspec2)), dccOrder=c(1, 1), distribution="mvnorm")
      fit <- dccfit(data=fitData, spec=spec, solver=c('hybrid', 'solnp'), fit.control=list(eval.se=F, scale=F))
      # nlminb
      
      coefsA1[i, step] <- coef(fit)['[Joint]dcca1']
      coefsA2[i, step] <- coef(fit)['[Joint]dccb1']
      
      # one-step ahead forecast
      forecast <- dccforecast(fit, n.ahead=1, n.roll=0)
      
      
      # returns the time-varying NxN covariance matrix in array format
      H <- rcov(forecast)[[1]][,,1]
      
      vars[i, step] <- H[1,1] # DCC variance
      covars[i, step] <- H[1,2] # DCC covariance
      
      # R <- rcor(forecast)[[1]][,,1]
      # forecast@model$sigma[1,1]^2 # univariate GARCH variance
    }
    
    time <- toc()
    time
    
    times[i] <- time$toc - time$tic
    
    cat('\n')
  }
  
  
  
  cols <- 2:(2+length(stepSizes))
  leg <- apply(as.matrix(stepSizes), 1, function(x) paste0('sub-sample size=', x))
  
  
  par(mfrow=c(2,2))
  matplot(t(coefsA1), type='o', ylim=c(0, 1), col=cols, main=expression('DCC coefficient a'[1]), ylab=expression('a'[1]), xlab="step")
  legend('topleft', legend=leg, col=cols, fill=cols, cex=1, pt.cex=1.2, box.lty=0, bg='transparent', inset=0)
  
  matplot(t(coefsA2), type='o', ylim=c(0, 1), col=cols, main=expression('DCC coefficient a'[2]), ylab=expression('a'[2]), xlab="step")
  legend('bottomleft', legend=leg, col=cols, fill=cols, cex=1, pt.cex=1.2, box.lty=0, bg='transparent', inset=0)
  
  matplot(t(vars), type='o', ylim=c(0, 65), col=cols, main='market variance', xlab="step")
  legend('bottomleft', legend=leg, col=cols, fill=cols, cex=1, pt.cex=1.2, box.lty=0, bg='transparent', inset=0)
  
  matplot(t(covars), type='o', ylim=c(0, 65), col=cols, main='market-assetX covariance', xlab="step")
  legend('bottomleft', legend=leg, col=cols, fill=cols, cex=1, pt.cex=1.2, box.lty=0, bg='transparent', inset=0)
  
  par(mfrow=c(1,1))
  plot(stepSizes, times/60, type='l', ylim=c(0, max(times/60)), col=cols, xlab='number of assets (columns) in sub-sample', ylab="runtime in minutes", main="runtimes per 252x472 window of different step sizes")

}













