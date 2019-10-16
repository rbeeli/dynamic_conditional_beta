library(data.table)
library(rmgarch)
library(doParallel)
library(tictoc)
library(benchmarkme)


# load excess returns data
fitData <- fread('../../../0 data/CRSP/sp100_stock_ex_returns_demeaned_scaled.csv', header=T, sep=';')
head(fitData[, 1:10], n=3)

dates <- as.vector(fitData$date)
market <- as.matrix(fitData[, 'market'])
stocks <- as.matrix(fitData[, !c('date', 'market')])
stocks.cols <- colnames(stocks)

head(dates, n=3)
head(market, n=3)
head(stocks[, 1:10], n=3)


# rolling window configuration
windowSize <- 252
subsampleSize <- 1
to <- nrow(fitData)
from <- windowSize
#from <- 8689 # 1998-01
steps <- to - from + 1


# function to compute estimated and forecasted covariances
computeStep <- function(i) {
  # latest date for window
  date <- dates[i]
  
  # output directories
  outDir1 <- paste0('out_sp100/', date, '_pending')
  outDir2 <- paste0('out_sp100/', date)
  
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
  tic(paste('step', i - from + 1, 'of', steps, '(', date, ')'))
  
  # extract window data
  fit.market <- market[(i - windowSize + 1):i, ]
  fit.stocks <- stocks[(i - windowSize + 1):i, ]
  
  # omit columns in window with NA values
  fit.stocks <- fit.stocks[, colSums(is.na(fit.stocks)) == 0]
  
  # fit model stepwise in batches (sub-sampling) to circumvent convergence issues
  errors <- 0
  fit.steps <- ceiling(ncol(fit.stocks) / subsampleSize)
  
  # Fama-French market excess returns variances
  vars <- matrix(NA, nrow=2, ncol=fit.steps, dimnames=list(c('estimated', '1stepahead'), 1:fit.steps))
  
  # market to stocks covariances
  covs <- matrix(NA, nrow=2, ncol=ncol(fit.stocks), dimnames=list(c('estimated', '1stepahead'), colnames(fit.stocks)))
  
  # # information criterias
  # infocriterias <- matrix(NA, nrow=fit.steps, ncol=4, dimnames=list(c(), c('Akaike', 'Bayes', 'Shibata', 'Hannan-Quinn')))
  # 
  # # DCC and GARCH market coefficients per sub-sampling step
  # coefs.dcc.market.cols <- c(
  #   'dcca1', 'dccb1',
  #   '[market].mu', '[market].omega', '[market].alpha1', '[market].beta1')
  # coefs.dcc.market <- matrix(NA, nrow=fit.steps, ncol=length(coefs.dcc.market.cols), dimnames=list(c(), coefs.dcc.market.cols))
  # 
  # # GARCH stock coefficients
  # # [PERMNO].mu, [PERMNO].omega, [PERMNO].alpha1, [PERMNO].beta1
  # coefs.stocks.cols <- c(apply(as.matrix(colnames(fit.stocks)), 1, function(x) c(paste0('[', x, '].mu'), paste0('[', x, '].omega'), paste0('[', x, '].alpha1'), paste0('[', x, '].beta1'))))
  # coefs.stocks <- matrix(NA, nrow=1, ncol=length(coefs.stocks.cols), dimnames=list(c(), coefs.stocks.cols))
  
  for (step in 1:fit.steps) {
    step.from <- (step - 1) * subsampleSize + 1
    step.to <- min(step.from + subsampleSize - 1, ncol(fit.stocks))
    step.data <- cbind(fit.market, fit.stocks[, step.from:step.to]) # drops colnames of fit.stocks if step.from == step.to!!!
    step.stockCols <- colnames(fit.stocks)[step.from:step.to]
    colnames(step.data) <- c('market', step.stockCols) # set colnames again
    
    # fit Gaussian DCC-GARCH model
    uspec <- ugarchspec(mean.model=list(armaOrder=c(0, 0), include.mean=T),
                        variance.model=list(model="sGARCH", garchOrder=c(1,1), variance.targeting=F),
                        distribution.model="norm")
    
    spec <- dccspec(uspec=multispec(replicate(ncol(step.data), uspec)),
                    dccOrder=c(1,1),
                    distribution="mvnorm")
    
    fit <- tryCatch(dccfit(data=step.data, spec=spec, solver=c('hybrid', 'solnp'), fit.control=list(eval.se=F, scale=F)),
                    error=function(e) NULL)
    
    # one-step ahead forecast
    if (is.null(fit) || is(fit, "uGARCHmultifit")) {
      # in some cases, fit fails because of no convergence - log message
      cat('dccfit() failed at position', i, 'for date', date, 'for window', step.from, '-', step.to, '\n')
      errors <- errors + 1
    }
    else {
      forecast <- dccforecast(fit, n.ahead=1, n.roll=0)
      
      # last estimated covariance
      cov.estimated <- rcov(fit)[ , , nrow(step.data)]
      
      # 1-step ahead forecasted covariance
      cov.1stepahead <- rcov(forecast)[[1]][ , , 1]
      
      # save results
      vars['estimated', step] <- cov.estimated['market', 'market']
      vars['1stepahead', step] <- cov.1stepahead['market', 'market']
      
      covs['estimated', step.stockCols] <- cov.estimated['market', step.stockCols]
      covs['1stepahead', step.stockCols] <- cov.1stepahead['market', step.stockCols]
      
      # infocriterias[step, ] <- infocriteria(fit)[, 1]
      
      step.coefs.garch <- coef(fit, 'garch')
      step.coefs.dcc <- coef(fit, 'dcc')
      
      step.coefs.stocks <- step.coefs.garch[5:length(step.coefs.garch)]
      step.coefs.dcc.market <- c(step.coefs.dcc, step.coefs.garch[1:4])
      
      # coefs.stocks[, names(step.coefs.stocks)] <- step.coefs.stocks
      # coefs.dcc.market[step, names(step.coefs.dcc.market)] <- step.coefs.dcc.market
    }
  }
  
  tt <- toc()
  tt
  
  # write result
  runtime=tt$toc - tt$tic
  
  fwrite(data.frame(errors=errors, runtime=runtime), paste0(outDir1, '/info.csv'), sep=';')
  fwrite(as.data.frame(vars), paste0(outDir1, '/vars.market.csv'), sep=';', row.names=T)
  fwrite(as.data.frame(covs), paste0(outDir1, '/covs.stocks.csv'), sep=';', row.names=T)
  # fwrite(as.data.frame(infocriterias), paste0(outDir1, '/infocriterias.csv'), sep=';')
  # fwrite(as.data.frame(coefs.dcc.market), paste0(outDir1, '/coefs.dcc.market.csv'), sep=';')
  # fwrite(as.data.frame(coefs.stocks), paste0(outDir1, '/coefs.stocks.csv'), sep=';')
  
  # rename directory (remove `_pending`)
  if (dir.exists(outDir2))
    unlink(outDir2, recursive=T, force=T)
  
  file.rename(outDir1, outDir2)
  
  return(T)
}



# tic('Total runtime')
# 
# # create parallel cluster
# cores <- detectCores()
# cl <- makeCluster(cores, outfile='parallel-log.txt')
# registerDoParallel(cl, cores=cores)
# 
# 
# cat('Period:       ', from, 'to', to, '(', dates[from], 'to', dates[to], ') \n')
# cat('Window size:  ', windowSize, ' \n')
# cat('Machine:      ', get_cpu()$model_name, '\n')
# cat('Cores:        ', get_cpu()$no_of_cores, '\n')
# cat('Memory:       ', get_ram()/1024/1024, 'MB \n')
# cat('System info: \n')
# get_sys_details()$sys_info
# 
# cat('--------------------------------------------------\n')
# cat('Started at:', as.character(Sys.time()), ' \n')
# 
# predictedCovs <- foreach(i=from:to, .packages=c('rmgarch', 'tictoc', 'data.table'), .verbose=F) %dopar% {
#   computeStep(i)
# }
# 
# stopCluster(cl)
# 
# toc()
# 
# cat('Finished at:', as.character(Sys.time()), ' \n')





########################
# dev

for (i in from:to) {
  computeStep(i)
}











