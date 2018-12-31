# removes the given prefix from all colnames of given matrix
remove.colnames.prefixes <- function(tbl, prefix) {
  colnames(tbl) <- unname(sapply(colnames(tbl), function(x) sub(prefix, '', x)))
  return(tbl)
}

# return rows of matrix which have same dates (column `date`)
merge.by.date <- function(dates, mat) {
  if (length(which(!dates %in% mat$date)) > 0) {
    stop('Not all dates in matrix')
  }
  
	mat[which(mat$date %in% dates), ]
}

# applies the first column as rownames of the dataframe and removes it
first.col.2.rownames <- function(dataframe) {
  rownames(dataframe) <- dataframe[, 1]
  dataframe <- dataframe[, -c(1)]
  return(dataframe)
}

# check if two matrices are equal
matequal <- function(x, y) {
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
}

# converts daily returns to monthly returns by compounding them 21 times.
return.d2m <- function(daily) {
  (1 + daily) ^ 21 - 1
}

# converts monthly returns to daily returns by taking the 1/21th root.
return.m2d <- function(monthly) {
  (1 + monthly) ^ (1/21) - 1
}


# Sortino ratio
sortino.ratio <- function(returns, MAR, scale) {
  ex.rets <- returns - MAR
  downside.risk <- sqrt(sum(1/length(ex.rets) * ex.rets[which(ex.rets < 0)]^2))
  ratio <- mean(ex.rets) / downside.risk
  return(ratio * sqrt(scale))
}

# Sharpe ratio
sharpe.ratio <- function(returns, rf, scale) {
  return(mean(returns - rf) / sd(returns - rf) * sqrt(scale))
}





# repeat given vector as column n times, returns a matrix
rep.col <- function(x, n) {
  matrix(rep(x, each=n), ncol=n, byrow=T)
}

rep.row <- function(x, n){
  matrix(rep(x,each=n), nrow=n)
}



# returns table without the given columns
cols.omit <- function(dt, cols.omit) {
  if (is.null(dt))
    return(NULL)
  
  cols <- colnames(dt)
  cols.select <- cols[!cols %in% cols.omit]
  
  return(dt[, cols.select, with=F])
}









