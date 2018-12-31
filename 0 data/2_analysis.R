library(data.table)

data.stocks <- fread('CRSP/sp100_stock_returns.csv', header=T)
data.stocks.raw <- fread('CRSP/sp100_stocks.csv', header=T)


# show data rows before and after a delisting has occured
has.dlret <- which(!is.na(data.stocks.raw$dlret))

for (i in has.dlret) {
  date <- data.stocks.raw[i, ]$date
  stock.permno <- data.stocks.raw[i, ]$permno
  stock.data <- data.stocks.raw[data.stocks.raw[,  permno==stock.permno], ]
  date.idx <- which(stock.data$date == date)
  
  range <- (date.idx-1):(date.idx+1)
  print(stock.data[range, ])
  print('----------------------------')
}
