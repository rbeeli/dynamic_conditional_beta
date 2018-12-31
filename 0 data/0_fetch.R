library(RPostgres)
library(plyr)
library(data.table)
library(readr)

# break on warnings
options(warn=2)


# sample period
# from <- '1963-07-01'
from <- '1996-01-01'
to <- '2017-12-31'


# connect to WRDS database
wrds <- dbConnect(Postgres(), host='wrds-pgdata.wharton.upenn.edu', port=9737, dbname='wrds', sslmode='require', user='rbeeli', password=read_file('WRDS_pwd.txt'))


# query S&P 500 single stock returns including delisting information, market cap and volume
res <- dbSendQuery(wrds, paste0(
    "select b.permno, b.date, b.ret, d.dlret, d.dlstcd, (ABS(b.prc) * (b.shrout * 1000)) as cap, GREATEST(b.vol, 0) as vol, ABS(b.prc) as prc, (b.shrout * 1000) as shrout
       from crsp.dsp500list a
       join crsp.dsf b on b.permno=a.permno
  left join crsp.dse d on d.permno=b.permno and d.date=b.date and d.dlstcd is not null
      where b.date >= a.start and b.date <= a.ending
        and b.date >= '", from, "' and b.date <= '", to, "'
   order by b.date"))
sp500StocksData <- dbFetch(res, n=-1)
dbClearResult(res)

# query S&P 500 index returns
#   vwretd - Value-Weighted Return (includes distributions)
#   ewretd - Equal-Weighted Return (includes distributions)
res <- dbSendQuery(wrds, paste0(
  "select caldt as date, vwretd, ewretd
     from crsp.dsp500
    where caldt >= '", from, "' and caldt <= '", to, "'"))
sp500IndexData <- dbFetch(res, n=-1)
dbClearResult(res)

# close connection
dbDisconnect(wrds)
rm(res)
rm(wrds)

# write to CSV files
fwrite(sp500IndexData, 'CRSP/sp500_index.csv', sep=';')
fwrite(sp500StocksData, 'CRSP/sp500_stocks.csv', sep=';')




# https://wrds-www.wharton.upenn.edu/pages/support/wrds-cloud/r-wrds-cloud/accessing-wrds-data-r/
#
# res <- dbSendQuery(wrds, "select distinct table_schema
#                    from information_schema.tables
#                    where table_type ='VIEW'
#                    or table_type ='FOREIGN TABLE'
#                    order by table_schema")
# data <- dbFetch(res, n=-1)
# dbClearResult(res)
# data
# 
# 
# res <- dbSendQuery(wrds, "select distinct table_name
#                    from information_schema.columns
#                    where table_schema like 'crsp%'
#                    order by table_name")
# data <- dbFetch(res, n=-1)
# dbClearResult(res)
# data
# 
# res <- dbSendQuery(wrds, "select column_name, data_type
#                    from information_schema.columns
#                    where table_schema like 'crsp' and table_name='msenames'")
# data <- dbFetch(res, n=-1)
# dbClearResult(res)
# data