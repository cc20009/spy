

library(data.table)
library(RMySQL)

run <- function()
{
  
  con <- dbConnect(MySQL(), user="carlton", password= '1234password', db='tradeking' )
  dt.dates <- data.table(dbGetQuery(conn = con,statement = "select distinct quote_date from import_tradeking.spy"))
  dt.dates.already.inserted <- data.table(dbGetQuery(conn = con,statement = "select distinct quote_date from tradeking.spy"))
  
  dt.insert.dates <- setdiff(dt.dates$quote_date, dt.dates.already.inserted$quote_date)
  for ( i in dt.insert.dates) {
    chr.max.timestamp <- data.table(dbGetQuery(conn = con,
                                               statement = paste0("select max(timestamp) timestamp from import_tradeking.spy 
                                                              where quote_date in 
                                                             ( '", i, "' )")))
    dt.tradeking <- data.table(dbGetQuery(conn = con,
                                          statement = paste0("select * from import_tradeking.spy 
                                                              where quote_date in 
                                                             ( '", i, "' ) and timestamp = '",chr.max.timestamp, "'" )))
    
    dt.tradeking.insert <- dt.tradeking[,list(underlying_symbol,
                       quote_date,
                       expiration,
                       strike,
                       option_type,
                       trade_volume,
                       bid,
                       ask,
                       underlying_ask,
                       implied_volatility,
                       delta,
                       gamma,
                       theta,
                       vega,
                       rho,
                       vwap,
                       timestamp)]
    
    
    dbWriteTable(con, value = dt.tradeking.insert, name = "spy", append = TRUE, row.names=F ) 
  }

}
