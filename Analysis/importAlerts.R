

library(httr)
library(slackr)
library(data.table)
library(RMySQL)
library(lubridate)


run <- function()
{
  
  
  token <- 'xoxp-12270190368-12266411171-12268828419-52acf7691b'
  slackrSetup(channel="#datasources", api_token=token)

  resp <- GET("https://api.tradeking.com/v1/market/clock.json")
  chr.market.status <- content(resp)$response$status$current

  chr.current_time <- format(Sys.time(), tz="America/New_York",usetz=TRUE)
  
  
  if ( chr.market.status == 'open' & hour(chr.current_time) > 11 ) {
    
    con <- dbConnect(MySQL(), user="carlton", password= '1234password' )
    dt.tradeking <- data.table(dbGetQuery(con, paste0("Select * from import_tradeking.spy where underlying_symbol = 'SPY' and quote_date = '", as.Date(chr.current_time), "'")))
    if( nrow(dt.tradeking) == 0) { text_slackr(paste0( '@carlton', ' There seems to be an issue with import_tradeking.spy. No results have been imported as of ',chr.current_time ), channel = '#datasources', preformatted = F)}
    
    dt.yahoo <- data.table(dbGetQuery(con, paste0("Select * from import_yahoo.spy where quote_date = '", as.Date(chr.current_time), "'")))
    if( nrow(dt.yahoo) == 0) { text_slackr(paste0( '@carlton', ' There seems to be an issue with import_yahoo.spy. No results have been imported as of ',chr.current_time ), channel = '#datasources', preformatted = F)}
    
  }

}




