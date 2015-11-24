
library(XML)
library(stringr)
library(data.table)
library(rvest)
library(RMySQL)
library(httr)

run <- function()
{
  
  resp <- GET("https://api.tradeking.com/v1/market/clock.json")
  chr.market.status <- content(resp)$response$status$current
  
  
  
  spy_option <- read_html("http://finance.yahoo.com/q/op?s=SPY+Options")
  dt.timecodes <- data.table(unix_timecodes = spy_option %>% html_nodes("option") %>% html_attr("value"))
  dt.timecodes[,expiration:= substring(as.POSIXct(as.numeric(unix_timecodes), origin="1970-01-01"), 1,10)]
  
  datetime <- Sys.time()
  
  dt.allOptionChains <- data.table()
  for ( i in 1:nrow(dt.timecodes)) {
    
  
    url <- dt.timecodes[i,]
    
    spy_option <- read_html(paste0("http://finance.yahoo.com/q/op?s=SPY&date=", url$unix_timecodes))
    chr.underlying <- as.character(spy_option %>% html_nodes(xpath = '//*[@id="yfs_l84_SPY"]'))
    int.underlying <- as.numeric(substring(chr.underlying, 
                                           gregexpr("[0-9][0-9][0-9].[0-9][0-9]", chr.underlying)[[1]][1],
                                           gregexpr("[0-9][0-9][0-9].[0-9][0-9]", chr.underlying)[[1]][1] +  5 ))
    
    
    dt.option_chain <- readHTMLTable(paste0("http://finance.yahoo.com/q/op?s=SPY&date=", url$unix_timecodes))
    
    dt.1 <- data.frame(dt.option_chain[2])
    names(dt.1) <- c("Strike", "Contract_Name", "Last", "Bid", "Ask", "Change", "pct_Change", "Volume", "Open_Interest", "IV")
    dt.1 <- data.table(dt.1)
    
    dt.2 <- data.frame(dt.option_chain[3])
    names(dt.2) <- c("Strike", "Contract_Name", "Last", "Bid", "Ask", "Change", "pct_Change", "Volume", "Open_Interest", "IV")
    dt.2 <- data.table(dt.2)
    
    dt.options <- rbind(dt.1, dt.2)
    dt.options <- dt.options[,list(strike=as.numeric(as.character(Strike)),
                                   contract_name=as.character(Contract_Name),
                                   call_put = ifelse(grepl('C', as.character(Contract_Name)) == T , 'c', 'p'),
                                   expiration = as.character(url$expiration),
                                   last=as.numeric(as.character(Last)),
                                   bid=as.numeric(as.character(Bid)),
                                   ask=as.numeric(as.character(Ask)),
                                   change=as.numeric(as.character(Change)),
                                   pct_change=as.numeric(as.character(substring(pct_Change, 1, nchar(as.character(pct_Change)) - 1))),
                                   volume=as.numeric(as.character(Volume)),
                                   oi= as.numeric(as.character(Open_Interest)),
                                   iv = as.numeric(as.character(substring(IV, 1, nchar(as.character(IV)) - 1)))/100,
                                   underlying = int.underlying,
                                   timestamp = format(datetime, tz="America/New_York",usetz=TRUE),
                                   quote_date = substring(format(datetime, tz="America/New_York",usetz=TRUE), 1, 10))]
    
    
    dt.allOptionChains <- rbind(dt.allOptionChains, dt.options)
  }
  
  chr.current.date <- as.Date(format(Sys.time(), tz="America/New_York",usetz=TRUE))
  chr.current.hour <- as.numeric(substring(format(Sys.time(), tz="America/New_York",usetz=TRUE), 12, 13))
  
  if (chr.market.status == 'open') { 
    system(command = paste("logger -p local0.info \"", 'writing data', "\""))
    con <- dbConnect(dbDriver("MySQL"), user = "carlton", password = "1234password", dbname = "import_yahoo")
    dbWriteTable(con, value = dt.allOptionChains, name = "spy", append = TRUE, row.names=F ) 
  }


}

