
# Call the required packages
library(ROAuth)
library(RJSONIO)
library(plyr)
library(data.table)
library(RMySQL)
library(httr)

run <- function()
{
  
  resp <- GET("https://api.tradeking.com/v1/market/clock.json")
  chr.market.status <- content(resp)$response$status$current
  
  
  # Set your application keys
  cKey <- 'QHwV2gr1oxcnPBoTVvCPfR2P6HTbqjI4jDDdgHSF'
  cSecret <- 'GArXBqtOIlxPhp0bs8xL0vFtQZiyXGsM1K7N6oIo'
  oKey <- 'pttTe2RdYMCpAzx79Yr1viRcuKnAnczNwegzFcF6'
  oSecret <- '3pqPGfPFg5oxvK0nJScYjsHo0nzYNvnCjdjkPs0t'
  
  # Set the API endpoint
  tkURL <- "https://api.tradeking.com/v1/market/ext/quotes.json"
  
  credentials <- OAuthFactory$new(consumerKey=cKey,
                                  consumerSecret=cSecret,
                                  oauthKey = oKey, 
                                  oauthSecret = oSecret,
                                  needsVerifier=FALSE,
                                  signMethod='HMAC')
  
  # Update the connection so the handshake is TRUE
  credentials$handshakeComplete <- TRUE
  
  chr.timestamp <- as.character(format(Sys.time(), tz="America/New_York",usetz=TRUE))
  
  ### Get All SPY Options 
  tkURLD <- "https://api.tradeking.com/v1/market/options/search.json"
  
  query <- list()
  query[[ "symbols" ]] <- "spy"
  query[[ "fids" ]] <- "last,bid,ask"
  
  # Make the GET request, passing along the parameters, storing the response, then parsing the response
  response.underlying <- credentials$OAuthRequest(tkURL, query)
  
  
  
  query <- list()
  query[[ "symbol" ]] <- "SPY"
  query[[ "query" ]] <- "xyear-gt:2014"
  response.options <- credentials$OAuthRequest(tkURLD, query, method = "POST")
  
  
  
  temp <- fromJSON(response.options)
  dt.spy_options <- data.table()
  for (i in 1:length(temp$response$quotes$quote))
  {
    tempB <- data.table(as.data.frame(t(temp$response$quotes$quote[[i]])))
    dt.spy_options <- rbind(dt.spy_options, tempB,fill=T)
  }
  
  temp <- fromJSON(response.underlying)
  underlying.spy <- data.table(t(temp$response$quotes$quote))
  
  
  dataset <- dt.spy_options[, .(
    symbol, 
    undersymbol = as.character(undersymbol),
    date = as.character(date),
    high=as.numeric((paste(hi))),
    open=as.numeric((paste(opn))),
    close= as.numeric((paste(cl))),
    low=as.numeric((paste(lo))),
    bid_size = as.numeric((paste(bidsz))),
    ask_size = as.numeric((paste(asksz))),
    issue_desc, 
    put_call,
    xdate, 
    strike = as.numeric((paste(strikeprice))), 
    ask = as.numeric((paste(ask))),
    ask_time,
    bid = as.numeric((paste(bid))),
    bid_time, 
    last = as.numeric((paste(last))),
    dte = as.numeric((paste(days_to_expiration))), 
    imp_volatility = as.numeric((paste(imp_volatility))), 
    irho = as.numeric((paste(irho))), 
    itheta = as.numeric((paste(itheta))),
    ivega = as.numeric((paste(ivega))), 
    idelta = as.numeric((paste(idelta))),
    igamma = as.numeric((paste(igamma))), 
    openinterest = as.numeric((paste(gsub(",", "", openinterest)))), 
    opt_val = as.numeric((paste(opt_val))), 
    pr_openinterest = as.numeric((paste(gsub(",", "", pr_openinterest)))), 
    timestamp = as.numeric((paste(timestamp))),
    vl = as.numeric((paste(gsub(",", "", vl)))), 
    vwap = as.numeric((paste(gsub(",", "", vwap)))), 
    time_added = format(Sys.time(), tz="America/New_York",usetz=TRUE)
  )]
  
  
  
  dataset <-dataset[,list(underlying_symbol = undersymbol,
                quote_date = date,
                root = symbol,
                expiration = paste0(substring(xdate,1,4), '-', substring(xdate,5,6), '-', substring(xdate, 7,8)),
                strike = as.numeric(strike),
                option_type = ifelse(put_call=='put','p','c'),
                open,
                high,
                low,
                close,
                trade_volume= vl,
                bid_size,
                bid, 
                ask_size,
                ask,
                underlying_bid = underlying.spy$bid,
                underlying_ask = underlying.spy$ask,
                underlying_last = underlying.spy$last,
                implied_volatility = imp_volatility,
                delta = idelta,
                gamma = igamma,
                theta = itheta,
                vega = ivega,
                rho = irho,
                vwap = vwap,
                open_interest = openinterest,
                timestamp = chr.timestamp)]
  
  
  if (chr.market.status == 'open') { 
    con <- dbConnect(MySQL(), user="carlton", password= '1234password',db='import_tradeking' )
    dbWriteTable(con, value = data.frame(dataset), name = "spy", append = TRUE, row.names=F ) 
  }
  
  


}





