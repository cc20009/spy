
# Call the required packages
library(ROAuth)
library(RJSONIO)
library(plyr)
library(data.table)
library(RMySQL)
library(httr)
library(lubridate)
library(httpuv)
library(ggplot2)



con <- dbConnect(MySQL(), user="carlton", password= '1234password',db='import_tradeking' )

cacheObject <- function(mxt.data, chr.location, chr.name) {
  
  suppressWarnings(dir.create(paste0('/home/ubuntu/cache/',chr.location)))
  saveRDS(mxt.data, paste0('/home/ubuntu/cache/',chr.location,'/',chr.name, '.rds'))
  
}


getCachedObject <- function(chr.location, chr.name) {
  
  dt <- readRDS(paste0('/home/ubuntu/cache/',chr.location,'/',chr.name,'.rds'))
  return(dt)
  
}


describeCache <- function(chr.location) {
  
  chr.directory <- list.files(paste0('/home/ubuntu/cache/', chr.location))
  return(chr.directory)
  
}





organizeDataLivevol <- function(dt) {
  
  dt <-  dt[,list(symbol=underlying_symbol, 
                  trade_date=as.character(quote_date),
                  root, 
                  expiration=as.character(expiration),
                  strike,
                  call_put=option_type, 
                  bid=bid_1545, 
                  ask=ask_1545, 
                  underlying = underlying_ask_1545,
                  distance_from_underlying = ifelse(option_type=='c', underlying_ask_1545 - strike, strike- underlying_ask_1545),
                  iv = implied_volatility_1545,
                  vol = trade_volume,
                  delta = delta_1545,
                  gamma = gamma_1545,
                  theta = theta_1545, 
                  vega = vega_1545,
                  rho = rho_1545,
                  vwap,
                  trade_volume,
                  strip=paste0(substring(expiration,1,8),'01'))]
  
  return(dt)
  
  
}





organizeDataTradeking <- function(dt) {
  
  dt <-  dt[,list(symbol=underlying_symbol, 
                  trade_date=as.character(quote_date),
                  root=underlying_symbol, 
                  expiration=as.character(expiration),
                  strike,
                  call_put=option_type, 
                  bid, 
                  ask, 
                  underlying = underlying_ask,
                  distance_from_underlying = ifelse(option_type=='c', underlying_ask - strike, strike- underlying_ask),
                  iv = implied_volatility,
                  vol = trade_volume,
                  delta,
                  gamma,
                  theta, 
                  vega,
                  rho,
                  vwap,
                  trade_volume,
                  strip=paste0(substring(expiration,1,8),'01'))]
  
  return(dt)
  
  
}





# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.dim)
  names(out) <- c("Type", "Size", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}
# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}



