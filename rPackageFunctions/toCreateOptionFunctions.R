





updateMarks <- function(data=dt.spy,  dt.portfolio, chr.trade_date) {
  
  dt.slim <- data[trade_date==chr.trade_date]
  dt.portfolio.slim <- dt.portfolio[trade_date == max(dt.portfolio$trade_date)]
  #remove closed trades
  dt.portfolio.slim <- dt.portfolio.slim[!trade_id %in% dt.portfolio.slim[qty==0]$trade_id]
  if (nrow(dt.portfolio.slim) == 0) { return (dt.portfolio) }
  dt.update <- merge(dt.slim, dt.portfolio.slim[,list(symbol,trade_date=chr.trade_date, expiration, strike, call_put, qty, trade_id)], by=c('symbol','trade_date','expiration','strike','call_put'))
  dt.portfolio <- rbind(dt.portfolio, dt.update)
  return(dt.portfolio)
  
  
}



getPortfolioMtm <- function(dt.portfolio) {
  
  dt.mtm <- data.table()
  for ( j in unique(dt.portfolio$trade_id)) {
    temp <- dt.portfolio[trade_id == j][qty!=0][order(trade_date)]
    temp[,yest_ask:=c(NA, temp[,ifelse(qty>0, ask, bid)*qty][-nrow(temp)])]
    temp[,dmtm:=ifelse(is.na(yest_ask) == T, 0,ifelse(qty>0, ask, bid)*qty-yest_ask)]
    dt.mtm <- rbind(dt.mtm, temp)[order(trade_id, trade_date)]
    
  }
  return(dt.mtm)
  
}



getExpirations <- function(connSettings=con) {
  
  dt.expirations <- data.table(dbGetQuery(conn = connSettings,statement = 'select expiration, month(expiration) month, year(expiration) year, sum(trade_volume) trade_volume 
                                     from import_livevol.spy group by expiration  '))
  dt.expirations <-  merge(dt.expirations,dt.expirations[,list(trade_volume=max(trade_volume)), by=list(month,year)], by=c('month','year','trade_volume'))
  return(dt.expirations[,list(expiration)])
}


getBestOption <- function(data=dt.spy, 
                          chr.trade_date, 
                          chr.expiration, 
                          chr.option_type, 
                          int.min_delta = NULL,
                          int.strike = NULL,
                          chr.money_in_out = NULL, 
                          int.min_dollar = NULL ) {
  
  if(nrow(data[chr.trade_date==trade_date]) == 0) {warning("No results"); return(NA)}
  if(nrow(data[chr.expiration==expiration ]) == 0) {warning("No results"); return(NA)}
  if(is.null(int.min_delta) & is.null(int.strike) & (is.null(chr.money_in_out) & is.null(int.min_dollar))) { warning("Must have delta/strike or in-out combo") ;return(NA) }
  
  
  if ( !is.null(int.strike) ) {
    dt.result <- data[trade_date == chr.trade_date & expiration==chr.expiration & call_put == chr.option_type & strike == int.strike ] 
    if(nrow(dt.result) > 0) {return(dt.result[1,])} else {warning("No results"); return(NA)}
  }
  
  if ( !is.null(int.min_delta)) {
    dt.result <- data[trade_date == chr.trade_date & expiration==chr.expiration & call_put == chr.option_type & abs(delta) <= int.min_delta ][order(-abs(delta))]
    if(nrow(dt.result) > 0) {return(dt.result[1,])} else {warning("No results"); return(NA)}
  }
  
  int.money_in_out <- ifelse(chr.money_in_out == 'in', 1, -1)
  
  if (int.money_in_out > 0 ) {
    dt.result <- data[trade_date == chr.trade_date & expiration==chr.expiration & call_put == chr.option_type & distance_from_underlying >= int.money_in_out*int.min_dollar ][order(distance_from_underlying)]
  } else {
    dt.result <- data[trade_date == chr.trade_date & expiration==chr.expiration & call_put == chr.option_type & distance_from_underlying <= int.money_in_out*int.min_dollar ][order(-distance_from_underlying)]
  }
  
  if (nrow(dt.result) == 0) { warning("No results Returned"); return(NA) } else { return(dt.result[1,]) }
  
  
}



getRangeAcceptableDelta <- function(data=dt.spy, chr.trade_date, dt.portfolio) {
  
  dt.atm <- getBestOption(data=dt.spy, 
                          chr.trade_date = chr.trade_date,
                          chr.expiration =expirations[expiration>chr.trade_date][order(expiration)][1,]$expiration,
                          chr.option_type = 'c',
                          chr.money_in_out = 'out',
                          int.min_dollar = 0)
  
  int.vol <- dt.atm$iv
  int.underlying <- dt.atm$underlying
  dt.portfolio.slim <- dt.portfolio[trade_date==chr.trade_date]
  dt.portfolio.slim <- dt.portfolio.slim[!trade_id %in% dt.portfolio.slim[qty==0]$trade_id]
  int.current_delta <- dt.portfolio.slim[,sum(qty*delta)*100]
  int.current_gamma <- dt.portfolio.slim[,sum(qty*gamma)*100]
  int.oneSDmove <- int.underlying*(int.vol)*(sqrt(1/365))
  int.range_gamma <- abs(int.oneSDmove * (int.current_gamma)/2 )
  ls.metrics <- list(current_delta = int.current_delta, range_delta=int.range_gamma)
  return(ls.metrics)
  
  
}




optVertical <- function(data=dt.spy, 
                        chr.trade_date, 
                        chr.expiration, 
                        chr.option_type, 
                        chr.long_short, 
                        vec.deltas=NULL, 
                        vec.strikes=NULL, 
                        int.qty = 1 ) {
  
  if (!is.null(vec.deltas)) {
    dt.price_high <- getBestOption(data,chr.trade_date,chr.expiration,chr.option_type, int.min_delta = max(vec.deltas) )
    dt.price_low <- getBestOption(data,chr.trade_date,chr.expiration,chr.option_type, int.min_delta = min(vec.deltas) )
  }
  
  if (!is.null(vec.strikes)) {
    dt.price_high <- getBestOption(data,chr.trade_date,chr.expiration,chr.option_type, int.strike = ifelse(chr.option_type == 'p', max(vec.strikes), min(vec.strikes)) )
    dt.price_low <- getBestOption(data,chr.trade_date,chr.expiration,chr.option_type, int.strike = ifelse(chr.option_type == 'p', min(vec.strikes), max(vec.strikes)) )
  }
  
  dt.price_high[,qty:=ifelse(chr.long_short == 'long', int.qty*1, int.qty*-1)]
  dt.price_low[,qty:=ifelse(chr.long_short == 'long', int.qty*-1, int.qty*1)]
  
  dt.all <- rbind(dt.price_high, dt.price_low)
  return(dt.all)
  
}


optCalendar <- function(data=dt.spy, 
                        chr.long_short, 
                        chr.trade_date, 
                        chr.expiration_front, 
                        chr.expiration_back,  
                        chr.option_type, 
                        int.min_delta = NULL,
                        int.strike = NULL,
                        chr.money_in_out = NULL, 
                        int.min_dollar = NULL, 
                        int.qty = 1) {
  
  dt.slim <- data[expiration %in% c(chr.expiration_front, chr.expiration_back) & call_put== chr.option_type][trade_date==chr.trade_date]
  dt.slim <- dt.slim[strike %in% dt.spy.slim[,.N, by=strike][N==2]$strike]
  dt.front_month <- getBestOption(data= dt.spy.slim, chr.trade_date, chr.expiration=chr.expiration_front, chr.option_type, chr.money_in_out='out', int.min_dollar)
  dt.back_month <- getBestOption(data= dt.spy.slim, chr.trade_date, chr.expiration=chr.expiration_back, chr.option_type, chr.money_in_out='out', int.min_dollar, int.strike = dt.front_month$strike)
  dt.front_month[,qty:=ifelse(chr.long_short=='long', -1*int.qty, int.qty)]
  dt.back_month[,qty:=ifelse(chr.long_short=='long', int.qty, -1*int.qty)]
  dt.pos <- rbind(dt.front_month, dt.back_month)
  return(dt.pos)
  
}




caclulateRealizedVol <- function(data=dt.spy, chr.date_start, chr.date_end, chr.symbol = 'SPY') {
  
  dt.spy.tmp <- data[trade_date %between% c(chr.date_start, chr.date_end)]
  dt.underlying <- dt.spy.tmp[,list(underlying=mean(underlying)), by=list(date=trade_date)]
  dt.underlying[,underlying_yesterday := c(NA, dt.underlying$underlying[-nrow(dt.underlying)])]
  dt.underlying[,returns:=underlying/underlying_yesterday - 1]
  int.stdev <- dt.underlying[,sd(returns, na.rm=T)]
  int.vol <- int.stdev*sqrt(262)
  return(int.vol)
  
}
