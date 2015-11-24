library(data.table)




expirations <- getExpirations(con)

for (i in 2005:2015) {
  
  dt.spy <- data.table(dbGetQuery(conn = con,statement = paste0("select * from import_livevol.spy 
                                                                where vwap > 0 and year(expiration) = ",i," and expiration in (", 
                                                                paste("'",as.character(expirations$expiration),"'",
                                                                      collapse=", ",sep=""), ") "  )))
  
  
  dt.spy <- organizeData(dt.spy)
  cacheObject(mxt.data = dt.spy, chr.location= 'spy_options/', chr.name = paste0('spy_',i))
  
}




dt.spy <- organizeData(dt.spy)








getBestOption(data=dt.spy,chr.trade_date = '2014-01-02',chr.expiration =  '2015-01-17',chr.option_type =  'p', int.min_delta = .3)



optVertical(data = under2, chr.trade_date = '2014-01-02', chr.expiration = '2015-01-17', chr.option_type = 'c',chr.long_short = 'short',vec.deltas = c(.15,.3),int.qty=1)
    


getBestOption(data=under2,chr.trade_date = '2014-01-02',chr.expiration =  '2014-01-18',chr.option_type =  'c', int.min_delta = .15 )





tradeking <- data.table(dbGetQuery(con,"select * from tradeking.spy"))
tradeking2 <- tradeking[expiration=='2015-11-20'][,list(quote_date,expiration,strike,option_type,underlying_tradeking=underlying_ask,bid_tradeking=bid,ask_tradeking=ask, delta_tradeking=delta)]


livevol <- data.table(dbGetQuery(con, "select * from import_livevol.spy where quote_date >= 20151105"))
livevol2 <- livevol[expiration=='2015-11-20'][,list(quote_date, expiration, strike, option_type, underlying_livevol=underlying_ask_1545, bid_livevol=bid_1545, ask_livevol=ask_1545,delta_livevol=delta_1545)]




alls <- merge(tradeking2, livevol2, by=c('quote_date','expiration','strike','option_type'))

ggplot(alls, aes(x=bid_tradeking, y=bid_livevol)) + geom_point() + geom_abline(slope=1)






