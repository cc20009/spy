
con <- dbConnect(MySQL(), user="carlton", password= '1234password' )
yahoo <- data.table(dbGetQuery(conn = con, "select * from import_yahoo.spy"))

tradeking <- data.table(dbGetQuery(conn = con, "select * from import_tradeking.spy"))
livevol <- data.table(dbGetQuery(conn = con, "select * from import_livevol.spy where underlying_symbol='SPY' and  quote_date >= '2015-10-19'"))



yahoo[hour(timestamp) > 14][,mean(underlying), by=substring(timestamp,1,10)]


tradeking[hour(time_added) > 14][,mean(underlying), by=substring(time_added,1,10)]



