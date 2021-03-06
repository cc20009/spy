

suppressWarnings(source('~/rPackageFunctions/toCreate.R'))
suppressWarnings(source('~/rPackageFunctions/toCreateOptionFunctions.R'))







dt.spy <- getCachedObject(chr.location = 'spy_options/', chr.name='spy_2014')

dt.positions <- data.table()

dt.pos1 <- getBestOption(data=dt.spy,chr.trade_date = '2014-01-02', 
              chr.expiration = '2014-01-18', chr.option_type = 'c',int.min_delta = .3)[,qty:=1]
dt.pos1[,trade_id:=1]

dt.pos2 <- getBestOption(data=dt.spy,chr.trade_date = '2014-01-02', 
                         chr.expiration = '2014-10-18', chr.option_type = 'c',int.min_delta = .3)[,qty:=-1]
dt.pos2[,trade_id:=2]







dt.portfolio <- rbind(dt.pos1, dt.pos2)
dt.portfolio <- updateMarks(dt.portfolio=dt.portfolio, chr.trade_date = '2014-01-03')
q <- getBestOption(data = dt.spy, chr.trade_date= '2014-01-03', chr.expiration='2014-10-18',chr.option_type = 'c',int.strike = 195)[,qty:=0][,trade_id:=2]
dt.portfolio <- rbind(dt.portfolio, q)
dt.portfolio <- updateMarks(dt.portfolio=dt.portfolio, chr.trade_date = '2014-01-06')

dt.portfolio <- updateMarks(dt.portfolio=dt.portfolio, chr.trade_date = '2014-01-07')
q <- getBestOption(data = dt.spy, chr.trade_date= '2014-01-07', chr.expiration='2014-01-18',chr.option_type = 'c',int.strike = 186)[,qty:=0][,trade_id:=1]
dt.portfolio <- rbind(dt.portfolio, q)

updateMarks(dt.portfolio=dt.portfolio, chr.trade_date = '2014-01-09')




#optVertical(data = dt.spy,chr.trade_date = '2014-01-06', chr.expiration = '2014-01-18', chr.option_type = 'p',chr.long_short = 'short',vec.deltas = c(.15,.35))[,sum(qty*delta)]





dt.positions <- data.table()
for ( i in unique(dt.spy[trade_date <= '2015-01-16']$trade_date)) {
  print(i)
  chr.exp <- getExpirationToTrade(i, 1)
  dt.pos.day_of <- getBestOption(data= dt.spy, chr.trade_date=i, chr.expiration=chr.exp, chr.option_type='c', chr.money_in_out='in', int.min_dollar=0) 

  
  #dt.pos.day_of[,qty:=-1]
  
  if (nrow(dt.positions)==0) { id <- 1} else ( id <- dt.positions[,max(trade_id)]+1 )
  
  dt.pos.day_of[,trade_id:=seq(id, id+nrow(dt.pos.day_of)-1)]
  
  
  if (nrow(dt.positions) != 0) {
    dt.from_previous <- merge(dt.positions[trade_date==max(dt.positions$trade_date)][,list(trade_date=i,expiration,strike,call_put,qty, trade_id)], dt.spy, by=c('trade_date','expiration','strike','call_put'))
    dt.total <- rbind(dt.pos.day_of, dt.from_previous)
  } else {
    dt.total <- copy(dt.pos.day_of)
  }
  dt.positions <- rbind(dt.positions, dt.total)
  
  
  
}





