

args<-commandArgs(TRUE)
test <- args[1]
system(command = paste("logger -p local0.info \"", 'hey meg', "\""))


x <- system(command=' tail /var/log/syslog --lines 100', intern=T)
x2 <- data.table(x[grep('Executing',x)])
x2[,str:=data.table(str_locate(pattern ='Executing ',V1))$end]
x2[,script:=substring(V1, str+1)]
x2[,str_time:=data.table(str_locate(pattern =' ip-',V1))$start]
x2[,time:=substring(V1, 1,str_time-1)]
#system(command=' tail /var/log/syslog --lines 100', intern=T)

#system(command=' tail /var/mail/root --lines 100', intern=T)


con <- dbConnect(MySQL(), user="carlton", password= '1234password',db='import_tradeking' )
dbSendQuery(con, "DELETE FROM import_yahoo.spy where hour(timestamp) < 14")
dbSendQuery(con, "DELETE FROM import_tradeking.spy where hour(timestamp) < 14")
