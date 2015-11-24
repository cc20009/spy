
args<-commandArgs(TRUE)
chr.script <- args[1]

# Call the required packages
library(RMySQL)
library(data.table)

con <- dbConnect(MySQL(), user="carlton", password= '1234password' )
chr.location <- dbGetQuery(con, paste0("Select * from zn.automated_process where name = '",chr.script,"'" ))


source(paste0(chr.location$location, chr.script))

run()




