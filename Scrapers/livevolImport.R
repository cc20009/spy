

# Call the required packages
library(RMySQL)
library(data.table)

run <- function()
{
  
  con <- dbConnect(MySQL(), user="carlton", password= '1234password',db='import_livevol' )
  dates_current <- data.table(dbGetQuery(con, "select distinct quote_date from import_livevol.spy"))
  dates_current[,quotes:=as.character(paste0(substring(quote_date,1,4), substring(quote_date,6,7), substring(quote_date, 9,10)))]
  
  temp = list.files(path = '/home/ubuntu/optionsdata/python_import/outdir/', pattern="*.csv")
  
  setdiff(substring(temp, 23,30), dates_current$quotes)
  
  list_of_files <- paste0('SPY-options-eod-calcs-', setdiff(substring(temp, 23,30), dates_current$quotes), '.csv')
  
  dt.insert <- data.table()
  for (i in list_of_files) {
    print(i)
    t <- data.table(read.csv(paste0('/home/ubuntu/optionsdata/python_import/outdir/', i)))
    dt.insert <- rbind(dt.insert,t)
  }
  dbWriteTable(con, value = data.frame(dt.insert), name = "spy", append = TRUE, row.names=F ) 
  system(command = paste("logger -p local0.info would insert now3"))
  
}



