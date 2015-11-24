


library(RMySQL)
library(httr)


df <- data.frame(date= Sys.Date(), value=305.22 )

con <- dbConnect(dbDriver("MySQL"), user = "carlton", password = "1234password", dbname = "test_db")
dbWriteTable(con, value = df, name = "spy", append = TRUE, row.names=F , field.types=list(date="date", value="decimal(5,2)")) 

#As I've become more interested in data science, I've started expanding my the importance of automated processes and monitoring of those processes has become 
#more important to me.
#A good example of this is stock data. If you pull up a webpage on Yahoo Fiannce (link) you're getting a split second snapshot of the current state of the market.  
#That data isn't 
#stored anywhere (at least publically) so if you miss scraping that information, it is gone -- unless you're willing to pay a third party a pretty penny. 
#This provides a unique challenge especially because I can't monitor my scraping on a daily basis.

#Enter R, crontab, Slack and logging, 4 tools that provide a pretty comprehensive set of tools for automated processes and monitoring of those processes.



#The Data
#My best hypothetical example, and what I'm most interested in is 
