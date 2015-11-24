



library(shiny)
library(datasets)
library(data.table)
library(RMySQL)

con <- dbConnect(dbDriver("MySQL"), user = "carlton", password = "1234password")

Logged = FALSE;
PASSWORD <- data.frame(Brukernavn = "yellowjacket", Passord = "loveless")
# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  source("www/Login.R",  local = TRUE)
  
  
  observe({
    if (USER$Logged == TRUE) {

      dt.spy <- data.table(dbGetQuery(con, paste0("select * from import_livevol.spy where underlying_symbol = 'SPY' and quote_date in ('",input$date,"')")))
      
      output$mytable1 = renderDataTable({
        dt.spy
      }, options = list(paging = FALSE))
      
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste('data-', input$date, '.csv', sep='')
        },
        content = function(con) {
          write.csv(dt.spy[quote_date==input$date], con,row.names = F)
        }
      )
      
      

      

    }
  })
})



