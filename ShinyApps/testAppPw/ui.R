



shinyUI(bootstrapPage(
  # Add custom CSS & Javascript;
  tagList(
    tags$head(
      tags$link(rel="stylesheet", type="text/css",href="style.css"),
      tags$script(type="text/javascript", src = "md5.js"),
      tags$script(type="text/javascript", src = "passwdInputBinding.js")
    )
  ),
  
  ## Login module;
  div(class = "login",
      uiOutput("uiLogin"),
      textOutput("pass")
  ), 
  
  
  div(dateInput("date", "", value = Sys.Date())),
  div(downloadLink('downloadData', 'Download')),
  div(dataTableOutput("mytable1"))
  
  
))




