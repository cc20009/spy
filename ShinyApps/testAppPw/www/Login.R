

#### Log in module ###
USER <- reactiveValues(Logged = Logged)

passwdInput <- function(inputId, label) {
  tagList(
    tags$label(label),
    br(),
    tags$input(id = inputId, type="password", value="")
  )
}

output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {

    sidebarPanel(
      br(),
      textInput("userName", "User Name:"),
      passwdInput("passwd", "Password:"),
      hr(),
      actionButton("Login", "Log in"),
      width = 2
    )
    
    
  }
})

output$pass <- renderText({  
  if (USER$Logged == FALSE) {
    if (!is.null(input$Login)) {
      if (input$Login > 0) {
        Username <- isolate(input$userName)
        Password <- isolate(input$passwd)
        Id.username <- which(PASSWORD$Brukernavn == Username)
        Id.password <- which(PASSWORD$Passord    == Password)
        if (length(Id.username) > 0 & length(Id.password) > 0) {
          if (Id.username == Id.password) {
            USER$Logged <- TRUE
          } 
        } else  {
          "User name or password failed!"
        }
      } 
    }
  }
})



