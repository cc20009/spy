

library(shiny)

shinyUI(navbarPage(
  title = 'NBA',
  tabPanel('Summary of Payments',     dataTableOutput('ex1')),
  tabPanel('NBA Results',        dataTableOutput('ex2'))
))



