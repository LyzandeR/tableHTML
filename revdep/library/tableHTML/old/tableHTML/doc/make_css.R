## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----basic, echo = TRUE--------------------------------------------------
library(tableHTML)
mycss <- make_css(list('table', c('text-align', 'font-size'), c('center', '20px')),
                  list('th', c('background-color', 'height'), c('lightgreen', '30px')))
print(mycss)


## ----example 1-----------------------------------------------------------
#another example
mycss <- make_css(list(c('table', 'td'), c('text-align', 'font-size'), c('center', '20px')),
                  list('th', c('background-color', 'height'), c('lightgreen', '30px')))
print(mycss)

## ----example 2-----------------------------------------------------------
#adding a special selector
mycss <- make_css(list('table td', c('text-align', 'font-size'), c('center', '20px')))
print(mycss)

## ----example 3-----------------------------------------------------------
mycss <- make_css(list('table td:hover', 'background-color', 'lightyellow'))
print(mycss)

## ----example 4-----------------------------------------------------------
mycss <- make_css(list(c('.myclass', '.myclass2', '#myid', '.myclass[type="text"]'), 
                       c('padding', 'margin-bottom', 'background-color', 'box-shadow'),
                       c('15px', '15px', 'rgba(0,0,0,0.5)', '0 1px 2px #ccc, inset 0 1px 0 #fff')))
print(mycss)

## ----saving to file, eval = FALSE----------------------------------------
#  make_css(list(c('table', 'td'), c('text-align', 'font-size'), c('center', '20px')),
#           list('th', c('background-color', 'height'), c('lightgreen', '30px')),
#           file = 'mycss.css')

## ----shiny css, eval = FALSE---------------------------------------------
#  library(shiny)
#  #ui.R
#  shinyApp(
#   ui = fluidPage(
#     fluidRow(
#      #leave some spacing
#      br(),
#      tags$style(make_css(list('tr:hover', 'background-color', 'lightyellow'))),
#      column(width = 1),
#      uiOutput("mytable"))
#   ),
#   server = function(input, output) {
#    output$mytable <- renderUI(
#     tableHTML(mtcars, second_headers = list(c(3, 4, 5), c('col1', 'col2', 'col3')))
#   )}
#  )

## ----shiny css 2, eval = FALSE-------------------------------------------
#  #when working on shiny the css file is best placed in the www/ folder of the shiny app.
#  #This will then be read by includeCSS in the normal way.
#  #check the above link for more info.
#  make_css(list('tr:hover', 'background-color', 'lightyellow'), file = 'www/mycss.css')

## ----shiny app with includeCSS, eval = FALSE-----------------------------
#  #ui.R
#  shinyUI(
#   fluidPage(
#    fluidRow(
#     #leave some spacing
#     br(),
#     column(width = 1),
#     #include css file in shiny
#     includeCSS('www/mycss.css'),
#     uiOutput("mytable"))
#   )
#  )
#  
#  #server.R
#  shinyServer(
#  function(input, output) {
#     output$mytable <- renderUI(
#      tableHTML(mtcars, second_headers = list(c(3, 4, 5), c('col1', 'col2', 'col3')))
#     )}
#  )

