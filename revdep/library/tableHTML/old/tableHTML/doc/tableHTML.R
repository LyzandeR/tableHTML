## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----basic---------------------------------------------------------------
library(tableHTML)
tableHTML(mtcars)

## ----rownames------------------------------------------------------------
tableHTML(mtcars, rownames = FALSE)

## ---- eval = FALSE-------------------------------------------------------
#  mytable <- tableHTML(mtcars)
#  str(mytable)
#  # Classes 'tableHTML', 'html', 'character'  atomic [1:1]
#  # <table class=table_1901 border=1 style="border-collapse: collapse;">
#  # <tr>
#  #   <th id="tableHTML_header_1"> </th>
#  #   <th id="tableHTML_header_2">mpg</th>
#  #   <th id="tableHTML_header_3">cyl</th>
#  # truncated...

## ---- eval = FALSE-------------------------------------------------------
#  mytable <- tableHTML(mtcars, class = 'myClass')
#  str(mytable)
#  # Classes 'tableHTML', 'html', 'character'  atomic [1:1]
#  # <table class=myClass border=1 style="border-collapse: collapse;">
#  # <tr>
#  #   <th id="tableHTML_header_1"> </th>
#  #   <th id="tableHTML_header_2">mpg</th>
#  #   <th id="tableHTML_header_3">cyl</th>
#  # truncated...

## ----second header-------------------------------------------------------
tableHTML(mtcars, second_headers = list(c(3, 4, 5), c('col1', 'col2', 'col3')))

## ----row groups----------------------------------------------------------
tableHTML(mtcars, 
          rownames = FALSE, 
          row_groups = list(c(10, 10, 12), c('Group 1', 'Group 2', 'Group 3')))

## ----widths--------------------------------------------------------------
tableHTML(mtcars, 
          widths = rep(100, 12), 
          second_headers = list(c(3, 4, 5), c('col1', 'col2', 'col3')))

## ----border--------------------------------------------------------------
tableHTML(mtcars, border = 0)

## ----caption-------------------------------------------------------------
tableHTML(mtcars, caption = 'This is  a table')

## ----footer--------------------------------------------------------------
tableHTML(mtcars, footer = 'This is  a footer')

## ----collapse, eval = FALSE----------------------------------------------
#  tableHTML(mtcars, collapse = 'separate')

## ----collapse 2----------------------------------------------------------
tableHTML(mtcars, collapse = 'separate_shiny')

## ----spacing 1-----------------------------------------------------------
tableHTML(mtcars, collapse = 'separate_shiny', spacing = '2px')

## ----spacing 2-----------------------------------------------------------
tableHTML(mtcars, collapse = 'separate_shiny', spacing = '5px 2px')

## ----escaping------------------------------------------------------------
df <- data.frame(a = c('abcd<efgh'))
tableHTML(df, escape = FALSE, rownames = FALSE)
tableHTML(df, escape = TRUE, rownames = FALSE)

## ----theme-scientific----------------------------------------------------
tableHTML(mtcars, widths = c(140, rep(50, 11)), theme = 'scientific')

## ----theme-rshiny-blue---------------------------------------------------
tableHTML(mtcars, widths = c(140, rep(50, 11)), theme = 'rshiny-blue')

## ----add css row 1-------------------------------------------------------
mtcars %>%
  tableHTML(widths = c(140, rep(45, 11))) %>% 
  add_css_row(css = list(c('background-color', 'border'), c('lightblue', '2px solid lightgray')))

## ----add css row 2-------------------------------------------------------
mtcars %>%
  tableHTML(widths = c(140, rep(45, 11))) %>% 
  add_css_row(css = list(c('background-color', 'border'), c('lightblue', '2px solid lightgray')),
              rows = 2:33)

## ----add css row 3-------------------------------------------------------
mtcars %>%
  tableHTML(widths = c(140, rep(45, 11))) %>% 
  add_css_row(css = list('background-color', '#f2f2f2'),
              rows = odd(1:33)) %>%
  add_css_row(css = list('background-color', '#e6f0ff'),
              rows = even(1:33))

## ----add css column 1----------------------------------------------------
mtcars %>%
  tableHTML(widths = c(140, rep(45, 11))) %>% 
  add_css_column(css = list(c('background-color', 'border'), c('lightblue', '3px solid lightgray')), 
                 columns = c('cyl', 'hp', 'rownames'))

## ----add css column 2----------------------------------------------------
mtcars %>%
  tableHTML(widths = c(140, rep(45, 11))) %>% 
  add_css_row(css = list('background-color', '#f2f2f2')) %>%
  add_css_column(css = list('background-color', 'lightblue'), 
                 columns = c('cyl', 'hp', 'rownames')) 
 

## ---- eval = FALSE-------------------------------------------------------
#  mytable <- tableHTML(mtcars)
#  print(mytable, viewer = FALSE)
#  <table style="border-collapse:collapse;" class=table_2079 border=1>
#  <thead>
#  <tr>
#    <th id="tableHTML_header_1"> </th>
#    <th id="tableHTML_header_2">mpg</th>
#    <th id="tableHTML_header_3">cyl</th>
#  truncated...

## ----add css header 1----------------------------------------------------
mtcars %>%
  tableHTML(widths = c(140, rep(45, 11))) %>% 
  add_css_header(css = list('background-color', 'lightgray'), headers = c(1, 4))

## ----add css second header 1---------------------------------------------
mtcars %>%
  tableHTML(widths = c(140, rep(45, 11)), 
            second_headers = list(c(3, 4, 5), c('col1', 'col2', 'col3'))) %>% 
  add_css_second_header(css = list(c('background-color', 'border'), 
                                   c('lightgray', '3px solid green')),
                        second_headers = c(1, 3))  

## ----add css caption-----------------------------------------------------
mtcars %>%
  tableHTML(widths = c(140, rep(45, 11)), 
            caption = 'This is a table') %>% 
  add_css_caption(css = list(c('color', 'font-size', 'text-align'), c('blue', '20px', 'left')))  

## ----add css footer------------------------------------------------------
mtcars %>%
  tableHTML(widths = c(140, rep(45, 11)), 
            footer = 'This is a footer') %>% 
  add_css_footer(css = list(c('color', 'font-size', 'text-align'), c('blue', '20px', 'left')))  

## ----add css thead-------------------------------------------------------
mtcars %>%
  tableHTML() %>% 
  add_css_thead(css = list('background-color', 'lightgray')) 

## ----add css tbody-------------------------------------------------------
mtcars %>%
  tableHTML() %>% 
  add_css_tbody(css = list('background-color', 'lightgray')) 

## ----thead example 1-----------------------------------------------------
mtcars %>%
  tableHTML() %>% 
  add_css_thead(css = list('background-color', 'lightgray')) %>%
  add_css_row(css = list('background-color', 'blue'), rows = 1)

## ----thead example 2-----------------------------------------------------
mtcars %>%
  tableHTML() %>% 
  add_css_tbody(css = list('background-color', 'lightgray')) %>%
  add_css_row(css = list('background-color', 'blue'), rows = c(4, 6))

## ----add css table-------------------------------------------------------
mtcars %>%
  tableHTML() %>% 
  add_css_table(css = list('background-color', 'lightgray')) 

## ----all together--------------------------------------------------------
mtcars %>%
  tableHTML(widths = c(140, rep(45, 11)),
            second_headers = list(c(3, 4, 5), c('team1', 'team2', 'team3')),
            caption = 'Table of Cars',
            footer = 'Figure 1. Stats for famous cars') %>% 
  add_css_second_header(css = list(c('height', 'background-color', 'font-size'), 
                                   c('40px', ' #e6e6e6', '30px')),
                        second_headers = 1:3) %>%
  add_css_header(css = list(c('height', 'background-color'), c('30px', ' #e6e6e6')),
                 headers = 1:12) %>%
  add_css_row(css = list('background-color', '#f2f2f2'),
              rows = even(1:34)) %>%
  add_css_row(css = list('background-color', '#e6f0ff'),
              rows = odd(1:34)) %>%
  add_css_column(css = list('text-align', 'center'), 
                 columns = names(mtcars)) %>%
  add_css_caption(css = list(c('text-align', 'font-size', 'color'), c('center', '20px', 'black'))) %>%
  add_css_footer(css = list(c('text-align', 'color'), c('left', 'black')))

## ----replace html--------------------------------------------------------
mtcars %>%
 tableHTML(widths = c(140, rep(45, 11))) %>%
 replace_html(' <td id="tableHTML_column_1">21</td>', 
              '<td id="mpg" style="background-color:lightyellow">21</td>')

## ----shiny 1, eval = FALSE-----------------------------------------------
#  library(shiny)
#  shinyApp(
#   ui = fluidPage(
#     fluidRow(
#      #leave some spacing
#      br(),
#      column(width = 1),
#      tableHTML_output("mytable"))
#   ),
#   server = function(input, output) {
#    output$mytable <- render_tableHTML(
#     tableHTML(mtcars)
#   )}
#  )

## ----shiny 2, eval = FALSE-----------------------------------------------
#  shinyApp(
#   ui = fluidPage(
#     fluidRow(
#      #leave some spacing
#      br(),
#      column(width = 1),
#      tableHTML_output("mytable"))
#   ),
#   server = function(input, output) {
#    output$mytable <- render_tableHTML(
#     mtcars %>%
#       tableHTML(widths = c(140, rep(45, 11)),
#                 second_headers = list(c(3, 4, 5), c('team1', 'team2', 'team3'))) %>%
#       add_css_second_header(css = list(c('height', 'background-color', 'font-size', 'text-align'),
#                                        c('40px', ' #e6e6e6', '30px', 'center')),
#                             second_headers = 1:3) %>%
#       add_css_header(css = list(c('height', 'background-color', 'text-align'),
#                                 c('30px', ' #e6e6e6', 'center')),
#                      headers = 1:12) %>%
#       add_css_row(css = list('background-color', '#f2f2f2'),
#                   rows = even(1:34)) %>%
#       add_css_row(css = list('background-color', '#e6f0ff'),
#                   rows = odd(1:34)) %>%
#       add_css_column(css = list('text-align', 'center'),
#                      columns = names(mtcars))
#   )}
#  )

## ----shiny css, eval = FALSE---------------------------------------------
#  #ui.R
#  shinyUI(
#   fluidPage(
#    fluidRow(
#     #leave some spacing
#     br(),
#     column(width = 1),
#     #include css file in shiny
#     includeCSS('www/mycss.css'),
#     tableHTML_output("mytable"))
#   )
#  )
#  
#  #server.R
#  shinyServer(
#  function(input, output) {
#     output$mytable <- render_tableHTML(
#      tableHTML(mtcars, second_headers = list(c(3, 4, 5), c('col1', 'col2', 'col3')))
#     )}
#  )

