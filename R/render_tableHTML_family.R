#' Implementing tableHTML in shiny
#' 
#' This function is used to implement tableHTML in a shiny app. It is used in the shiny
#' ui.R file. Internally, it just calls uiOutput, since tableHTML creates HTML code.
#'
#' @param outputId input name.
#' @param ... Other arguments to passed along to [shiny::uiOutput()]
#'
#' @seealso [uiOutput()]
#' @md
#'
#' @examples
#' \dontrun{
#' 
#' library(shiny)
#' shinyApp(
#'  ui = fluidPage(
#'   fluidRow(
#'   #leave some spacing
#'   br(),
#'   column(width = 1),
#'   tableHTML_output("mytable"))
#'  ), 
#' server = function(input, output) {
#'  output$mytable <- render_tableHTML( 
#'   tableHTML(mtcars)
#'  )}
#' )
#' 
#' }
#' 
#' @export
tableHTML_output <- function(outputId, ...) {
  shiny::uiOutput(outputId, ...)
}

#' Implementing tableHTML in shiny
#' 
#' This function is used to implement tableHTML in a shiny app. This function is used in the shiny
#' server.R file. Internally, it just calls renderUI, since tableHTML creates HTML code.
#'
#' @param expr A tableHTML object.
#' @param ... Other arguments passed along to [shiny::renderUI()].
#'
#' @seealso [shiny::renderUI()]
#' @md
#'
#' @examples
#' \dontrun{
#' 
#' library(shiny)
#' shinyApp(
#'  ui = fluidPage(
#'   fluidRow(
#'   #leave some spacing
#'   br(),
#'   column(width = 1),
#'   tableHTML_output("mytable"))
#'  ), 
#' server = function(input, output) {
#'  output$mytable <- render_tableHTML( 
#'   tableHTML(mtcars)
#'  )}
#' )
#' 
#' }
#' 
#' @export
render_tableHTML <- function(expr, ...) {
 shiny::renderUI(expr, ...)
}


globalVariables('func')

