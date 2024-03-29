% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/render_tableHTML_family.R
\name{tableHTML_output}
\alias{tableHTML_output}
\title{Implementing tableHTML in shiny}
\usage{
tableHTML_output(outputId, ...)
}
\arguments{
\item{outputId}{input name.}

\item{...}{Other arguments to passed along to \code{\link[shiny:htmlOutput]{shiny::uiOutput()}}}
}
\description{
This function is used to implement tableHTML in a shiny app. It is used in the shiny
ui.R file. Internally, it just calls uiOutput, since tableHTML creates HTML code.
}
\examples{
\dontrun{

library(shiny)
shinyApp(
 ui = fluidPage(
  fluidRow(
  #leave some spacing
  br(),
  column(width = 1),
  tableHTML_output("mytable"))
 ), 
server = function(input, output) {
 output$mytable <- render_tableHTML( 
  tableHTML(mtcars)
 )}
)

}

}
\seealso{
\code{\link[=uiOutput]{uiOutput()}}
}
