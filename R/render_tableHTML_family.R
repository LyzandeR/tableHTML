#' Implementing tableHTML in shiny
#' 
#' This function is used to implement tableHTML in a shiny app. It is used in the shiny
#' ui.R file. Internally, it just calls uiOutput, since tableHTML creates HTML code.
#'
#' @param outputId input name.
#' @param inline use an inline (span()) or block container (div()) for the output.
#' @param container  a function to generate an HTML element to contain the text. 
#' @param ... Other arguments to pass to the container tag function. This is useful for providing 
#'        additional classes for the tag.
#' 
#' @seealso \code{uiOutput} 
#' 
#' @export
tableHTML_output <- shiny::uiOutput

#' Implementing tableHTML in shiny
#' 
#' This function is used to implement tableHTML in a shiny app. This function is used in the shiny
#' server.R file. Internally, it just calls renderUI, since tableHTML creates HTML code.
#' 
#' @param expr A tableHTML object. 
#' @param env An environment.
#' @param quoted  A boolean value. Whether the expression is quoted or not.
#' @param func Deprecated. Please use expr instead.  
#' 
#' @seealso \code{renderUI} 
#' 
#' @export
render_tableHTML <- shiny::renderUI


