#' Create a css file or string
#'
#' \code{make_css} will create a css file or string which can also be used in shiny
#' 
#' \code{make_css} will create a css file which can also be used in shiny. If the argument file
#'   is provided the css code will be printed out to the file. The file can then be used in shiny
#'   with the \code{includeCSS} function. Alternatively there are two ways to use \code{make_css}
#'   in order to add css to your shiny app. If you have a very small css file or you are just 
#'   testing your app you can use \code{tags$style} with \code{make_css} directly. There is an
#'   example in the examples section. Another way (which will make your code cleaner) is to create
#'   your css in global.R assign it to a variable and then use that variable with \code{tags$style}.
#'   There is another example on the examples section. Keep in mind that for complete shiny apps
#'   it is best practice to use a file and load it with \code{includeCSS}. This will be faster as
#'   well as it won't run the code to create the css file each time.
#'
#' @param ... css style definitions. Each object you provide must be a list of three elements. 
#'   The first element will be a vector of the selectors to be styled (e.g. table, th, an id or html 
#'   class). If the first element is a vector of length greater than one then the selectors will be
#'   comma separated in the css. The second element will be a vector of the css definitions and the 
#'   third element will a vector of the values of those definitions.
#'
#' @return A css file. 
#'         
#' @examples
#' make_css(list('table', c('text-align', 'font-size'), c('center', '20px')),
#'          list('th', c('background-color', 'height'), c('lightgreen', '30px')))
#'          
#' make_css(list(c('table', 'td'), c('text-align', 'font-size'), c('center', '20px')),
#'          list('th', c('background-color', 'height'), c('lightgreen', '30px')))         
#' 
#' @export
make_css <- function(...) {
 
 css_defs <- list(...)
 
 #create the css string
 all_css <- 
   vapply(css_defs, function(x) {
    
     #make the styles
     css_comp <- paste0(x[[2]], ':', x[[3]], ';')
     style <- paste(css_comp, collapse = '\n  ')
     
     #comma separate selectors if > 1
     to_be_styled <- paste(x[[1]], collapse = ',\n')
     
     #create a css string for the above selectors
     paste0(to_be_styled, 
            ' {\n  ',
            style,
            '\n}\n')
    
   }, FUN.VALUE = character(1))
 
 #return a big string with all the selectors and style definitions 
 htmltools::HTML(paste(all_css, collapse = '\n'))
 
}




