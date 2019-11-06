#' Generate hexagon logo from a tableHTML object.
#'
#' The purpose of the function \code{create_hexlogo} is to generate the hexagon logo 
#' of the \code{tableHTML} package.
#' 
#' The function \code{create_hexlogo} will generate the hexagon logo 
#' of the \code{tableHTML} package.
#'
#' @param save A boolean when set to TRUE the logo will be saved in the specified format.
#'
#' @param format A character string to specify the format of the output,
#' it accepts \code{'html'}, \code{'png'}, or \code{'jpeg'}.
#' Default is \code{'html'}.
#'
#' @param file A character string to specify the name and path to the new file.
#' Should end with \code{'.html'}, \code{'.png'}, or \code{'.jpeg'}, 
#' depending on the selected format.
#'
#' @param ... Further parameters to pass to webshot.
#'
#' @inheritParams write_tableHTML
#' 
#' @return The hexagon logo of the tableHTML package as a tableHTML object.
#' 
#' The output will be shown in the Viewer pane, and can be saved
#' either as an image or as an HTML.
#'
#' @examples
#' \dontrun{
#' create_hexlogo(save = FALSE)
#' 
#' create_hexlogo(format = 'jpeg',
#'             file = '~/exported_hexlogo.jpeg')
#' }
#' @export
create_hexlogo <- function(save = TRUE, format = 'html',
                           file = 'tableHTML_hexlogo.html',
                           complete_html = FALSE, ...){
  # checks
  if(save){
    if(! format %in% c('html', 'png', 'jpeg')){
      stop("format should be 'html', 'png', or 'jpeg'")
    }
    if(!endsWith(file, format)){
      stop("file extension should be the same as the the format")
    }
  }
  
  # create inner logo
  logo <- create_logo(save = FALSE)
  # check the style of the logo
  if (! grepl('class="hexagon inner"', logo))
    stop('logo does not have the compatible style. 
         The style of the logo should be \"hexagon inner\".
         Check the create_logo() function. ')
  
  # colors
  border_color <- 'lightgray'
  inner_fill <- '#F2F2F2'
  
  # Define the hexagon style (this is defined separately to extract additional attributes for testing)
  css_style <- list(list('.hexagon',
                         c('position', 'width', 'height', 'background-color', 'margin'),
                         c('relative', '800px', '500px', border_color, '300px 0')),
                    list(c('.hexagon:before', '.hexagon:after'),
                         c('content', 'position', 'width', 'border-left', 'border-right'),
                         c('\"\"', 'absolute', '0', '400px solid transparent', '400px solid transparent')),
                    list('.hexagon:before',
                         c('bottom', 'border-bottom'),
                         c('100%', paste0('200px solid ', border_color))),
                    list('.hexagon:after',
                         c('top', 'width', 'border-top'),
                         c('100%', '0', paste0('200px solid ', border_color))),
                    list('.hexagon.inner',
                         c('background-color', '-webkit-transform', '-moz-transform', 'transform', 'z-index'),
                         c(inner_fill, 'scale(.9, .9)', 'scale(.9, .9)', 'scale(.9, .9)', '1')),
                    list('.hexagon.inner:before',
                         c('bottom', 'border-bottom'),
                         c('100.3%', paste0('200px solid ', inner_fill))),
                    list('.hexagon.inner:after',
                         c('border-top'),
                         c(paste0('200px solid ', inner_fill))))
  
  # make the css 
  my_css <- make_css(css_style[[1]], css_style[[2]], css_style[[3]], 
                     css_style[[4]], css_style[[5]], css_style[[6]], css_style[[7]])
  
  attributes <- attributes(logo)
  hexlogo <- paste0("<style type=\"text/css\">\n",
                    my_css, "\n</style>\n",
                    "<div id=\"centering\" style=\"width:800px; margin:0 auto;\">\n",
                    "<div id=\"outerhex\" class=\"hexagon\">\n",
                    logo, 
                    "\n</div>\n</div>") %>%
    htmltools::HTML()
  
  class(hexlogo) <- c("tableHTML", class(hexlogo))
  attributes(hexlogo) <- attributes
  # add more attributes for testing
  attr(hexlogo, 'css_style') <- list('selectors_cnt' = length(css_style), 
                                     'definitions_cnt' = sapply(css_style, function(x) x[2]) %>% lengths())
  
  # save the logo
  if (save){
    if (format != 'html'){
      tableHTML_to_image(hexlogo,
                         file = file,
                         type = format,
                         selector = NULL,
                         zoom = 5,
                         ...)
    }else{
      write_tableHTML(hexlogo,
                      file = file,
                      complete_html = complete_html)
    }
  }
  hexlogo
}
