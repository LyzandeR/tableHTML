#' Generate a hexlogo package's logo
#'
#' \code{to_hex_logo} will generate a hexagon logo for the \code{tableHTML} package.
#'
#' \code{to_hex_logo} will generate a hexagon logo for the \code{tableHTML} package.
#'
#' @param logo An object of class tableHTML. Preferably the output of the function \code{tableHTML_logo()}.
#'
#' @param save A boolean when set to TRUE the logo will be saved in the specified format.
#'
#' @param format A character string to specify the format of the output, it accepts \code{'html'}, \code{'png'}, \code{'pdf'}, or \code{'jpeg'}. Default is \code{'html'}.
#'
#' @param file A character string to specify the name and path to the new file.
#'
#' @param ... Further parameters to pass to webshot.
#'
#' @return The hexagon logo of the tableHTML package in the Viewer pane, and saves it either as an image or as an HTML.
#'
#' @examples
#'
#' logo <- tableHTML_logo(save = FALSE)
#' to_hex_logo(logo,
#'             format = 'pdf',
#'             file = '~/exported_logo')
#' @export
to_hex_logo <- function(logo, save = TRUE,
                        format = 'html', file = 'hexlogo_export', ...){

  if (!inherits(logo, 'tableHTML')) stop('logo needs to be a tableHTML object')

  if(save){
    if(! format %in% c('html', 'png', 'pdf', 'jpeg')){
      stop("format should be 'html', 'png', 'pdf', or 'jpeg'")
    }}
  # colors
  border_color <- 'lightgray'
  inner_fill <- '#F2F2F2'

  # make the css style
  my_css <- make_css(list('.hexagon',
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

  attributes <- attributes(logo)
  hexlogo <- paste0("<style type=\"text/css\">",
                    my_css, "</style>\n<div id=\"outerhex\" class=\"hexagon\">",
                    logo, "</div>") %>%
    htmltools::HTML()

  class(hexlogo) <- c("tableHTML", class(hexlogo))
  attributes(hexlogo) <- attributes

  # save the logo
  if (save){
    if (format != 'html'){
     tableHTML_to_image(hexlogo,
                       file = paste0(file, '.', format),
                       selector = NULL,  ...)
    }else{
      html_file <- paste0(file, '.html')
      htmltools::save_html(html = hexlogo, html_file)
    }
  }
  hexlogo
}
