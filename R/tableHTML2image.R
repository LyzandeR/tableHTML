#' Convert a tableHTML into an image
#'
#' \code{tableHTML_to_image} converts the tableHTML into an image.
#'
#' The main rational behind this function is to make it work well with pdfs / word documents.
#' When using rmarkdown and want to incude a tableHTML in a pdf / word document this is the
#' function you would need to use. Obviously, you don't need this function if you are exporting
#' to an html file.
#'
#' Specifying a type will determine which function is used to create the image. Either
#' JPEG or PNG. When using JPEG as the type you will need to add a background
#' colour to the table otherwise it will be set to black by JPEG. Both of the
#' built-in themes (rshiny-blue, scientific) work well with JPEG.
#'
#' When working on rmarkdown and you want to knit as pdf, use this function.
#' Works with microsoft word as well.
#'
#' To use this function you need phantomjs installed. Please use \code{webshot::install_phantomjs()}
#' to install if it is not installed already.
#'
#' @param tableHTML A tableHTML object created by the tableHTML function.
#'
#' @param file A file to write the image to. If NULL then file is just
#' displayed on screen.
#'
#' @param type Either png or jpeg. The type of the image.
#'
#' @param add Logical. If TRUE, the plot will be added to the existing plot.
#' If FALSE, the current device will be shut down.
#'
#' @param ... Parameters passed on to webshot. Check \code{\link[webshot]{webshot}}.
#'
#' @inheritParams webshot::webshot
#'
#' @return An image of the tableHTML.
#'
#' @examples
#' \dontrun{
#' mtcars %>%
#'   tableHTML() %>%
#'   tableHTML_to_image()
#' }
#'
#' @importFrom graphics par plot.new
#'
#' @export
tableHTML_to_image <- function(tableHTML,
                               file = NULL,
                               type = c('png', 'jpeg'),
                               add = FALSE,
                               selector = 'table',
                               ...) {

 #check type
 type <- match.arg(type)

 #check add argument
 if (!is.logical(add)) {
  stop("add must be TRUE or FALSE")
 }

 #check if phantom_js is installed
 jsinstalled <- suppressMessages(find_phantom())
 if (is.null(jsinstalled)) {
   stop('To use this function, phantomjs needs to be installed.
        You can install it with webshot::install_phantomjs')
 }

 #save tableHTML to html file
 temp_file <- tempfile(pattern = 'tableHTML',
                       fileext = '.html')

 #temp file to save the image
 image <- tempfile(fileext = paste0('.', type))

 #writing tableHTML HTML into the temp file
 if (type == 'jpeg') {
  write_tableHTML(tableHTML %>%
                   add_css_table(css = list('background-color', 'white')),
                  file = temp_file,
                  complete_html = TRUE)
 } else if (type == 'png') {
  write_tableHTML(tableHTML,
                  file = temp_file,
                  complete_html = TRUE)
 }

 #webshot the image into the temp file
 webshot::webshot(temp_file,
                  file = image,
                  selector = selector,
                  ...)

 #read the image to display to markdown
 readfunc <- switch(type,
                    png = png::readPNG,
                    jpeg = jpeg::readJPEG)
 writefunc <- switch(type,
                     png = png::writePNG,
                     jpeg = jpeg::writeJPEG)

 #read
 img <- readfunc(image)
 class(img) <- c('tableHTML_image', class(img))

 #delete temp files
 file.remove(temp_file)
 file.remove(image)

 # shut down the current device
 if (!add) {
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  base::on.exit(graphics::par(mar = c(5.1, 4.1, 4.1, 2.1)))
 }

 #export the image
 if (is.null(file)) {
  grid::grid.raster(img)
  return(img)
 } else {
  writefunc(img, file)
  return(invisible(img))
 }

}

