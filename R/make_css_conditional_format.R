#' @importFrom grDevices colorRampPalette col2rgb
NULL

#' Get css properties for custom color rank theme
#'
#' \code{make_css_color_rank_theme} will create a list of css properties needed for custom conditional formatting.
#'
#' \code{make_css_color_rank_theme} will add conditional css to a tableHTML's columns. \code{add_conditional_css_column} will only
#'   add css to the columns without the headers or second headers (i.e. it only affects the td tag
#'   internally and not the th tag). If you want to add css to the headers or second headers please
#'   use \code{add_css_header} or \code{add_css_second_header}.
#'
#' @param column_data A named list of vectors of values that are in a tableHTML column which
#'  should be mapped to a color palette.
#'
#' @param css_property Character. An optional character specifying the css attribute
#' that should be used. Default is \code{'backgroud-color'}
#'
#' @inheritParams grDevices::colorRampPalette
#' @inheritParams base::order
#' @inheritParams add_css_conditional_column
#'
#' @return A list of css properties
#'
#' @examples
#'
#' tableHTML <- tableHTML(mtcars)
#'
#' css <- make_css_color_rank_theme(list(mpg = mtcars$mpg),
#'                                  c("orange", "yellow","springgreen","royalblue"))
#'
#' tableHTML %>% add_css_conditional_column(conditional = "color_rank",
#'                                         color_rank_theme = "Custom",
#'                                         color_rank_css = css, column = 1)
#'
#' @export

make_css_color_rank_theme <- function(column_data,
                                      colors,
                                      css_property = "background-color",
                                      decreasing = FALSE,
                                      same_scale = TRUE) {

  #checks
  if (class(column_data) != 'list' | is.null(names(column_data))) {
    stop('column_data must be a named list')
  }

  check <- tryCatch(col2rgb(colors),
    error = function(e) {
      stop('colors argument not valid. see ?col2rgb for more details')
    }
  )

  col_names <- names(column_data)

  cols_context <- switch(ifelse(same_scale, "TRUE", "FALSE"),
                         "TRUE" = function(x) { return(unname(unlist(column_data))) },
                         "FALSE" = function(y) { return(y) }
  )

  css <- lapply(column_data, function(cd) {

    css_colors <- character(length(cd))

    if (is.numeric(cols_context(cd))) {
     colors <- if (decreasing) rev(colors) else colors

     color_palette <- colorRampPalette(colors)

     colsteps <- max(cols_context(cd)) - min(cols_context(cd)) + 1

     value <- unique(cols_context(cd))[order(unique(cols_context(cd)),
                                             decreasing = decreasing)]

     color <- color_palette(colsteps)[findInterval(value,
                                                     seq(min(value),
                                                         max(value),
                                                         length.out = colsteps))]
    } else {
     value <- unique(cols_context(cd))[order(unique(cols_context(cd)),
                                             decreasing = decreasing)]

     color_palette <- colorRampPalette(colors)

     color <- color_palette(length(unique(cols_context(cd))))
    }



    col_df <- data.frame(value = value,
                         color = color,
                         stringsAsFactors = FALSE)

    css_colors[1:length(cd)] <-
      vapply(1:length(cd), function(i) {

        col_df$color[which(col_df$value %in%  cd[i])]

      }, FUN.VALUE = character(1))

    list(c(css_property), list(c(css_colors)))


  })

  names(css) <- col_names

  return(css)
}






