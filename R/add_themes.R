#' Add a theme to the tableHTML
#'
#' \code{add_theme} will add a theme to tableHTML
#'
#' \code{add_theme} will add a theme to tableHTML.
#'
#' @param theme Pick one of the provided themes. These can still be modified by extra css. Choices
#'   are: scientific, rstudio-blue. Column widths are not provided when you select a theme.
#'   Please use the width argument for column widths.
#'
#' @return A tableHTML object.
#'
#' @examples
#' tableHTML(mtcars,
#'           rownames = FALSE,
#'           widths = c(140, rep(50, 11)),
#'           row_groups = list(c(10, 10, 12), c('Group 1', 'Group 2', 'Group 3')),
#'           second_headers = list(c(3, 4), c('col1', 'col2'))) %>%
#'   add_theme('scientific')
#'
#' tableHTML(mtcars, widths = c(140, rep(50, 11))) %>%
#'   add_theme ('rshiny-blue')
#'
#' @export
add_theme <- function(tableHTML,
                      theme = c('scientific', 'rshiny-blue')) {

  #make sure they are ok
  theme <- match.arg(theme)

  #use appropriate function
  themefunc <- switch(theme,
                      scientific = theme_scientific,
                      `rshiny-blue` = theme_rshiny_blue)

  #return
  tableHTML %>% themefunc()

}
