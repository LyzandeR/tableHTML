#' Add a theme to the tableHTML
#'
#' \code{add_theme} will add a theme to tableHTML
#'
#' \code{add_theme} will add a theme to tableHTML.
#'
#' @param tableHTML A tableHTML object.
#'
#' @param theme Pick one of the provided themes. These can still be modified by extra css. Choices
#'   are: scientific, rstudio-blue, totals-green, totals-blue. Column widths are not provided when you select a theme.
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
#' x1 <- sample(1:100, 12)
#' x2 <- sample(1:100, 12)
#' x3 <- sample(1:100, 12)
#' df <- data.frame(Month = month.abb, x1, x2, x3, 
#'                  stringsAsFactors = FALSE)
#' df[nrow(df)+1,]<- c('Total', sum(x1), sum(x2), sum(x3))
#' df %>% 
#'   tableHTML(widths = rep(50, 4), rownames = FALSE) %>% 
#'   add_theme('totals-green')
#'
#' @export
add_theme <- function (tableHTML, 
                       theme = c("scientific", "rshiny-blue", 
                                 "totals-green", "totals-blue")) 
{
  theme <- match.arg(theme)
  themefunc <- switch(theme, 
                      scientific = theme_scientific, 
                      `rshiny-blue` = theme_rshiny_blue,
                      `totals-green` = function(x){
                        add_theme_totals(x, color = 'darkgreen')},
                      `totals-blue` = function(x){
                        add_theme_totals(x, color = 'steelblue')})
  tableHTML %>% themefunc()
}