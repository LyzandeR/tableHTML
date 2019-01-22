#' Add a theme to the tableHTML
#'
#' \code{add_theme} will add a theme to tableHTML
#'
#' \code{add_theme} will add a theme to tableHTML.
#'
#' @param tableHTML A tableHTML object.
#'
#' @param theme Pick one of the provided themes. These can still be modified by extra css. Choices
#'   are: scientific, rshiny-blue, totals. Column widths are not provided when you select a theme.
#'   Please use the width argument for column widths.
#'
#' @param ... Additional parameters to pass to the theme.
#' Currently \code{"totals"} is the only theme that takes additional parameters.
#' For more details on those parameters see \code{\link{add_theme_totals}}.
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
#' df <- mtcars[, 1:6]
#' df['Mean', ] <- (df %>% apply(2, mean))
#' df %>%
#'   tableHTML(widths = c(150, rep(70, ncol(df))), rownames = TRUE) %>%
#'   add_theme('totals')
#'
#' generate_df <- function(){
#'   df <- data.frame(Month = month.abb,
#'                    x1 = sample(1:100, 12),
#'                    x2 = sample(1:100, 12),
#'                    x3 = sample(1:100, 12),
#'                    stringsAsFactors = FALSE)
#'   df[nrow(df) + 1, ] <- c('Total', sum(df$x1), sum(df$x2), sum(df$x3))
#'   return(df)
#' }
#' df_1 <- generate_df()
#' df_2 <- generate_df()
#'
#' rbind(df_1, df_2) %>%
#'   tableHTML(widths = rep(50, 4), rownames = FALSE) %>%
#'   add_theme('totals', total_rows = c(13, 26),
#'             color = c('steelblue', 'green3'), id_column = TRUE)
#' @export
add_theme <- function (tableHTML,
                       theme = c("scientific", "rshiny-blue", "totals"), ...)
{
  theme <- match.arg(theme)
  themefunc <- switch(theme,
                      scientific = theme_scientific,
                      `rshiny-blue` = theme_rshiny_blue,
                      `totals` = add_theme_totals)
  tableHTML %>% themefunc(...)
}
