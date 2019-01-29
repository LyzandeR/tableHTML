#' Add conditional css to tableHTML's columns
#'
#' \code{add_css_conditional_column} will add conditional css to a tableHTML's columns
#'
#' \code{add_css_conditional_column} will add conditional css to a tableHTML's columns. \code{add_css_conditional_column} will only
#'   add css to the columns without the headers or second headers (i.e. it only affects the td tag
#'   internally and not the th tag). If you want to add css to the headers or second headers please
#'   use \code{add_css_header} or \code{add_css_second_header}. If you want to apply the same
#'   css for all rows in a column, please use \code{add_css_column}.
#'
#' @param conditional Choose a conditional that should be used to apply css to rows in columns. '==' and '!=' evaluate equality
#' and inequality resp. '<', '<=', '>', and '>=' evaluate the respective operators with the values of
#' columns on the left. 'between' is SQL like, i.e. inclusive. 'top_n' highlights the n highest values columns, 'bottom_n'
#' hightlights the lowest n values. 'max' and 'min' are equivalent of top_1 and bottom_1. 'contains' uses \code{grepl()} to see
#' if values of a column contain a pattern specified in \code{value}. 'color-rank' applies
#' one of the \code{color_rank_theme}s.
#'
#' @param n the number of rows to highlight in 'top_n' and 'bottom_n'. If no value for n is provided, 1 is assumed
#' with a warning.
#'
#' @param value the comparison value for "==", "!=", ">", ">=", "<", "<=", and "contains". value is the right hand side of
#' the equation or the pattern in case of "contains".
#'
#' @param between a numeric vector of length 2 that defines a range, where \code{between[1]} is the lower bound
#' and \code{between[2]} is the upper bound of the range. between is inclusive.
#'
#' @param color_rank_theme You can either pick one of the provided themes (RAG, White-Red, White-Green,
#' White-Blue, Spectral, or Rainbow) or create your own by choosing 'Custom' and providing a custom css list
#' in \code{color_rank_css}.
#'
#' @param columns A character atomic vector with the names of the columns or a numeric atomic vector
#'   with the positions of the columns where the style definitions will be applied on. At least one
#'   column must be provided. If the rownames are included the column name is "tableHTML_rownames"
#'   and the position is 0. If row_groups are are included the column name is "tableHTML_row_groups"
#'   and the position is -1.
#'
#' @param color_rank_css An optional named list with css to be applied if a custom styling should be used.
#' The names correspond to a header of the tableHTML, 'rownames', or 'row_groups'.
#' The elements of this css named list are themselves lists of an atomic vector with style definitions (e.g. background-color).
#' and a list of atomic vecors that contains the style definitions' values with the same
#' length as the number of rows for each style definition. You can use \code{make_css_color_rank_theme} to obtain this list.
#'
#' @param same_scale Logical. This flag indicates whether the condition should be applied to columns individually or
#' in conjunction. If TRUE, the condition will be evaluated on all values of all \code{columns}. If FALSE,
#' the condition will be evaluated per column.
#'
#' @param levels A character vector with the factor levels of the columns. Will be used to order factors for color ranks.
#' If NULL, factor levels are in alphabetic order.
#'
#' @inheritParams add_css_column
#' @inheritParams base::order
#'
#' @return A tableHTML object.
#'
#' @examples
#'
#' qu_25_75 <- quantile(mtcars$disp, c(0.25, 0.75))
#'
#' tableHTML(mtcars) %>%
#'   add_css_conditional_column(conditional = "<",
#'                              value = qu_25_75[1],
#'                              css = list('background-color', "green"),
#'                              columns = c("disp")) %>%
#'   add_css_conditional_column(conditional = "between",
#'                              between = qu_25_75,
#'                              css = list('background-color', "orange"),
#'                              columns = c("disp")) %>%
#'   add_css_conditional_column(conditional = ">",
#'                              value = qu_25_75[2],
#'                              css = list('background-color', "red"),
#'                              columns = c("disp"))
#'
#' tableHTML(mtcars) %>%
#'   add_theme('rshiny-blue') %>%
#'   add_css_header(css = list(c("background-color", "color"),
#'                  c("darkgray", "white")),
#'                  headers = 1:12) %>%
#'   add_css_conditional_column(conditional = "min",
#'                              css = list('background-color', "#99CCA0"),
#'                              columns = c("wt")) %>%
#'   add_css_conditional_column(conditional = "max",
#'                              value = qu_25_75[1],
#'                              css = list('background-color', "#EA9393"),
#'                              columns = c("disp")) %>%
#'   add_css_conditional_column(conditional = "contains",
#'                              value = "Toyota",
#'                               css = list(c('background-color', "color"),
#'                                          c("lightgrey", "darkred")),
#'                               columns = c("rownames"))  %>%
#'   add_css_conditional_column(conditional = "contains",
#'                              value = "Mazda",
#'                              css = list(c('background-color', "color"),
#'                                         c("steelblue", "lightgray")),
#'                              columns = c("rownames")) %>%
#'   add_css_conditional_column(conditional = "color_rank",
#'                              color_rank_theme = "White-Blue",
#'                              columns = 11)
#'
#' tableHTML(mtcars) %>%
#'  add_theme('scientific') %>%
#'  add_css_conditional_column(conditional = "color_rank",
#'                             color_rank_theme =  "RAG",
#'                             columns = 1) %>%
#'  add_css_conditional_column(conditional = "color_rank",
#'                             color_rank_theme = "Rainbow",
#'                             columns = 5,
#'                             decreasing = TRUE)
#'
#' css <- make_css_color_rank_theme(list(qsec = mtcars$qsec),
#'                                   colors = c('#E41A1C', '#377EB8', '#4DAF4A',
#'                                              '#984EA3', '#FF7F00', '#FFFF33',
#'                                              '#A65628', '#F781BF', '#999999'))
#'
#' tableHTML(mtcars) %>%
#'   add_css_conditional_column(conditional = "color_rank",
#'                              color_rank_theme =  "Custom",
#'                              color_rank_css = css,
#'                              columns = 7,
#'                              decreasing = FALSE,
#'                              same_scale = FALSE)
#'
#' tableHTML(mtcars) %>%
#'   add_css_conditional_column(conditional = "color_rank",
#'                              color_rank_theme = "RAG",
#'                              columns = c(1, 5)) %>%
#'   add_css_conditional_column(conditional = "color_rank",
#'                              color_rank_theme = "White-Blue",
#'                              columns = c(8, 11),
#'                              same_scale = TRUE) %>%
#'   add_css_conditional_column(conditional = "color_rank",
#'                              color_rank_theme = "White-Red",
#'                              columns = c(9, 10),
#'                              same_scale = FALSE)
#'
#' @export

add_css_conditional_column <- function(tableHTML,
                                       columns,
                                       conditional = c("color_rank", "==", "!=", "min", "max", "top_n", "bottom_n", ">", ">=",
                                                       "<", "<=", "between", "contains"),
                                       n = NULL,
                                       value = NULL,
                                       between = NULL,
                                       css = NULL,
                                       color_rank_theme = c("Custom", "RAG", "Spectral", "Rainbow",
                                                             "White-Green", "White-Red", "White-Blue"),
                                       color_rank_css = NULL,
                                       decreasing = FALSE,
                                       same_scale = TRUE,
                                       levels = NULL) {

  #checks
  if (!inherits(tableHTML, 'tableHTML')) stop('tableHTML needs to be a tableHTML object')

  if (!suppressWarnings(sum(is.na(as.numeric(columns)))) %in% c(0, length(columns))) {
    stop("columns must either be numeric or text, but not both")
  }

  #persist attributes
  attributes <- attributes(tableHTML)

  if (c("row_groups") %in% columns) {
    if (!attributes$row_groups) {
      stop('tableHTML does not have row_groups')
    }
  }

  if (c("rownames") %in% columns) {
    if (!attributes$rownames) {
      stop('tableHTML does not have rownames')
    }
  }

  conditional <- match.arg(conditional)

  color_rank_theme <- match.arg(color_rank_theme)

  if (conditional %in% c("top_n", "bottom_n")) {
    if (is.null(n)) {
      n <- 1
      warning(sprintf('n not provided, %s 1 value(s) highlighted', substr(conditional, 1, nchar(conditional) - 2)))
    }
    if (n > attributes$nrows) {
      stop('n cannot exceed number of rows')
    } else {
      if (n == attributes$nrows) {
        warning('all rows selected. you might want to use add_css_column()')
      }
    }
  }

  if (conditional %in% c("==", "!=", ">", ">=", "<", "<=", "contains")) {
    if (is.null(value)) {
      stop('comparison value needed for comparison')
    }
  }

  if (conditional == "between") {
    if (is.null(between)) {
      stop('begin and end values for between need to be provided')
    }
    if (suppressWarnings(any(is.na(as.numeric(between))))) {
      stop('begin and end values of begin must be numeric')
    }
    if (class(between) != "numeric" | length(between) != 2) {
      stop('between needs to be a vector with 2 elements')
    }
    if (between[1] >= between[2]) {
      stop('begin must be smaller than end in between')
    }
  }

  if (conditional != "color_rank") {
    if (is.null(css)) {
      stop('css needs to be provided')
    }
  }

  #color_rank_css structure
  if (color_rank_theme == "Custom" & conditional == "color_rank") {
    if (is.null(color_rank_css)) {
      stop("color_rank_css needs to be provided for custom conditional formatting")
    }
    if (! unique(unlist(lapply(1:length(color_rank_css), function(i) {
      lengths(color_rank_css[[i]])
    }))) == 1) {
      stop('color_rank_css must be a list of corresponding css properties and css property values')
    }
    if (is.null(names(color_rank_css))) {
      stop('color_rank_css must be a named list')
    }
    if (min(names(color_rank_css) %in% c(attributes$headers, "rownames", "row_groups")) < 1 ) {
      stop('names of color_rank_css do not correspond to tableHTML columns')
    }
    if (attributes$row_groups) {
      if (!isTRUE(any(unlist(lapply(1:length(color_rank_css), function(i) {
        min(lengths(color_rank_css[[i]][[2]])) ==
          lengths(gregexpr('<td id="tableHTML_row_groups', tableHTML))
      }))))) {
        stop('the number of color_rank_css values provided differs from the number of row groups')
      }
    } else {
      if (!isTRUE(any(unlist(lapply(1:length(color_rank_css), function(i) {
        min(lengths(color_rank_css[[i]][[2]])) == attributes$nrows
      }))))) {
        stop('the number of color_rank_css values provided differs from the number of rows')
      }
    }

  } else {
    if (conditional == "color_rank" & !is.null(color_rank_css)) {
      warning("color rank theme selected, color_rank_css will be ignored")
    }
  }

  if (is.numeric(columns) | suppressWarnings(!any(is.na(as.numeric(columns))))) {
    indices <- columns
  } else {
    indices <- which(attributes$header %in% columns)
    if ('row_groups' %in% columns) {
      indices <- c(-1, indices)
    }
    if ('rownames' %in% columns) {
      indices <- c(0, indices)
    }
    indices <- sort(indices)
  }

  all_names <- c(c('row_groups', 'rownames'),
                 attributes$headers)[which(c(-1, 0, 1:attributes$ncols) %in% indices)]

  #create style for conditionals
  style <- switch(conditional,
                  "color_rank" = NULL,
                  {
                  css_comp <- paste0(css[[1]], ':', css[[2]], ';')
                  css_comp <- paste(css_comp, collapse = '')
                  style_temp <- rep(list(rep(paste0('style="', css_comp, '"'),
                                             attributes$nrows)),
                                    length(all_names))
                  names(style_temp) <- all_names
                  style_temp
                  }
                  )

  color_rank_theme_colors <- switch(color_rank_theme,
                                    "RAG" = c("#86c183", "#B9D48A", "#FCEC92", "#EFAE7F","#F8696B"),
                                    "Spectral" = c("#5E4FA2", "#5E4FA2", "#5E4FA2", "#ABDDA3", "#E6F598",
                                                   "#FFFDBF", "#FEE08B", "#FDAE61", "#F36D43", "#D53E4E", "#D53E4E"),
                                    "Rainbow" = c('#7394b1', '#89b185', '#c7ca80', '#c6b37f', '#ac6363'),
                                    "White-Green" = c("#FCFCFF", "#DAECDF", "#B9DCBF", "#99CCA0", "#7ABC81"),
                                    "White-Red" = c("#FCFCFF", "#F6D9DA", "#EFB6B6", "#EA9393", "#E7726F"),
                                    "White-Blue" = c("#FCFCFF", "#D9E6F3", "#B3CCE7", "#8EB3DC", "#6A99D0"),
                                    NULL
  )

  if (is.null(style) & !is.null(color_rank_theme_colors)) {

    condition <- rep(list(rep(TRUE, attributes$nrows)), length(indices))
    names(condition) <- all_names

  } else {
    comparison_value <- switch(conditional,
                               "top_n" = n,
                               "bottom_n" = n,
                               "between" = between,
                               value)

    column_data <- extract_column_data(tableHTML, indices, levels)

    condition <- conditional_test_function(column_data = column_data,
                                           conditional = conditional,
                                           same_scale = same_scale,
                                           comparison_value = comparison_value)
  }

  if (!is.null(color_rank_theme_colors)) {
    column_data <- extract_column_data(tableHTML, indices, levels)
    color_rank_css <- make_css_color_rank_theme(column_data,
                                       color_rank_theme_colors,
                                       decreasing = decreasing,
                                       same_scale = same_scale)
  }

  if (is.null(style)) {
    style <- lapply(all_names, function(name) {
      make_style_from_css("column")(color_rank_css, name)
    })
    names(style) <- all_names
  }



  for (i in indices) {

    if (isTRUE(all.equal(i, 0))) {

      tableHTML <- replace_style(tableHTML, split = 'id="tableHTML_rownames"', style[["rownames"]],
                                 condition[["rownames"]])
      attributes(tableHTML) <- attributes

    } else if (isTRUE(all.equal(i, -1))) {

      tableHTML <- replace_style(tableHTML, split = 'id="tableHTML_row_groups"', style[["row_groups"]],
                                 condition[["row_groups"]])
      attributes(tableHTML) <- attributes

    } else {

      tableHTML <- replace_style(tableHTML, split = paste0('id="tableHTML_column_', i, '"'),
                                 style[[attributes$headers[i]]],
                                 condition[[attributes$headers[i]]])
      attributes(tableHTML) <- attributes
    }
  }

  tableHTML <- htmltools::HTML(tableHTML)

  class(tableHTML) <- c('tableHTML', class(tableHTML))

  attributes(tableHTML) <- attributes

  tableHTML

}
