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
#' hightlights the lowest n values. 'max' and 'min' are equivalent of top_1 and bottom_1. 'colour-rank' applies 
#' one of the \code{colour_rank_theme}s. 
#'
#' @param n the number of rows to highlight in 'top_n' and 'bottom_n'. If no value for n is provided, 1 is assumed
#' with a warning.
#' 
#' @param value the comparison value for "==", "!=", ">", ">=", "<", and "<=". value is the right hand side of
#' the equation.
#' 
#' @param between a numeric vector of length 2 that defines a range, where \code{between[1]} is the lower bound 
#' and \code{between[2]} is the upper bound of the range. between is inclusive. 
#'  
#' @param colour_rank_theme You can either pick one of the provided themes (RAG, White-Red, White-Green, 
#' White-Blue, Spectral, or Rainbow) or create your own by choosing 'Custom' and providing a custom css list
#' in \code{colour_rank_css}. 
#'
#' @param columns A character atomic vector with the names of the columns or a numeric atomic vector
#'   with the positions of the columns where the style definitions will be applied on. At least one
#'   column must be provided. If the rownames are included the column name is "tableHTML_rownames"
#'   and the position is 0. If row_groups are are included the column name is "tableHTML_row_groups"
#'   and the position is -1. 
#'   
#' @param colour_rank_css An optional named list with css to be applied if a custom styling should be used. 
#' The names correspond to a header of the tableHTML, 'rownames', or 'row_groups'. 
#' The elements of this css named list are themselves lists of an atomic vector with style definitions (e.g. background-color). 
#' and a list of atomic vecors that contains the style definitions' values with the same 
#' length as the number of rows for each style definition. You can use \code{make_css_colour_rank_theme} to obtain this list.
#'   
#' @param same_scale This logical flag indicates whether all of the columns the css should be applied to 
#' are on the same scale. If TRUE, the same value in all columns will have the same colour applied to it.
#' If FALSE, the colours will be assigned between the columns min and max. 
#'  
#' @param levels A character vector with the factor levels of the columns. Will be used to order factors for colour ranks. 
#' If NULL, factor levels are in alphabetic order.
#' 
#' @inheritParams add_css_column
#' @inheritParams base::order
#'
#' @return A tableHTML object. 
#'         
#' @examples
#' 
#' 
#' tableHTML(mtcars, theme = "scientific") %>% 
#'  add_css_conditional_column(conditional = "colour_rank", colour_rank_theme =  "RAG",
#'   columns = 1) %>%
#'  add_css_conditional_column(conditional = "colour_rank", colour_rank_theme = "Rainbow", columns = 5, decreasing = TRUE)
#'  
#' css <- make_css_colour_rank_theme(list(qsec = mtcars$qsec),
#'                                    colours = RColorBrewer::brewer.pal(9, "Set1"))
#' tableHTML (mtcars) %>%
#'   add_css_conditional_column(conditional = "colour_rank", 
#'   colour_rank_theme =  "Custom", 
#'                                css = css, 
#'                                columns = 7, 
#'                                decreasing = FALSE,
#'                                same_scale = FALSE)
#'   
#' tableHTML %>%
#'   add_css_conditional_column(conditional = "colour_rank", colour_rank_theme = "RAG",
#'                                columns = c(1, 5)) %>%
#'   add_css_conditional_column(conditional = "colour_rank", colour_rank_theme = "White-Blue",
#'                                columns = c(8, 11),
#'                                same_scale = TRUE) %>%
#'   add_css_conditional_column(conditional = "colour_rank", colour_rank_theme = "White-Red",
#'                                columns = c(9, 10),
#'                                same_scale = FALSE)
#'
#' @export

add_css_conditional_column <- function(tableHTML,
                                       conditional = c("colour_rank", "==", "!=", "min", "max", "top_n", "bottom_n", ">", ">=",
                                                       "<", "<=", "between"),
                                       n = NULL,
                                       value = NULL,
                                       between = NULL,
                                       css = NULL,
                                       colour_rank_theme = c("Custom", "RAG", "Spectral", "Rainbow",
                                                             "White-Green", "White-Red", "White-Blue"),
                                       columns,
                                       colour_rank_css = NULL,
                                       decreasing = FALSE, 
                                       same_scale = TRUE,
                                       levels = NULL) {
  
  #checks
  if (!inherits(tableHTML, 'tableHTML')) stop('tableHTML needs to be a tableHTML object')
  #persist attributes
  attributes <- attributes(tableHTML)
  
  conditional <- match.arg(conditional)
  
  colour_rank_theme <- match.arg(colour_rank_theme)
 
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
  
  if (conditional %in% c("==", "!=", ">", ">=", "<", "<=")) {
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
    if (between[1] >= between[2]) {
      stop('begin must be smaller than end in between')
    }
  }
  
  if (conditional != "colour_rank") {
    if (is.null(css)) {
      stop('css needs to be provided')
    }
  }
  
  #colour_rank_css structure
  if (colour_rank_theme == "Custom" & conditional == "colour_rank") {
    if (is.null(colour_rank_css)) {
      stop("colour_rank_css needs to be provided for custom conditional formatting")
    }
    if (! unique(unlist(lapply(1:length(colour_rank_css), function(i) {
      lengths(colour_rank_css[[i]])
    }))) == 1) {
      stop('colour_rank_css must be a list of corresponding css properties and css property values')
    }
    if (is.null(names(colour_rank_css))) {
      stop('colour_rank_css must be a named list')
    } 
    if (min(names(colour_rank_css) %in% c(attributes$headers, "rownames", "row_groups")) < 1 ) {
      stop('names of colour_rank_css do not correspond to tableHTML columns')
    }
    if (!min(unlist(lapply(1:length(colour_rank_css), function(i) {
      min(lengths(colour_rank_css[[i]][[2]])) == attributes$nrows
    }))) == 1) {
      stop('the number of colour_rank_css values provided differs from the number of rows')
    }

  } else {
    if (conditional == "colour_rank" & !is.null(colour_rank_css)) {
      warning("colour rank theme selected, colour_rank_css will be ignored")
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
                  "colour_rank" = NULL,
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

  colour_rank_theme_colours <- switch(colour_rank_theme,
                                    "RAG" = c("#86c183", "#B9D48A", "#FCEC92", "#EFAE7F","#F8696B"),
                                    "Spectral" = c("#5E4FA2", "#5E4FA2", "#5E4FA2", "#ABDDA3", "#E6F598",
                                                   "#FFFDBF", "#FEE08B", "#FDAE61", "#F36D43", "#D53E4E", "#D53E4E"),
                                    "Rainbow" = c('#7394b1', '#89b185', '#c7ca80', '#c6b37f', '#ac6363'),
                                    "White-Green" = c("#FCFCFF", "#DAECDF", "#B9DCBF", "#99CCA0", "#7ABC81"),
                                    "White-Red" = c("#FCFCFF", "#F6D9DA", "#EFB6B6", "#EA9393", "#E7726F"),
                                    "White-Blue" = c("#FCFCFF", "#D9E6F3", "#B3CCE7", "#8EB3DC", "#6A99D0"),
                                    NULL
  )
  
  if (is.null(style) & !is.null(colour_rank_theme_colours)) {

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
  
  if (!is.null(colour_rank_theme_colours)) {
    column_data <- extract_column_data(tableHTML, indices, levels)
    colour_rank_css <- make_css_colour_rank_theme(column_data,
                                       colour_rank_theme_colours, 
                                       decreasing = decreasing, 
                                       same_scale = same_scale)
  }
 
  if (is.null(style)) {
    style <- lapply(all_names, function(name) {
      make_style_from_css("column")(colour_rank_css, name)
    })
    names(style) <- all_names
  }
  
  for (i in indices) {
    
    if (isTRUE(all.equal(i, 0))) {
      
      tableHTML <- replace_style(tableHTML, split = 'id="tableHTML_rownames"', style[["rownames"]],
                                 condition[["rownames"]])

    } else if (isTRUE(all.equal(i, -1))) {

      tableHTML <- replace_style(tableHTML, split = 'id="tableHTML_row_groups"', style[["row_groups"]],
                                 condition[["row_groups"]])
      
    } else {
      
      tableHTML <- replace_style(tableHTML, split = paste0('id="tableHTML_column_', i, '"'),
                                 style[[attributes$headers[i]]],
                                 condition[[attributes$headers[i]]])
    }
  }
  
  tableHTML <- htmltools::HTML(tableHTML)
  
  class(tableHTML) <- c('tableHTML', class(tableHTML))
  
  attributes(tableHTML) <- attributes
  
  tableHTML
  
}