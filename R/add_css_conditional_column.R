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
#' @param conditional_theme You can either pick one of the provided themes (RAG, White-Red, White-Green, 
#' White-Blue, Spectral, or Rainbow) or create your own by choosing 'Custom' and providing a custom css list. 
#'
#' @param columns A character atomic vector with the names of the columns or a numeric atomic vector
#'   with the positions of the columns where the style definitions will be applied on. At least one
#'   column must be provided. If the rownames are included the column name is "tableHTML_rownames"
#'   and the position is 0. If row_groups are are included the column name is "tableHTML_row_groups"
#'   and the position is -1. 
#'   
#' @param css An optional named list with css to be applied if a custom styling should be used. 
#' The names correspond to a header of the tableHTML, 'rownames', or 'row_groups'. 
#' The elements of this css named list are themselves lists of an atomic vector with style definitions (e.g. background-color). 
#' and a list of atomic vecors that contains the style definitions' values with the same 
#' length as the number of rows for each style definition. You can use \code{make_css_conditional_format} to obtain this list.
#'   
#' @param same_scale This logical flag indicates whether all of the columns the css should be applied to 
#' are on the same scale. If TRUE, the same value in all columns will have the same colour applied to it.
#' If FALSE, the colours will be assigned between the columns min and max. 
#'  
#' @param levels. A character vector with the factor levels of the columns. Will be used to order factors. 
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
#'  add_css_conditional_column(conditional_theme = "RAG", columns = 1) %>%
#'  add_css_conditional_column(conditional_theme = "Rainbow", columns = 5, decreasing = TRUE)
#'  
#' css <- make_css_conditional_format(list(qsec = mtcars$qsec),
#'                                    colors = RColorBrewer::brewer.pal(9, "Set1"))
#' tableHTML (mtcars) %>%
#'   add_css_conditional_column(conditional_theme = "Custom", 
#'                                css = css, 
#'                                columns = 7, 
#'                                decreasing = FALSE,
#'                                same_scale = FALSE)
#'   
#' tableHTML %>%
#'   add_css_conditional_column(conditional_theme = "RAG",
#'                                columns = c(1, 5)) %>%
#'   add_css_conditional_column(conditional_theme = "White-Blue",
#'                                columns = c(8, 11),
#'                                same_scale = TRUE) %>%
#'   add_css_conditional_column(conditional_theme = "White-Red",
#'                                columns = c(9, 10),
#'                                same_scale = FALSE)
#'
#' @export

add_css_conditional_column <- function(tableHTML,
                                       conditional_theme = c("Custom", "RAG", "Spectral", "Rainbow",
                                                             "White-Green", "White-Red", "White-Blue"),
                                       columns,
                                       css = NULL,
                                       decreasing = FALSE, 
                                       same_scale = TRUE,
                                       levels = NULL) {
  
  #checks
  if (!inherits(tableHTML, 'tableHTML')) stop('tableHTML needs to be a tableHTML object')
  
  conditional_theme <- match.arg(conditional_theme)
  
  #css structure
  if (conditional_theme == "Custom") {
    if (is.null(css)) {
      stop("css needs to be provided for custom conditional formatting")
    }
    if (! unique(unlist(lapply(1:length(css), function(i) {
      lengths(css[[i]])
    }))) == 1) {
      stop('css must be a list of corresponding css properties and css property values')
    }
    if (is.null(names(css))) {
      stop('css must be a named list')
    } 
    if (min(names(css) %in% c(attr(tableHTML, "headers"), "rownames", "row_groups")) < 1 ) {
      stop('names of css do not correspond to tableHTML columns')
    }
    if (!min(unlist(lapply(1:length(css), function(i) {
      min(lengths(css[[i]][[2]])) == attr(tableHTML, 'nrows')
    }))) == 1) {
      stop('the number of css values provided differs from the number of rows')
    }

  } else {
    if (!is.null(css)) {
      warning("conditional theme selected, css will be ignored")
    }
  }
  
  if (is.numeric(columns) | suppressWarnings(!any(is.na(as.numeric(columns))))) {
    indices <- columns
  } else {
    indices <- which(attr(tableHTML, 'header') %in% columns) 
    if ('row_groups' %in% columns) {
      indices <- c(-1, indices)
    }
    if ('rownames' %in% columns) {
      indices <- c(0, indices)
    }
    indices <- sort(indices)
  }
  
  attributes <- attributes(tableHTML)
  
  theme_colors <- switch(conditional_theme,
                         "RAG" = c("#86c183", "#B9D48A", "#FCEC92", "#EFAE7F","#F8696B"),
                         "Spectral" = c("#5E4FA2", "#5E4FA2", "#5E4FA2", "#ABDDA3", "#E6F598",
                                        "#FFFDBF", "#FEE08B", "#FDAE61", "#F36D43", "#D53E4E", "#D53E4E"),
                         "Rainbow" = c('#7394b1', '#89b185', '#c7ca80', '#c6b37f', '#ac6363'),
                         "White-Green" = c("#FCFCFF", "#DAECDF", "#B9DCBF", "#99CCA0", "#7ABC81"),
                         "White-Red" = c("#FCFCFF", "#F6D9DA", "#EFB6B6", "#EA9393", "#E7726F"),
                         "White-Blue" = c("#FCFCFF", "#D9E6F3", "#B3CCE7", "#8EB3DC", "#6A99D0"),
                         NULL
  )
  
  if (!is.null(theme_colors)) {
    column_data <- extract_column_data(tableHTML, indices, levels)
    css <- make_css_conditional_format(column_data,
                                       theme_colors, 
                                       decreasing = decreasing, 
                                       same_scale = same_scale)
  }
  
  for (i in indices) {
    
    if (identical(i, 0)) {
      
      style <- make_style_from_css("column")(css, "rownames")
      tableHTML <- replace_style(tableHTML, split = 'id="tableHTML_rownames"', style)
      
    } else if (identical(i, -1)) {
      
      style <- make_style_from_css("column")(css, "row_groups")
      tableHTML <- replace_style(tableHTML, split = 'id="tableHTML_row_groups"', style)
      
    } else {
      
      style <- make_style_from_css("column")(css, attributes$headers[i])
      tableHTML <- replace_style(tableHTML, split = paste0('id="tableHTML_column_', i, '"'), style)
      
    }
  }
  
  tableHTML <- htmltools::HTML(tableHTML)
  
  class(tableHTML) <- c('tableHTML', class(tableHTML))
  
  attributes(tableHTML) <- attributes
  
  tableHTML
  
}