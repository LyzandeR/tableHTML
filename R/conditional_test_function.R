

conditional_test_function <- function(column_data,
                                      conditional,
                                      same_scale = TRUE,
                                      ...) {
  
  arguments <- list(...)
  
  col_names <- names(column_data)
  cols_context <- switch(ifelse(same_scale, "TRUE", "FALSE"),
                         "TRUE" = function(x) { return(unname(unlist(column_data))) },
                         "FALSE" = function(y) { return(y) }
  )

  lapply(column_data, function(cd) {
    
    switch(conditional,
           "==" = cd == arguments$comparison_value,
           "!=" = cd != arguments$comparison_value,
           between = (arguments$comparison_value[1] <= cd) & (cd <= arguments$comparison_value[2]),
           top_n = cd %in% tail(sort(cols_context(cd)), arguments$comparison_value),
           bottom_n = cd %in% head(sort(cols_context(cd)), arguments$comparison_value),
           ">" = cd > arguments$comparison_value,
           ">=" = cd >= arguments$comparison_value,
           "<" = cd < arguments$comparison_value,
           "<=" = cd <= arguments$comparison_value,
           ">" = cd > arguments$comparison_value,
           min = cd == min(cols_context(cd)),
           max = cd == max(cols_context(cd))
           )
  })
  
}
