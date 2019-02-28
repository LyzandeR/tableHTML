extract_column_data <- function(tableHTML, indices) {

 attributes <- attributes(tableHTML)

  data_cols <- list()

  for (i in indices) {

    if (identical(i, 0)) {

      data_cols <- append(data_cols, list(row.names(attributes$data)))
      names(data_cols)[length(data_cols)] <- "rownames"

    } else if (identical(i, -1)) {

      data_cols <- append(data_cols, attributes$row_groups_data[2])
      names(data_cols)[length(data_cols)] <- "row_groups"

    } else {

      data_cols <- append(data_cols, list(attributes$data[, i]))
      names(data_cols)[length(data_cols)] <- attr(tableHTML, "headers")[i]


}

  }

  return(data_cols)

}
