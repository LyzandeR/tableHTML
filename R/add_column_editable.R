add_editable_column <- function(tableHTML,
                                columns) {

 #checks
 if (!inherits(tableHTML, 'tableHTML')) stop('tableHTML needs to be a tableHTML object')

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

 #keep attributes
 attributes <- attributes(tableHTML)

 #creating editable
 for (i in indices) {

   if (isTRUE(all.equal(i, 0))) {

    captures <- gregexpr('.*<td id="tableHTML_rownames".*>(.*)</td>.*',
                         tableHTML,
                         perl = TRUE)[[1]]
    captures_start <- attr(captures, 'capture.start')
    captures_length <- attr(captures, 'capture.length')
    for (y in seq_along(captures_start)) {

       first_part <- substr(tableHTML, 1, captures_start[y] - 1 + (y - 1) * 27)
       middle_part <- substr(tableHTML,
                             captures_start[y] + (y - 1) * 27,
                             captures_start[y] + captures_length[y] - 1 + (y - 1) * 27)
       last_part <- substring(tableHTML, captures_start[y] + captures_length[y] + (y - 1) * 27)
       tableHTML <- paste0(first_part, '<div contenteditable>', middle_part, '</div>', last_part)
    }

  } else if (isTRUE(all.equal(i, -1))) {

    captures <- gregexpr('.*<td id="tableHTML_row_groups".*>(.*)</td>.*',
                         tableHTML,
                         perl = TRUE)[[1]]
    captures_start <- attr(captures, 'capture.start')
    captures_length <- attr(captures, 'capture.length')
    for (y in seq_along(captures_start)) {

      first_part <- substr(tableHTML, 1, captures_start[y] - 1 + (y - 1) * 27)
      middle_part <- substr(tableHTML,
                            captures_start[y] + (y - 1) * 27,
                            captures_start[y] + captures_length[y] - 1 + (y - 1) * 27)
      last_part <- substring(tableHTML, captures_start[y] + captures_length[y] + (y - 1) * 27)
      tableHTML <- paste0(first_part, '<div contenteditable>', middle_part, '</div>', last_part)
    }

  } else {

    captures <- gregexpr(paste0('.*<td id="tableHTML_column_', i, '.*>(.*)</td>.*'),
                         tableHTML,
                         perl = TRUE)[[1]]
    captures_start <- attr(captures, 'capture.start')
    captures_length <- attr(captures, 'capture.length')
    for (y in seq_along(captures_start)) {

      first_part <- substr(tableHTML, 1, captures_start[y] - 1 + (y - 1) * 27)
      middle_part <- substr(tableHTML,
                            captures_start[y] + (y - 1) * 27,
                            captures_start[y] + captures_length[y] - 1 + (y - 1) * 27)
      last_part <- substring(tableHTML, captures_start[y] + captures_length[y] + (y - 1) * 27)
      tableHTML <- paste0(first_part, '<div contenteditable>', middle_part, '</div>', last_part)
    }

  }

 }

 attributes(tableHTML) <- attributes

 tableHTML

}
