#for the editable code
get_pattern_editable <- function(i) {
 i = as.character(i)
 switch(i,
        '-1' = '.*<td id="tableHTML_rownames".*>(.*)</td>.*',
        '0' = '.*<td id="tableHTML_row_groups".*>(.*)</td>.*',
        paste0('.*<td id="tableHTML_column_', i, '.*>(.*)</td>.*'))
}

#the core of the function for editable
add_div_editable <- function(tableHTML, i) {
 pattern <- get_pattern_editable(i)

 captures <- gregexpr(pattern,
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

 return(tableHTML)

}
