#for the editable code
get_pattern_column_css <- function(i) {
 i = as.character(i)
 switch(i,
        '-1' = 'id="tableHTML_row_groups"',
        '0' = 'id="tableHTML_rownames"',
        paste0('id="tableHTML_column_', i, '"'))
}

#the core of the function for editable
add_core_column_css <- function(tableHTML, i, style) {
 pattern <- get_pattern_column_css(i)

 tableHTML <- gsub(paste0(pattern, ' style='), pattern, tableHTML)
 tableHTML <- gsub(pattern, paste(pattern, style), tableHTML)
 tableHTML <- gsub(';""', ';', tableHTML)

 return(tableHTML)
}
