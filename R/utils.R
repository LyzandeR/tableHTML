is.fachar <- function(x) is.character(x) | is.factor(x)


replace_style<- function(tableHTML, split, style, condition = NULL){

  if (is.null(condition)) {
    condition <- rep(TRUE, attr(tableHTML, "nrows"))
  }

  tableHTML <- gsub(paste(split, "style="),
                    paste0(split), tableHTML)

  splits <- strsplit(tableHTML, split)

  splits[[1]][2:length(splits[[1]])] <-
    vapply(2:length(splits[[1]]), function(i) {
      x <- splits[[1]][i]
      #check if style should be applied
      if (condition[i - 1]) {
        paste0(split,' ', style[i - 1], x)
      } else {
        #if not, check if style was applied from previous functions
        # or if rowspan attribute present
        if (substr(x, 1, 1) == ">" | substr(x, 1, 8) == " rowspan") {
          paste0(split, x)
        } else {
          paste0(split, ' style=', x)
        }
      }

    }, FUN.VALUE = character(1))

  gsub(';""', ';', paste(splits[[1]], collapse = ''))

}

fix_header_dupes <- function(h) {
 dup_positions <- which(duplicated(h))
 empty_strings <- sapply(dup_positions, function(x) paste0(rep(' ', x), collapse = ''))
 h[dup_positions] <- paste0(h[dup_positions], empty_strings)
 h
}



