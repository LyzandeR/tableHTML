HTMLtable <- function(obj, 
                      rownames = TRUE,
                      class = paste0('table_', deparse(substitute(obj))),
                      second_header = NULL) {
     
     if (rownames == TRUE) {
       headers <- paste0('<tr> <th id = header_1> </th>', 
                         paste0(vapply(seq_along(names(obj)) + 1,
                                       function(x){
                                        paste0('<th id=header_', x, '>',
                                               names(obj)[x - 1],
                                               '</th>')
                                        },
                                       FUN.VALUE=character(1)),
                                collapse=''),
                         '</tr>\n')
     } else {
       headers <- paste0('<tr>', 
                         paste0(vapply(seq_along(names(obj)), 
                                       function(x) {
                                        paste0('<th id = header_', 
                                               x, 
                                               '>',
                                               names(obj)[x],
                                               '</th>') 
                                        },
                                       FUN.VALUE = character(1)),
                                collapse = ''),
                         '</tr>\n')
     }
     
     if (!is.null(second_header)) {
       over_header <- 
         paste0('<tr>', 
                paste0(vapply(seq_along(second_header), 
                              function(x){
                               paste0('<th colspan =', 
                                      second_header[x],
                                      ' id = overheader_', 
                                      x,
                                      '>',
                               names(second_header)[x],
                               '</th>')
                              }, 
                              FUN.VALUE = character(1)),
                       collapse = ''),
                '</tr>\n')
     } else {
       over_header <- NULL
     }
     
     content <- lapply(names(obj), function(x) {
       paste0('<td align="right" id="', x, '">', obj[[x]], '</td>')
     })
     
     if (rownames == TRUE) {
       content <- c(list(paste0('<td align="right" id="rownames">', 
                                row.names(obj), 
                                '</td>')), 
                    content)
     }
     
     content <- cbind('<tr>', do.call(cbind, content), '</tr>\n')
     content <- paste(apply(content, 1, paste, collapse=''), collapse='')
     
     HTML(paste0('<table class=', 
                 class, 
                 ' border=1>\n', 
                 over_header, 
                 headers, 
                 content, 
                 '</table>', 
                 collapse=''))
   }
