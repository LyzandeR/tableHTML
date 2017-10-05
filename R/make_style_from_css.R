
make_style_from_css <- function(direction) {

  switch(direction,
         column = function(css, name) {
           css <- css[[name]]
           
           css_comp <-
             lapply(1:length(css[[1]]), function(i) {
               as.vector(outer(css[[1]][i], css[[2]][[i]], paste, sep=":"))
             })
           
           css_comp <- do.call("paste", c(css_comp, sep=";"))
           
           style <- paste0('style="', css_comp, ';"')
           
         },
         row = stop("row not implemented yet")
  )
}
