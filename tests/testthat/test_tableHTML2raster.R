context("tableHTML_to_raster testing")

test_that("Function fails for wrong inputs", {
 #no tableHTML
 expect_error(tableHTML_to_raster(c('a', 'b')),
              'tableHTML_image needs to be of class')

 #all checks ok
 expect_error(mtcars %>%
               head() %>%
               tableHTML(widths = c(120, rep(60, 11))) %>%
               add_theme('scientific') %>%
               tableHTML_to_image() %>%
               tableHTML_to_raster(),
              NA)

 #all checks ok
 expect_true('rastergrob' %in% class(mtcars %>%
                                      head() %>%
                                      tableHTML(widths = c(120, rep(60, 11))) %>%
                                      add_theme('scientific') %>%
                                      tableHTML_to_image() %>%
                                      tableHTML_to_raster()))

})
