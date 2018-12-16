context("add_theme and rshiny-blue testing")

test_that("Function fails for wrong inputs", {

 #wrong theme
 expect_error(mtcars %>%
               tableHTML() %>%
               add_theme('abc'),
              'should be one of ')

 #all checks ok
 expect_error(mtcars %>%
               tableHTML() %>%
               add_theme('rshiny-blue'),
              NA)

 #background is correct
 expect_true(grepl('"background-color:#f2f2f2;"',
                   mtcars %>%
                     tableHTML() %>%
                     add_theme('rshiny-blue')))

 #row groups exist
 expect_true(grepl('style="vertical-align:top;background-color:white;"',
                   tableHTML(mtcars,
                             rownames = TRUE,
                             widths = c(110, 140, rep(50, 11)),
                             row_groups = list(c(10, 10, 12), c('Group 1', 'Group 2', 'Group 3')),
                             second_headers = list(c(2, 5, 6), c('', 'col2', 'col3'))) %>%
                    add_theme('rshiny-blue')))

 #second headers are ok
 expect_true(grepl('style="font-size:25px;',
                   tableHTML(mtcars,
                             rownames = TRUE,
                             widths = c(110, 140, rep(50, 11)),
                             row_groups = list(c(10, 10, 12), c('Group 1', 'Group 2', 'Group 3')),
                             second_headers = list(c(2, 5, 6), c('', 'col2', 'col3'))) %>%
                    add_theme('rshiny-blue')))
})

