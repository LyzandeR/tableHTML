context("scientific theme testing")

test_that("Function fails for wrong inputs", {

 #all checks ok
 expect_error(mtcars %>%
               tableHTML() %>%
               add_theme('scientific'),
              NA)

 #background is correct
 expect_true(grepl('text-align:center',
                   mtcars %>%
                    tableHTML() %>%
                    add_theme('rshiny-blue')))

 #row groups exist
 expect_true(grepl('vertical-align:top;border-bottom:3px solid black;',
                   tableHTML(mtcars,
                             rownames = TRUE,
                             widths = c(110, 140, rep(50, 11)),
                             row_groups = list(c(10, 10, 12), c('Group 1', 'Group 2', 'Group 3')),
                             second_headers = list(c(2, 5, 6), c('', 'col2', 'col3'))) %>%
                    add_theme('scientific')))

 #second headers are ok
 expect_true(grepl('tableHTML_second_header_2',
                   tableHTML(mtcars,
                             rownames = TRUE,
                             widths = c(110, 140, rep(50, 11)),
                             row_groups = list(c(10, 10, 12), c('Group 1', 'Group 2', 'Group 3')),
                             second_headers = list(c(2, 5, 6), c('', 'col2', 'col3'))) %>%
                    add_theme('rshiny-blue')))
})

