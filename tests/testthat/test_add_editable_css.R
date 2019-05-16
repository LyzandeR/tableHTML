context("add_editable_column testing")

test_that("Function fails for wrong inputs", {

 tHTML <- tableHTML(mtcars,
                    rownames = TRUE,
                    widths = c(150, 100, rep(50, 11)),
                    row_groups = list(c(10, 10, 12), c('Group 1', 'Group 2', 'Group 3'))) %>%
  add_css_column(css = list('background-color', 'lightgray'), columns = 'row_groups') %>%
  add_css_column(css = list('text-align', 'right'), columns = 'row_groups') %>%
  add_css_header(css = list('background-color', 'lightgray'), headers = 1)

 #no tableHTML
 expect_error(add_editable_column(mtcars, columns = 1:3),
              'tableHTML needs to be')

 #all checks ok
 expect_error(tHTML %>% add_editable_column(-1:3), NA)

})

test_that("Function outputs the correct HTML", {

 tHTML <- tableHTML(mtcars,
                    rownames = TRUE,
                    widths = c(150, 100, rep(50, 11)),
                    row_groups = list(c(10, 10, 12), c('Group 1', 'Group 2', 'Group 3'))) %>%
  add_css_column(css = list('background-color', 'lightgray'), columns = 'row_groups') %>%
  add_css_column(css = list('text-align', 'right'), columns = 'row_groups') %>%
  add_css_header(css = list('background-color', 'lightgray'), headers = 1)

 #rows ok
 expect_true(grepl('id="tableHTML_rownames"><div contenteditable>',
                   tHTML %>% add_editable_column(columns = -1:3)))

 #row_groups ok
 expect_true(grepl('id="tableHTML_row_groups" style="text-align:right;background-color:lightgray;" rowspan="12"><div contenteditable>',
                   tHTML %>% add_editable_column(columns = -1:3)))

 #column 1 ok
 expect_true(grepl('<td id="tableHTML_column_1"><div contenteditable>',
                   tHTML %>% add_editable_column(columns = -1:3)))

})


test_that("Function outputs the correct HTML with column names", {

  tHTML <- tableHTML(mtcars,
                     rownames = TRUE,
                     widths = c(150, 100, rep(50, 11)),
                     row_groups = list(c(10, 10, 12), c('Group 1', 'Group 2', 'Group 3'))) %>%
    add_css_column(css = list('background-color', 'lightgray'), columns = 'row_groups') %>%
    add_css_column(css = list('text-align', 'right'), columns = 'row_groups') %>%
    add_css_header(css = list('background-color', 'lightgray'), headers = 1)

  #rows ok
  expect_true(grepl('id="tableHTML_rownames"><div contenteditable>',
                    tHTML %>% add_editable_column(columns = c('row_groups', 'rownames', 'hp'))))

  #row_groups ok
  expect_true(grepl('id="tableHTML_row_groups" style="text-align:right;background-color:lightgray;" rowspan="12"><div contenteditable>',
                    tHTML %>% add_editable_column(columns = c('row_groups', 'rownames', 'hp'))))

  #column 1 ok
  expect_true(grepl('<td id="tableHTML_column_1"><div contenteditable>',
                    tHTML %>% add_editable_column(columns = c('row_groups', 'rownames', 'mpg'))))

})
