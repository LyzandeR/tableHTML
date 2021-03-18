context("add_css_rows_in_column testing")

test_that("Function fails for wrong inputs", {
  #no tableHTML
  expect_error(add_css_rows_in_column(mtcars, css = list('background-color', 
                                                         rep(c('red', 'green'), each = 16))),
               'tableHTML needs to be')
  #all checks ok
  expect_error(add_css_rows_in_column(tableHTML(mtcars), css = list('background-color', 
                                                 rep(c('red', 'green'), each = 16)),
                                      column = 'mpg') , NA)
  
  #check css list has same lengths
  expect_error(tableHTML(mtcars) %>%
                 add_css_rows_in_column(css = list('background-color', 'red'),
               'the values of'))
  
  #check column exists in data
  expect_error(add_css_rows_in_column(tableHTML(mtcars), 
                                      css = list('background-color', 
                                                  rep(c('red', 'green'), each = 16)),
                                      column = 'abc'),
               'column not found')
    
  expect_error(add_css_rows_in_column(tableHTML(mtcars, headers = letters[1:11]), 
                                      css = list('background-color', 
                                                   rep(c('red', 'green'), each = 16)),
                                      column = 'a'),
                 NA)
})

test_that("css is added with add_css_rows_in_column", {
  #find style+
  expect_true(
    grepl(
      '<td id="tableHTML_column_1" style="background-color:lightgray;">',
      tableHTML(mtcars) %>%
        add_css_rows_in_column(css = list('background-color', rep('lightgray', 32)),
                               column = 'mpg')
    )
  )
  
  expect_true(
    grepl(
      '<td id="tableHTML_column_2" style="background-color:lightgray;">',
      tableHTML(mtcars) %>%
        add_css_rows_in_column(css = list('background-color', rep('lightgray', 32)),
                               column = 'cyl')
    )
  )
  
})

test_that("css works fine with add_css_column", {
  
  expect_true(
    grepl(
      '<td id="tableHTML_column_1" style="border:3px solid blue;background-color:lightgray;">',
      tableHTML(mtcars) %>%
        add_css_rows_in_column(css = list('background-color', rep('lightgray', 32)),
                               column = 'mpg') %>%
        add_css_column(css = list('border', '3px solid blue'),
                       columns = c('mpg', 'disp', 'rownames'))
    )
  )
  
  expect_true(
    grepl(
      '<td id="tableHTML_column_1" style="background-color:lightgray;border:3px solid blue;">',
      tableHTML(mtcars) %>%
        add_css_column(css = list('border', '3px solid blue'),
                       columns = c('mpg', 'disp', 'rownames')) %>%
        add_css_rows_in_column(css = list('background-color', rep('lightgray', 32)),
                               column = 'mpg') 

    )
  )
  
})


test_that("output has attribute", {
  expect_identical(
    attr(tableHTML(mtcars, headers = letters[1:11]) %>%
           add_css_rows_in_column(css = list('background-color', rep('lightgray', 32)),
                                  column = 'a'), 'headers'),
    letters[1:11]
  )
  
})

test_that("numeric index, character index and column name work fine", {
  
  expect_true(
    grepl(
      '<td id="tableHTML_column_1" style="background-color:lightgray;">',
      tableHTML(mtcars) %>%
        add_css_rows_in_column(css = list('background-color', rep('lightgray', 32)),
                               column = 'mpg')
    )
  )
  
  expect_true(
    grepl(
      '<td id="tableHTML_rownames" style="background-color:lightgray;"',
      tableHTML(mtcars) %>%
        add_css_rows_in_column(css = list('background-color', rep('lightgray', 32)),
                               column = 'rownames')
    )
  )
  
  expect_true(
    grepl(
      '<td id="tableHTML_column_1" style="background-color:lightgray;"',
      tableHTML(mtcars) %>%
        add_css_rows_in_column(css = list('background-color', rep('lightgray', 32)),
                               column = '1')
    )
  )
  
  expect_true(
    grepl(
      '<td id="tableHTML_column_1" style="background-color:lightgray;"',
      tableHTML(mtcars) %>%
        add_css_rows_in_column(css = list('background-color', rep('lightgray', 32)),
                               column = 1)
    )
  )
  
})

