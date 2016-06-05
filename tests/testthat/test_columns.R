context("add_css_column testing")

test_that("Function fails for wrong inputs", {
 #no tableHTML
 expect_error(add_css_column(mtcars, css = list('background-color', 'lightgray')),
              'tableHTML needs to be')
 #all checks ok
 expect_error(tableHTML(mtcars) %>% 
               add_css_column(css = list(c('background-color', 'border'), 
                                      c('lightgray', '3px solid green')),
                              column_names = 'mpg') , NA)
 #check css list has same lengths
 expect_error(tableHTML(mtcars) %>%
               add_css_column(css = list(c('background-color', 'height'), 'lightgray')),
              'same length')
})

test_that("css is added with add_css_column", {
 #find style+
 expect_true(
  grepl(
   '<td id="mpg" style="background-color:lightgray;height:30px;">',
   tableHTML(mtcars) %>%
    add_css_column(css = list(c('background-color', 'height'), c('lightgray', '30px')),
                   column_names = 'mpg')
  )
 )
 
 expect_true(
  grepl(
   '<td id="cyl" style="background-color:lightgray;height:30px;">',
   tableHTML(mtcars) %>%
    add_css_column(css = list(c('background-color', 'height'), c('lightgray', '30px')),
                   column_names = 'cyl')
  )
 )
 
})

test_that("css works fine with additional add_css_column of same style def", {
 
 expect_true(
  grepl(
   '<td id="cyl" style="background-color:red;background-color:lightgray;height:30px;"',
   tableHTML(mtcars) %>%
    add_css_column(css = list(c('background-color', 'height'), c('lightgray', '30px')),
                   column_names = 'cyl') %>%
    add_css_column(css = list('background-color', 'red'),
                   column_names = 'cyl')
  )
 )
 
 expect_true(
  grepl(
   '<td id="mpg" style="background-color:red;background-color:lightgray;height:30px;"',
   tableHTML(mtcars) %>%
    add_css_column(css = list(c('background-color', 'height'), c('lightgray', '30px')),
                   column_names = 'mpg') %>%
    add_css_column(css = list('background-color', 'red'),
                   column_names = 'mpg')
  )
 )
 
})

test_that("css works fine with additional add_css_column of different style def", {
 
 expect_true(
  grepl(
   '<td id="mpg" style="height:30px;background-color:lightgray;height:30px;"',
   tableHTML(mtcars) %>%
    add_css_column(css = list(c('background-color', 'height'), c('lightgray', '30px')),
                   column_names = 'mpg') %>%
    add_css_column(css = list('height', '30px'),
                   column_names = 'mpg')
  )
 )
 
 expect_true(
  grepl(
   '<td id="cyl" style="height:30px;background-color:lightgray;height:30px;"',
   tableHTML(mtcars) %>%
    add_css_column(css = list(c('background-color', 'height'), c('lightgray', '30px')),
                   column_names = 'cyl') %>%
    add_css_column(css = list('height', '30px'),
                   column_names = 'cyl')
  )
 )
 
})



