context("add_css_column testing")

test_that("Function fails for wrong inputs", {
 #no tableHTML
 expect_error(add_css_column(mtcars, css = list('background-color', 'lightgray')),
              'tableHTML needs to be')
 #all checks ok
 expect_error(tableHTML(mtcars) %>% 
               add_css_column(css = list(c('background-color', 'border'), 
                                      c('lightgray', '3px solid green')),
                              columns = 'mpg') , NA)
 #check deprecated
 expect_error(tableHTML(mtcars) %>% 
               add_css_column(css = list(c('background-color', 'border'), 
                                         c('lightgray', '3px solid green')),
                              column_names = 'mpg') , 'deprecated')
 #check css list has same lengths
 expect_error(tableHTML(mtcars) %>%
               add_css_column(css = list(c('background-color', 'height'), 'lightgray')),
              'same length')
})

test_that("css is added with add_css_column", {
 #find style+
 expect_true(
  grepl(
   '<td id="tableHTML_column_1" style="background-color:lightgray;height:30px;">',
   tableHTML(mtcars) %>%
    add_css_column(css = list(c('background-color', 'height'), c('lightgray', '30px')),
                   columns = 'mpg')
  )
 )
 
 expect_true(
  grepl(
   '<td id="tableHTML_column_2" style="background-color:lightgray;height:30px;">',
   tableHTML(mtcars) %>%
    add_css_column(css = list(c('background-color', 'height'), c('lightgray', '30px')),
                   columns = 'cyl')
  )
 )
 
})

test_that("css works fine with additional add_css_column of same style def", {
 
 expect_true(
  grepl(
   '<td id="tableHTML_column_2" style="background-color:red;background-color:lightgray;height:30px;"',
   tableHTML(mtcars) %>%
    add_css_column(css = list(c('background-color', 'height'), c('lightgray', '30px')),
                   columns = 'cyl') %>%
    add_css_column(css = list('background-color', 'red'),
                   columns = 'cyl')
  )
 )
 
 expect_true(
  grepl(
   '<td id="tableHTML_column_1" style="background-color:red;background-color:lightgray;height:30px;"',
   tableHTML(mtcars) %>%
    add_css_column(css = list(c('background-color', 'height'), c('lightgray', '30px')),
                   columns = 'mpg') %>%
    add_css_column(css = list('background-color', 'red'),
                   columns = 'mpg')
  )
 )
 
})

test_that("css works fine with additional add_css_column of different style def", {
 
 expect_true(
  grepl(
   '<td id="tableHTML_column_1" style="height:30px;background-color:lightgray;height:30px;"',
   tableHTML(mtcars) %>%
    add_css_column(css = list(c('background-color', 'height'), c('lightgray', '30px')),
                   columns = 'mpg') %>%
    add_css_column(css = list('height', '30px'),
                   columns = 'mpg')
  )
 )
 
 expect_true(
  grepl(
   '<td id="tableHTML_column_2" style="height:30px;background-color:lightgray;height:30px;"',
   tableHTML(mtcars) %>%
    add_css_column(css = list(c('background-color', 'height'), c('lightgray', '30px')),
                   columns = 'cyl') %>%
    add_css_column(css = list('height', '30px'),
                   columns = 'cyl')
  )
 )
 
})



