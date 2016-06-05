context("add_css_header testing")

test_that("Function fails for wrong inputs", {
 #no tableHTML
 expect_error(add_css_header(mtcars, css = list('background-color', 'lightgray')),
              'tableHTML needs to be')
 #all checks ok
 expect_error(tableHTML(mtcars) %>% 
               add_css_header(css = list(c('background-color', 'border'), 
                                         c('lightgray', '3px solid green')),
                              headers = 1) , NA)
 #check css list has same lengths
 expect_error(tableHTML(mtcars) %>%
               add_css_header(css = list(c('background-color', 'height'), 'lightgray')),
              'same length')
})

test_that("css is added with add_css_header", {
 #find style+
 expect_true(
  grepl(
   '<th id="header_1" style="background-color:lightgray;height:30px;">',
   tableHTML(mtcars) %>%
    add_css_header(css = list(c('background-color', 'height'), c('lightgray', '30px')),
                   headers = 1)
  )
 )
 
 expect_true(
  grepl(
   '<th id="header_2" style="background-color:lightgray;height:30px;">',
   tableHTML(mtcars) %>%
    add_css_header(css = list(c('background-color', 'height'), c('lightgray', '30px')),
                   headers = 2)
  )
 )
 
})

test_that("css works fine with additional add_css_header of same style def", {
 
 expect_true(
  grepl(
   '<th id="header_1" style="background-color:red;background-color:lightgray;height:30px;">',
   tableHTML(mtcars) %>%
    add_css_header(css = list(c('background-color', 'height'), c('lightgray', '30px')),
                   headers = 1) %>%
    add_css_header(css = list('background-color', 'red'),
                   headers = 1)
  )
 )
 
 expect_true(
  grepl(
   '<th id="header_2" style="background-color:red;background-color:lightgray;height:30px;">',
   tableHTML(mtcars) %>%
    add_css_header(css = list(c('background-color', 'height'), c('lightgray', '30px')),
                   headers = 2) %>%
    add_css_header(css = list('background-color', 'red'),
                   headers = 2)
  )
 )
 
})

test_that("css works fine with additional add_css_header of different style def", {
 
 expect_true(
  grepl(
   '<th id="header_1" style="height:30px;background-color:lightgray;height:30px;">',
   tableHTML(mtcars) %>%
    add_css_header(css = list(c('background-color', 'height'), c('lightgray', '30px')),
                   headers = 1) %>%
    add_css_header(css = list('height', '30px'),
                   headers = 1)
  )
 )
 
 expect_true(
  grepl(
   '<th id="header_2" style="height:30px;background-color:lightgray;height:30px;">',
   tableHTML(mtcars) %>%
    add_css_header(css = list(c('background-color', 'height'), c('lightgray', '30px')),
                   headers = 2) %>%
    add_css_header(css = list('height', '30px'),
                   headers = 2)
  )
 )
 
})



