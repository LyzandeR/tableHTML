context("add_css_row testing")

test_that("Function fails for wrong inputs", {
 #no tableHTML
 expect_error(add_css_row(mtcars, css = list('background-color', 'lightgray')),
              'tableHTML needs to be')
 #all checks ok
 expect_error(tableHTML(mtcars) %>% 
                add_css_row(css = list(c('background-color', 'border'), 
                                       c('lightgray', '3px solid green'))) , NA)
 #check css list has same lengths
 expect_error(tableHTML(mtcars) %>%
                add_css_row(css = list(c('background-color', 'height'), 'lightgray')),
             'same length')
})

test_that("css is added with add_css_row", {
 #find style+
 expect_true(
  grepl(
    '<tr style="background-color:lightgray;height:30px;">',
    tableHTML(mtcars) %>%
     add_css_row(css = list(c('background-color', 'height'), c('lightgray', '30px')))
  )
 )
 
 expect_identical(
  length(gregexpr(
   '<tr style="background-color:lightgray;height:30px;">',
   tableHTML(mtcars) %>%
    add_css_row(css = list(c('background-color', 'height'), c('lightgray', '30px')))
  )[[1]]), 33L
 )
})
 
test_that("css works fine with additional add_css_row of same style def", {

 expect_true(
  grepl(
   '<tr style="background-color:red;background-color:lightgray;height:30px;">',
   tableHTML(mtcars) %>%
    add_css_row(css = list(c('background-color', 'height'), c('lightgray', '30px'))) %>%
    add_css_row(css = list('background-color', 'red'))
  )
 )
 
 expect_identical(
  length(gregexpr(
   '<tr style="background-color:red;background-color:lightgray;height:30px;">',
   tableHTML(mtcars) %>%
    add_css_row(css = list(c('background-color', 'height'), c('lightgray', '30px'))) %>%
    add_css_row(css = list('background-color', 'red'))
  )[[1]]), 33L
 )

})

test_that("css works fine with additional add_css_row of different style def", {
 
 expect_true(
  grepl(
   '<tr style="height:30px;background-color:lightgray;height:30px;">',
   tableHTML(mtcars) %>%
    add_css_row(css = list(c('background-color', 'height'), c('lightgray', '30px'))) %>%
    add_css_row(css = list('height', '30px'))
  )
 )
 
 expect_identical(
  length(gregexpr(
   '<tr style="height:30px;background-color:lightgray;height:30px;">',
   tableHTML(mtcars) %>%
    add_css_row(css = list(c('background-color', 'height'), c('lightgray', '30px'))) %>%
    add_css_row(css = list('height', '30px'))
  )[[1]]), 33L
 )
 
})



