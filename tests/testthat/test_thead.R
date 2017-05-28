context("add_css_thead testing")

test_that("Function fails for wrong inputs", {
 #no tableHTML
 expect_error(add_css_thead(mtcars, css = list('background-color', 'lightgray')),
              'tableHTML needs to be')
 #all checks ok
 expect_error(tableHTML(mtcars) %>% 
               add_css_thead(css = list(c('background-color', 'border'), 
                                        c('lightgray', '3px solid green'))) 
              , NA)
 #check css list has same lengths
 expect_error(tableHTML(mtcars) %>%
               add_css_thead(css = list(c('background-color', 'height'), 'lightgray')),
              'same length')
})

test_that("css is added with add_css_thead", {
 #find style+
 expect_true(
  grepl('style="background-color:lightgray;',
        tableHTML(mtcars) %>%
         add_css_thead(css = list(c('background-color'), c('lightgray')))
  )
 )
 
 expect_true(
  grepl(
   'style="background-color:lightgray;height:30px;',
   tableHTML(mtcars) %>%
    add_css_thead(css = list(c('background-color', 'height'), c('lightgray', '30px')))
  )
 )
 
})

test_that("css works fine with additional add_css_thead of same style def", {
 
 expect_true(
  grepl(
   'style="background-color:red;background-color:lightgray;height:30px;',
   tableHTML(mtcars) %>%
    add_css_thead(css = list(c('background-color', 'height'), c('lightgray', '30px'))) %>%
    add_css_thead(css = list('background-color', 'red'))
  )
 )
 
 expect_true(
  grepl(
   'style="background-color:red;background-color:lightgray;height:30px;',
   tableHTML(mtcars) %>%
    add_css_thead(css = list(c('background-color', 'height'), c('lightgray', '30px'))) %>%
    add_css_thead(css = list('background-color', 'red'))
  )
 )
 
})

test_that("css works fine with additional add_css_thead of different style def", {
 
 expect_true(
  grepl(
   'style="height:30px;background-color:lightgray;height:30px;',
   tableHTML(mtcars) %>%
    add_css_thead(css = list(c('background-color', 'height'), c('lightgray', '30px'))) %>%
    add_css_thead(css = list('height', '30px'))
  )
 )
 
 expect_true(
  grepl(
   'style="height:30px;background-color:lightgray;height:30px;',
   tableHTML(mtcars) %>%
    add_css_thead(css = list(c('background-color', 'height'), c('lightgray', '30px'))) %>%
    add_css_thead(css = list('height', '30px'))
  )
 )
 
})

test_that("output has attribute", {
 expect_identical(
  attr(tableHTML(mtcars, headers = letters[1:11]) %>% 
        add_css_tbody(css = list('background-color', 'lightgreen')), 'headers'), 
  letters[1:11]
 )
 
})
