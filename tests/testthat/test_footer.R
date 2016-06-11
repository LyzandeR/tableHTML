context("add_css_footer testing")

test_that("Function fails for wrong inputs", {
 #no tableHTML
 expect_error(add_css_footer(mtcars, css = list('background-color', 'lightgray')),
              'tableHTML needs to be')
 #all checks ok
 expect_error(tableHTML(mtcars, footer = 'This is a footer') %>% 
               add_css_footer(css = list(c('background-color', 'border'), 
                                         c('lightgray', '3px solid green'))) 
              , NA)
 #check css list has same lengths
 expect_error(tableHTML(mtcars, footer = 'This is a footer') %>%
              add_css_footer(css = list(c('background-color', 'height'), 'lightgray')),
              'same length')
})

test_that("css is added with add_css_footer", {
 #find style+
 expect_true(
  grepl(
   '<caption id="footer" align="bottom" style="background-color:lightgray;height:30px;">',
   tableHTML(mtcars, footer = 'This is a caption') %>%
    add_css_footer(css = list(c('background-color', 'height'), c('lightgray', '30px')))
  )
 )
 
 expect_true(
  grepl(
   '<caption id="footer" align="bottom" style="background-color:lightgray;height:30px;">',
   tableHTML(mtcars, footer = 'This is a footer') %>%
    add_css_footer(css = list(c('background-color', 'height'), c('lightgray', '30px')))
  )
 )
 
})

test_that("css works fine with additional add_css_footer of same style def", {
 
 expect_true(
  grepl(
   '<caption id="footer" align="bottom" style="background-color:red;background-color:lightgray;height:30px;">',
   tableHTML(mtcars, footer = 'This is a footer') %>%
    add_css_footer(css = list(c('background-color', 'height'), c('lightgray', '30px'))) %>%
    add_css_footer(css = list('background-color', 'red'))
  )
 )
 
 expect_true(
  grepl(
   '<caption id="footer" align="bottom" style="background-color:red;background-color:lightgray;height:30px;">',
   tableHTML(mtcars, footer = 'This is a footer') %>%
    add_css_footer(css = list(c('background-color', 'height'), c('lightgray', '30px'))) %>%
    add_css_footer(css = list('background-color', 'red'))
  )
 )
 
})

test_that("css works fine with additional add_css_footer of different style def", {
 
 expect_true(
  grepl(
   '<caption id="footer" align="bottom" style="height:30px;background-color:lightgray;height:30px;">',
   tableHTML(mtcars , footer = 'This is a caption') %>%
    add_css_footer(css = list(c('background-color', 'height'), c('lightgray', '30px'))) %>%
    add_css_footer(css = list('height', '30px'))
  )
 )
 
 expect_true(
  grepl(
   '<caption id="footer" align="bottom" style="height:30px;background-color:lightgray;height:30px;">',
   tableHTML(mtcars, footer = 'This is a caption') %>%
    add_css_footer(css = list(c('background-color', 'height'), c('lightgray', '30px'))) %>%
    add_css_footer(css = list('height', '30px'))
  )
 )
 
})



