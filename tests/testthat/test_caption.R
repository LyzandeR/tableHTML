context("add_css_caption testing")

test_that("Function fails for wrong inputs", {
 #no tableHTML
 expect_error(add_css_caption(mtcars, css = list('background-color', 'lightgray')),
              'tableHTML needs to be')
 #all checks ok
 expect_error(tableHTML(mtcars) %>% 
               add_css_caption(css = list(c('background-color', 'border'), 
                                         c('lightgray', '3px solid green'))) 
              , NA)
 #check css list has same lengths
 expect_error(tableHTML(mtcars) %>%
               add_css_caption(css = list(c('background-color', 'height'), 'lightgray')),
              'same length')
})

test_that("css is added with add_css_caption", {
 #find style+
 expect_true(
  grepl(
   '<caption style="background-color:lightgray;height:30px;">',
   tableHTML(mtcars, caption = 'This is a caption') %>%
    add_css_caption(css = list(c('background-color', 'height'), c('lightgray', '30px')))
  )
 )
 
 expect_true(
  grepl(
   '<caption style="background-color:lightgray;height:30px;">',
   tableHTML(mtcars, caption = 'This is a caption') %>%
    add_css_caption(css = list(c('background-color', 'height'), c('lightgray', '30px')))
  )
 )
 
})

test_that("css works fine with additional add_css_header of same style def", {
 
 expect_true(
  grepl(
   '<caption style="background-color:red;background-color:lightgray;height:30px;">',
   tableHTML(mtcars, caption = 'This is a caption') %>%
    add_css_caption(css = list(c('background-color', 'height'), c('lightgray', '30px'))) %>%
    add_css_caption(css = list('background-color', 'red'))
  )
 )
 
 expect_true(
  grepl(
   '<caption style="background-color:red;background-color:lightgray;height:30px;">',
   tableHTML(mtcars, caption = 'This is a caption') %>%
    add_css_caption(css = list(c('background-color', 'height'), c('lightgray', '30px'))) %>%
    add_css_caption(css = list('background-color', 'red'))
  )
 )
 
})

test_that("css works fine with additional add_css_header of different style def", {
 
 expect_true(
  grepl(
   '<caption style="height:30px;background-color:lightgray;height:30px;">',
   tableHTML(mtcars , caption = 'This is a caption') %>%
    add_css_caption(css = list(c('background-color', 'height'), c('lightgray', '30px'))) %>%
    add_css_caption(css = list('height', '30px'))
  )
 )
 
 expect_true(
  grepl(
   '<caption style="height:30px;background-color:lightgray;height:30px;">',
   tableHTML(mtcars, caption = 'This is a caption') %>%
    add_css_caption(css = list(c('background-color', 'height'), c('lightgray', '30px'))) %>%
    add_css_caption(css = list('height', '30px'))
  )
 )
 
})



