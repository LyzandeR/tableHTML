context("make_css testing")

test_that("Function fails for wrong inputs", {
 
 #no tableHTML
 expect_error(make_css(list('table', c('text-align', 'font-size'), c('center', '20px')),
                       list('th', c('background-color'), c('lightgreen', '30px'))),
              'The second and third elements')
 
 expect_error(make_css('a string'), 'Each element in ... needs')
 
 expect_error(make_css(list('table', c('text-align', 'font-size'), c('center', '20px')),
                       list('another list')),
              'Each element in ... needs')
}) 
 
test_that("Function creates the css components", {
 
 expect_true(
  grepl('text-align: center;',
        make_css(list(c('table', 'td'), c('text-align', 'font-size'), c('center', '20px')),
                 list('th', c('background-color', 'height'), c('lightgreen', '30px'))))
  )
 
 expect_true(
  grepl('table,\ntd',
        make_css(list(c('table', 'td'), c('text-align', 'font-size'), c('center', '20px')),
                 list('th', c('background-color', 'height'), c('lightgreen', '30px'))))
 )
 
 expect_true(
  grepl('height: 30px;',
        make_css(list(c('table', 'td'), c('text-align', 'font-size'), c('center', '20px')),
                 list('th', c('background-color', 'height'), c('lightgreen', '30px'))))
 )
 
})