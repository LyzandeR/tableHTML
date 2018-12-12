context("to_hex_logo Testing")

test_that("Function fails for wrong inputs", {
  
  #invalid format
  expect_error(to_hex_logo(tableHTML(mtcars[1:23, ], class = '"hexagon inner"'),
                           format = 'doc'),
               "format should be")
  #invalid logo
  expect_error(to_hex_logo(mtcars),
               "logo needs to be")
  #invalid style in the input
  expect_error(to_hex_logo(tableHTML(mtcars)),
               "logo does not have")
})

test_that("output is of the right class", {
  # output is an tableHTML object
  expect_is(to_hex_logo(tableHTML(mtcars[1:23, ], class = '"hexagon inner"'), save =F), 
            'tableHTML')
})