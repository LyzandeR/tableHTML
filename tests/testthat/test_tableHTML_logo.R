context("tableHTML_logo Testing")

test_that("Function fails for wrong inputs", {
  
  #invalid format
  expect_error(tableHTML_logo(format = 'doc'),
               "format should be")
})

test_that("output is of the right class and style", {
  # find class hexagon inner
  expect_match(tableHTML_logo(save = F),
               'class="hexagon inner"' )
  expect_is(tableHTML_logo(save = F), 'tableHTML')
})


