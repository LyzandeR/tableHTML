context("make_css_colour_rank_theme testing")

test_that("Function fails for wrong inputs", {
  #column_data not a list
  expect_error(make_css_colour_rank_theme(mtcars$mpg, colors = c("#1FFF6F")),
               'column_data must be a named list')
  
  #column_data not a named list
  expect_error(make_css_colour_rank_theme(list(mtcars$mpg), colors = c("#1FFF6F")),
               'column_data must be a named list')
  
  # colors argument not valid 
  expect_error(make_css_colour_rank_theme(list(mpg = mtcars$mpg), colors = c("#FFF6F")),
               'colors argument not valid.')
  
  #all checks ok
  expect_error(make_css_colour_rank_theme(list(mpg = mtcars$mpg), colors = c("#1FFF6F")),
               NA)
  
})

test_that("Test returned css ", {
  
  #check class
  expect_equal(class(make_css_colour_rank_theme(list(mpg = mtcars$mpg),
                                                colors = c("#1FFF6F"))),
               "list")
  #check names
  expect_equal(names(make_css_colour_rank_theme(list(mpg = mtcars$mpg),
                                                colors = c("#1FFF6F"))),
               "mpg")
  
  #check css property
  expect_equal(unique(make_css_colour_rank_theme(list(mpg = mtcars$mpg), 
                                                 colors = c("#1FFF6F"))$mpg[[1]][[1]]),
               "background-color")
  
  #check css property values
  expect_equal(unique(make_css_colour_rank_theme(list(mpg = mtcars$mpg), 
                                                 colors = c("#1FFF6F"))$mpg[[2]][[1]]),
               "#1FFF6F")
  
})

