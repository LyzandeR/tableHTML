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

test_that("colour to value mapping is correct", {
 expect_equal(
  {
   # column a, decreasing TRUE
   css <- make_css_colour_rank_theme(list(a =  rep(c(1:4, 10), 2),
                                          b = 1:10),
                                     c("#86c183", "#B9D48A", "#FCEC92", "#EFAE7F","#F8696B"),
                                     decreasing = TRUE,
                                     same_scale = FALSE)
   css[["a"]][[2]][[1]]

  },
  expected = c("#F8696B", "#F48773", "#F0A67C", "#F3C285", "#86C183",
               "#F8696B", "#F48773", "#F0A67C", "#F3C285", "#86C183")
  )
 expect_equal(
  {
   # column b, decreasing TRUE
   css <- make_css_colour_rank_theme(list(a =  rep(c(1:4, 10), 2),
                                          b = 1:10),
                                     c("#86c183", "#B9D48A", "#FCEC92", "#EFAE7F","#F8696B"),
                                     decreasing = TRUE,
                                     same_scale = FALSE)
   css[["b"]][[2]][[1]]

  },
  expected = c("#F8696B", "#F48773", "#F0A67C", "#F3C285", "#F9DE8D",
               "#EDE690", "#CFDC8C", "#B3D189", "#9CC986", "#86C183")
  )
 expect_equal(
  {
   # column a, decreasing FALSE
   css <- make_css_colour_rank_theme(list(a =  rep(c(1:4, 10), 2),
                                          b = 1:10),
                                     c("#86c183", "#B9D48A", "#FCEC92", "#EFAE7F","#F8696B"),
                                     decreasing = FALSE,
                                     same_scale = FALSE)
   css[["a"]][[2]][[1]]

  },
  expected = c("#86C183", "#9CC986", "#B3D189", "#CFDC8C", "#F8696B",
               "#86C183", "#9CC986", "#B3D189", "#CFDC8C", "#F8696B")
 )
 expect_equal(
  {
   # column a, decreasing TRUE, negtive value
   css <- make_css_colour_rank_theme(list(a =  rep(c(1:4, -10), 2),
                                          b = 1:10),
                                     c("#86c183", "#B9D48A", "#FCEC92", "#EFAE7F","#F8696B"),
                                     decreasing = FALSE,
                                     same_scale = FALSE)
   css[["a"]][[2]][[1]]

  },
  expected = c("#F0A47C", "#F29076", "#F57C70", "#F8696B", "#86C183",
               "#F0A47C", "#F29076", "#F57C70", "#F8696B", "#86C183")
 )
 expect_equal(
  {
   # column a, decreasing TRUE, negtive value, same_scale TRUE
   css <- make_css_colour_rank_theme(list(a =  rep(c(1:4, -10), 2),
                                          b = 1:10),
                                     c("#86c183", "#B9D48A", "#FCEC92", "#EFAE7F","#F8696B"),
                                     decreasing = FALSE,
                                     same_scale = TRUE)
   css[["a"]][[2]][[1]]

  },
  expected = c("#F9DF8E", "#F6D38A", "#F4C686", "#F1BA82", "#86C183",
               "#F9DF8E", "#F6D38A", "#F4C686", "#F1BA82", "#86C183")
 )
 expect_equal(
  {
   # column b, decreasing TRUE, negtive value, same_scale TRUE
   css <- make_css_colour_rank_theme(list(a =  rep(c(1:4, -10), 2),
                                          b = 1:10),
                                     c("#86c183", "#B9D48A", "#FCEC92", "#EFAE7F","#F8696B"),
                                     decreasing = FALSE,
                                     same_scale = TRUE)
   css[["b"]][[2]][[1]]

  },
  expected = c("#F9DF8E", "#F6D38A", "#F4C686", "#F1BA82", "#EFAE7F",
               "#F0A07B", "#F29277", "#F48473", "#F6766F", "#F8696B")
 )
 expect_equal(
  {
   # factor columns
   css <- make_css_colour_rank_theme(list(a =  factor(letters[1:4])),
                                     c("#86c183", "#B9D48A", "#FCEC92", "#EFAE7F","#F8696B"),
                                     decreasing = FALSE)
   css[["a"]][[2]][[1]]

  },
  expected = c("#86C183", "#CFDC8C", "#F3C285", "#F8696B")
 )
 expect_equal(
  {
   # factor columns
   css <- make_css_colour_rank_theme(list(a =  letters[1:4]),
                                     c("#86c183", "#B9D48A", "#FCEC92", "#EFAE7F","#F8696B"),
                                     decreasing = TRUE)
   css[["a"]][[2]][[1]]

  },
  expected = c("#F8696B","#F3C285", "#CFDC8C", "#86C183")
 )
 }
)

