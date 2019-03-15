context("create_hexlogo testing")

test_that("Function fails for wrong inputs", {
 skip_on_cran()
 #invalid format
 expect_error(create_hexlogo(format = 'doc'),
              "format should be")
 #invalid file extension
 expect_error(create_hexlogo(format = 'png', file = 'hexlogo_pic.jpeg'),
              "file extension should be")
})

test_that("Output is of the right class", {
 skip_on_cran()
 # output is a tableHTML object
 expect_is(create_hexlogo(save =FALSE), 'tableHTML')
})

test_that('css style is well defined', {
 skip_on_cran()
 hex_logo <- create_hexlogo(save=FALSE)
 # extract the style from the logo, which should be defined on top
 css_style <- strsplit(hex_logo, '>')[[1]][1:2] %>%
  strsplit('\\}') %>% unlist() %>%
  strsplit('\\{') %>% unlist()

 # check that the style starts and ends with the right tag
 expect_equal(c(grep('<style', css_style), grep('</style', css_style)),
              c(1, length(css_style)))

 # check the hexagon and hexagon inner classes
 expect_equal(sum(grepl('.hexagon', css_style)),
              attributes(hex_logo)$css_style$selectors_cnt)
 expect_equal(sum(grepl('.hexagon.inner', css_style)), 3)

 # check the number of css defnitions
 # this should equal the length of the list of css definitions
 # for each selector (+ 1 for '\n' the new line chatacter at the end)
 expect_equal(css_style[!grepl('.hexagon', css_style) & ! grepl('style', css_style)] %>%
               strsplit(';') %>% lengths(),
              attributes(hex_logo)$css_style$definitions_cnt + 1)
})

test_that('logo is saved when save=TRUE', {
 skip_on_cran()
 # when format=html
 expect_true({
  create_hexlogo(file='logo.html')
  out <- file.size('logo.html') > 1
  file.remove('logo.html')
  out
 })
 # when format!=html
 expect_true({
  create_hexlogo(format = 'png', file='logo.png')
  out <- file.size('logo.png') > 1
  file.remove('logo.png')
  out
 })
})

