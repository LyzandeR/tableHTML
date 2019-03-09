context("create_logo testing")

test_that("Function fails for wrong inputs", {

  #invalid format
  expect_error(create_logo(format = 'doc'),
               "format should be")

 #invalid file extension
 expect_error(create_logo(format = 'png', file = 'logo_pic.jpeg'),
              "file extension should be")
})

test_that("Output is of the right class and style", {
  # find class hexagon inner
  expect_match(create_logo(save = FALSE),
               'class="hexagon inner"' )
  expect_equal(attributes(create_logo(save = FALSE))$table_class,
               '"hexagon inner"' )
  expect_is(create_logo(save = FALSE), 'tableHTML')
})

test_that("The number of colored cells is the same as the number of letters in ",{
  # for each letter the color should exist in 5 locations (4 borders and background)
  # plus the background of the second header
  logo <- create_logo(save = FALSE)
  cls <- attributes(logo)$colors
  expect_equal({
    c(
      sum(grepl(cls[1], unlist(strsplit(logo, ';')))),
      sum(grepl(cls[2], unlist(strsplit(logo, ';')))),
      sum(grepl(cls[3], unlist(strsplit(logo, ';')))),
      sum(grepl(cls[4], unlist(strsplit(logo, ';')))),
      sum(grepl(cls[5], unlist(strsplit(logo, ';')))),
      sum(grepl(cls[6], unlist(strsplit(logo, ';')))),
      sum(grepl(cls[7], unlist(strsplit(logo, ';')))),
      sum(grepl(cls[8], unlist(strsplit(logo, ';')))),
      sum(grepl(cls[9], unlist(strsplit(logo, ';'))))
    )
  }, {
    c(
      attributes(logo)$letters_cnt['t'] * 5 + 1,
      attributes(logo)$letters_cnt['a'] * 5 + 1,
      attributes(logo)$letters_cnt['b'] * 5 + 1,
      attributes(logo)$letters_cnt['l'] * 5 + 1,
      attributes(logo)$letters_cnt['e'] * 5 + 1,
      attributes(logo)$letters_cnt['H'] * 5 + 1,
      attributes(logo)$letters_cnt['T'] * 5 + 1,
      attributes(logo)$letters_cnt['M'] * 5 + 1,
      attributes(logo)$letters_cnt['L'] * 5 + 1
    ) %>% unname()
  })
})

test_that("The number of headers is correct and they're identical", {
  logo <- create_logo(save = FALSE)
  expect_equal({
    trimws(attributes(logo)$headers)
  },
  rep('..', attributes(logo)$ncol)
  )
})

test_that('logo is saved when save=TRUE', {
 # when format=html
 expect_true({
  create_logo(file='logo.html')
  out <- file.size('logo.html') > 1
  file.remove('logo.html')
  out
 })
 # when format!=html
 expect_true({
  create_logo(format = 'png', file='logo.png')
  out <- file.size('logo.png') > 1
  file.remove('logo.png')
  out
 })
})

