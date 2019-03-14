context("colorize theme testing")

test_that("Function fails for wrong inputs", {

  # default and add_theme work fine
  expect_error(mtcars %>%
                 tableHTML() %>%
                 add_theme('colorize'),
               NA)
  expect_error(mtcars %>%
                 tableHTML() %>%
                 add_theme_colorize(),
               NA)
  # wrong input
  expect_error(mtcars %>% add_theme_colorize(),
               'tableHTML needs to be')
  expect_error(tableHTML(mtcars) %>%
                 add_theme_colorize(total_rows = c('1', '2', '3')),
               'total_rows should be either')
  expect_error(tableHTML(mtcars) %>%
                 add_theme_colorize(color = c('red', 'green', 'blue')),
               'color should be a vector')
  expect_error(tableHTML(mtcars) %>%
                 add_theme_colorize(id_column = NA),
               'id_column should be')
})

test_that("color distribution is fine", {

 # Default. one color, no total row, and with rownames.
 expect_equal({
  tab <- tableHTML(mtcars) %>%
   add_theme_colorize()
  rgb_col <- col2rgb(attributes(tab)$theme$colors)

  color_full <-  paste0('rgba\\(', paste0(rgb_col[, 1], collapse = ','), ',1\\)')
  header_background <- paste0('rgba\\(', paste0(rgb_col[, 1], collapse = ','), ',0.7\\)')
  background_color_1 <- paste0('rgba\\(', paste0(rgb_col[, 1], collapse = ','), ',0.3\\)')
  background_color_2 <- paste0('rgba\\(', paste0(rgb_col[, 2], collapse = ','), ',0.1\\)')

  c(# total rows background (no total rows)
   sum(grepl(paste0('background:', color_full), strsplit(tab, '<tr') %>% unlist)),
   # header background (number of rows for the row names, plus one header)
   sum(grepl(paste0('background:', header_background), strsplit(tab, '<tr') %>% unlist)),
   # sum of rows with color1 and rows with color2 (number of rows, plus one header)
   sum(grepl(paste0('background:', background_color_1), strsplit(tab, '<tr') %>% unlist)) +
    sum(grepl(paste0('background:', background_color_2), strsplit(tab, '<tr') %>% unlist)),
   # inner borders (number of rows, plus one header)
   sum(grepl(paste0('border-top:3px solid ', color_full), strsplit(tab, '<tr') %>% unlist)),
   # outer borders (one time)
   sum(grepl(paste0('border:3px solid ', color_full), strsplit(tab, '<tr') %>% unlist)))
 },
 c(0,
   attributes(tab)$nrows + 1,
   attributes(tab)$nrows + 1,
   attributes(tab)$nrows + 1,
   1)
 )

 # One color, no total row, and without rownames.
 expect_equal({
  tab <- tableHTML(mtcars, rownames = FALSE) %>%
   add_theme_colorize()
  rgb_col <- col2rgb(attributes(tab)$theme$colors)

  color_full <-  paste0('rgba\\(', paste0(rgb_col[, 1], collapse = ','), ',1\\)')
  header_background <- paste0('rgba\\(', paste0(rgb_col[, 1], collapse = ','), ',0.7\\)')
  background_color_1 <- paste0('rgba\\(', paste0(rgb_col[, 1], collapse = ','), ',0.3\\)')
  background_color_2 <- paste0('rgba\\(', paste0(rgb_col[, 2], collapse = ','), ',0.1\\)')

  c(# total rows background (no total rows)
   sum(grepl(paste0('background:', color_full), strsplit(tab, '<tr') %>% unlist)),
   # header background
   sum(grepl(paste0('background:', header_background), strsplit(tab, '<tr') %>% unlist)),
   # sum of rows with color1 and rows with color2
   sum(grepl(paste0('background:', background_color_1), strsplit(tab, '<tr') %>% unlist)) +
    sum(grepl(paste0('background:', background_color_2), strsplit(tab, '<tr') %>% unlist)),
   # inner borders
   sum(grepl(paste0('border-top:3px solid ', color_full), strsplit(tab, '<tr') %>% unlist)),
   # outer borders
   sum(grepl(paste0('border:3px solid ', color_full), strsplit(tab, '<tr') %>% unlist)))
 },
 c(0,
   1,
   attributes(tab)$nrows + 1,
   attributes(tab)$nrows + 1,
   1)
 )

  # One color, one total row, and with rownames.
  expect_equal({
    tab <- tableHTML(mtcars) %>%
      add_theme_colorize(total_rows = nrow(mtcars))
    rgb_col <- col2rgb(attributes(tab)$theme$colors)

    color_full <-  paste0('rgba\\(', paste0(rgb_col[, 1], collapse = ','), ',1\\)')
    header_background <- paste0('rgba\\(', paste0(rgb_col[, 1], collapse = ','), ',0.7\\)')
    background_color_1 <- paste0('rgba\\(', paste0(rgb_col[, 1], collapse = ','), ',0.3\\)')
    background_color_2 <- paste0('rgba\\(', paste0(rgb_col[, 2], collapse = ','), ',0.1\\)')

    c(# total rows background
      sum(grepl(paste0('background:', color_full), strsplit(tab, '<tr') %>% unlist)),
      # header background (number of rows for the row names, plus one header)
      sum(grepl(paste0('background:', header_background), strsplit(tab, '<tr') %>% unlist)),
      # sum of rows with color1 and rows with color2 (number of rows)
      sum(grepl(paste0('background:', background_color_1), strsplit(tab, '<tr') %>% unlist)) +
      sum(grepl(paste0('background:', background_color_2), strsplit(tab, '<tr') %>% unlist)),
      # inner borders (number of rows)
      sum(grepl(paste0('border-top:3px solid ', color_full), strsplit(tab, '<tr') %>% unlist)),
      # outer borders (one time)
      sum(grepl(paste0('border:3px solid ', color_full), strsplit(tab, '<tr') %>% unlist)))
  },
  c(1,
    attributes(tab)$nrows + 1,
    attributes(tab)$nrows,
    attributes(tab)$nrows,
    1)
  )
  # One color, one total row, and without rownames.
  expect_equal({
    tab <- tableHTML(mtcars, rownames = FALSE) %>%
      add_theme_colorize(total_rows = nrow(mtcars))
    rgb_col <- col2rgb(attributes(tab)$theme$colors)

    color_full <-  paste0('rgba\\(', paste0(rgb_col[, 1], collapse = ','), ',1\\)')
    header_background <- paste0('rgba\\(', paste0(rgb_col[, 1], collapse = ','), ',0.7\\)')
    background_color_1 <- paste0('rgba\\(', paste0(rgb_col[, 1], collapse = ','), ',0.3\\)')
    background_color_2 <- paste0('rgba\\(', paste0(rgb_col[, 2], collapse = ','), ',0.1\\)')

    c(# total rows background
      sum(grepl(paste0('background:', color_full), strsplit(tab, '<tr') %>% unlist)),
      # header background
      sum(grepl(paste0('background:', header_background), strsplit(tab, '<tr') %>% unlist)),
      # sum of rows with color1 and rows with color2
      sum(grepl(paste0('background:', background_color_1), strsplit(tab, '<tr') %>% unlist)) +
      sum(grepl(paste0('background:', background_color_2), strsplit(tab, '<tr') %>% unlist)),
      # inner borders
      sum(grepl(paste0('border-top:3px solid ', color_full), strsplit(tab, '<tr') %>% unlist)),
      # outer borders
      sum(grepl(paste0('border:3px solid ', color_full), strsplit(tab, '<tr') %>% unlist)))
  },
  c(1,
    1,
    attributes(tab)$nrows,
    attributes(tab)$nrows,
    1)
  )

  # One color, multiple total rows, without rownames, but with an id column.
  expect_equal({
    tab <- tableHTML(mtcars, rownames = FALSE) %>%
      add_theme_colorize(total_rows = c(4, 10, 30), id_column = TRUE)
    rgb_col <- col2rgb(attributes(tab)$theme$colors)

    color_full <-  paste0('rgba\\(', paste0(rgb_col[, 1], collapse = ','), ',1\\)')
    header_background <- paste0('rgba\\(', paste0(rgb_col[, 1], collapse = ','), ',0.7\\)')
    background_color_1 <- paste0('rgba\\(', paste0(rgb_col[, 1], collapse = ','), ',0.3\\)')
    background_color_2 <- paste0('rgba\\(', paste0(rgb_col[, 2], collapse = ','), ',0.1\\)')

    c(#total rows background
      sum(grepl(paste0('background:', color_full), strsplit(tab, '<tr') %>% unlist)),
      #header background
      sum(grepl(paste0('background:', header_background), strsplit(tab, '<tr') %>% unlist)),
      #sum of rows with color1 and rows with color2
      sum(grepl(paste0('background:', background_color_1), strsplit(tab, '<tr') %>% unlist)) +
      sum(grepl(paste0('background:', background_color_2), strsplit(tab, '<tr') %>% unlist)),
      # inner borders
      sum(grepl(paste0('border-top:3px solid ', color_full), strsplit(tab, '<tr') %>% unlist)),
      # outer borders
      sum(grepl(paste0('border:3px solid ', color_full), strsplit(tab, '<tr') %>% unlist)))
  },
  c(length(attributes(tab)$theme$total_rows),
    attributes(tab)$nrows + 1,
    attributes(tab)$nrows - (length(attributes(tab)$theme$total_rows) - 1),
    attributes(tab)$nrows - (length(attributes(tab)$theme$total_rows) - 1),
    1)
  )

  # One color, multiple total rows, without rownames, and with row groups.
  expect_equal({
    tab <- tableHTML(mtcars, rownames = FALSE,
                     row_groups = list(c(2, 10, 10, 10),
                                       c('R1', 'R2', 'R3', 'R4'))) %>%
      add_theme_colorize(total_rows = c(4, 10, 30))
    rgb_col <- col2rgb(attributes(tab)$theme$colors)

    color_full <-  paste0('rgba\\(', paste0(rgb_col[, 1], collapse = ','), ',1\\)')
    header_background <- paste0('rgba\\(', paste0(rgb_col[, 1], collapse = ','), ',0.7\\)')
    background_color_1 <- paste0('rgba\\(', paste0(rgb_col[, 1], collapse = ','), ',0.3\\)')
    background_color_2 <- paste0('rgba\\(', paste0(rgb_col[, 2], collapse = ','), ',0.1\\)')

    c(# total rows background
      sum(grepl(paste0('background:', color_full), strsplit(tab, '<tr') %>% unlist)),
      # header background
      sum(grepl(paste0('background:', header_background), strsplit(tab, '<tr') %>% unlist)),
      # sum of rows with color1 and rows with color2
      sum(grepl(paste0('background:', background_color_1), strsplit(tab, '<tr') %>% unlist)) +
        sum(grepl(paste0('background:', background_color_2), strsplit(tab, '<tr') %>% unlist)),
      # inner borders
      sum(grepl(paste0('border-top:3px solid ', color_full), strsplit(tab, '<tr') %>% unlist)),
      # outer borders
      sum(grepl(paste0('border:3px solid ', color_full), strsplit(tab, '<tr') %>% unlist)))
  },
  c(length(attributes(tab)$theme$total_rows),
    length(attributes(tab)$row_groups_data[[1]]) + 1,
    attributes(tab)$nrows - (length(attributes(tab)$theme$total_rows) - 1),
    attributes(tab)$nrows - (length(attributes(tab)$theme$total_rows) - 1),
    1)
  )

  # One color, multiple total rows, without rownames, and with second header.
  expect_equal({
    tab <- tableHTML(mtcars, rownames = FALSE,
                     second_headers = list(c(5, 6),
                                           c('H1', 'H2'))) %>%
      add_theme_colorize(total_rows = c(4, 10, 30))
    rgb_col <- col2rgb(attributes(tab)$theme$colors)

    color_full <- paste0('rgba\\(', paste0(rgb_col[, 1], collapse = ','), ',1\\)')
    header_background <- paste0('rgba\\(', paste0(rgb_col[, 1], collapse = ','), ',0.7\\)')
    background_color_1 <- paste0('rgba\\(', paste0(rgb_col[, 1], collapse = ','), ',0.3\\)')
    background_color_2 <- paste0('rgba\\(', paste0(rgb_col[, 2], collapse = ','), ',0.1\\)')

    c(# total rows background
      sum(grepl(paste0('background:', color_full), strsplit(tab, '<tr') %>% unlist)),
      # header background
      sum(grepl(paste0('background:', header_background), strsplit(tab, '<tr') %>% unlist)),
      # sum of rows with color1 and rows with color2
      sum(grepl(paste0('background:', background_color_1), strsplit(tab, '<tr') %>% unlist)) +
        sum(grepl(paste0('background:', background_color_2), strsplit(tab, '<tr') %>% unlist)),
      # inner borders
      sum(grepl(paste0('border-top:3px solid ', color_full), strsplit(tab, '<tr') %>% unlist)),
      # outer borders
      sum(grepl(paste0('border:3px solid ', color_full), strsplit(tab, '<tr') %>% unlist)))
  },
  c(length(attributes(tab)$theme$total_rows),
    2,
    attributes(tab)$nrows - (length(attributes(tab)$theme$total_rows) - 2),
    attributes(tab)$nrows - (length(attributes(tab)$theme$total_rows) - 2),
    1)
  )
})

