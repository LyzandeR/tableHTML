context("tableHTML testing")

test_that("Function fails if obj not a data.frame", {
 #no data.frame
 expect_error(tableHTML(letters), 'obj needs to be either')
 #data.frame here
 expect_error(tableHTML(mtcars), NA)
 #matrix here
 expect_error(tableHTML(as.matrix(mtcars)), NA)
})

test_that("check for rownames works", {
 #rownames throws error if not logical
 expect_error(tableHTML(mtcars, rownames = 'foo'), 'rownames argument')
 #rownames works fine if TRUE or FALSE
 expect_error(tableHTML(mtcars, rownames = FALSE), NA)
})

test_that("rows are ok", {
 #number of trs is ok
 expect_identical(length(gregexpr('<tr>', tableHTML(mtcars))[[1]]), nrow(mtcars) + 1L)
})

test_that("columns and ids are ok", {
 #number of tds is ok
 expect_identical(length(gregexpr('<td id="tableHTML_column_1"', tableHTML(mtcars))[[1]]), nrow(mtcars))
 expect_identical(length(gregexpr('<td id="tableHTML_column_2"', tableHTML(mtcars))[[1]]), nrow(mtcars))
 expect_identical(length(gregexpr('<td id="tableHTML_rownames"', tableHTML(mtcars))[[1]]), nrow(mtcars))
})

test_that("widths are ok", {
 #number of widths is ok
 expect_identical(
  length(gregexpr('<col width="100">', tableHTML(mtcars, widths = rep(100, 12)))[[1]]),
  ncol(mtcars) + 1L
 )

 expect_error(tableHTML(mtcars, widths = rep(100, 11)),
              'widths must have the same length as the columns')
 expect_error(tableHTML(mtcars, widths = rep(100, 11), row_groups = list(c(10, 10, 12))),
              'same length as the columns')
 expect_error(tableHTML(mtcars, widths = rep(100, 12), rownames = FALSE),
              'same length as the columns')
 expect_error(tableHTML(mtcars, widths = rep(100, 11), rownames = FALSE, row_groups = list(c(10, 10, 12))),
              'length as the columns')

})

test_that("headers are ok", {
 #number of headers is ok
 expect_identical(length(gregexpr('<th id="tableHTML_header_', tableHTML(mtcars))[[1]]), ncol(mtcars) + 1L)
 expect_true(grepl('<th id="tableHTML_header_8"', tableHTML(mtcars)))
})

test_that("second headers are ok", {
 #number of second headers is ok
 expect_identical(
  length(gregexpr('id="tableHTML_second_header_',
                  tableHTML(mtcars,
                            second_headers = list(c(3, 4, 5),
                                                 c('col1', 'col2', 'col3'))))[[1]]), 3L
 )
 expect_true(grepl('id="tableHTML_second_header_1"',
                   tableHTML(mtcars, second_headers = list(c(3, 4, 5), c('col1', 'col2', 'col3')))))

 expect_error(tableHTML(mtcars, second_headers = c('a', 'b')),
              'second_headers needs to be a list')

 expect_error(tableHTML(mtcars, second_headers = list('a')),
              'second_headers needs to be a list of length')

 expect_error(tableHTML(mtcars, second_headers = list('a', 5)),
              'first element needs to be')

 expect_error(tableHTML(mtcars, second_headers = list(5, 5)),
              'second element needs to be')

 expect_error(tableHTML(mtcars, second_headers = list(5, c('a', 'b'))),
              'need to have the same length')

})


test_that("argument headers has the right length", {
 #number of second headers is ok
 expect_error(
  tableHTML(mtcars, headers = letters), 'The length of the headers'
 )

})

test_that("output has attribute", {

 expect_identical(
  attr(tableHTML(mtcars, headers = letters[1:11]), 'headers'), letters[1:11]
 )

})

test_that("characters < and > get escaped correctly", {

 df <- data.frame(a = factor(c('ldskjf', ';sldfkj</%>;lkdjhf', 'http://www.acb.com/test.php')))
 expect_true(grepl('<td id="tableHTML_column_1">;sldfkj&#60;/%&#62;;lkdjhf</td>',
                   tableHTML(df)))

})

test_that("attributes exist", {

 htmltable <- tableHTML(mtcars)
 expect_identical(attr(htmltable, 'headers'), names(mtcars))
 expect_identical(attr(htmltable, 'nrows'), 32L)
 expect_identical(attr(htmltable, 'ncols'), 11L)
 expect_identical(attr(htmltable, 'col_classes'), rep('numeric', 11))
 expect_identical(attr(htmltable, 'rownames'), TRUE)
 expect_identical(is.null(attr(htmltable, 'row_groups_data')), TRUE)
 expect_identical(is.null(attr(htmltable, 'second_headers_data')), TRUE)
 expect_identical(attr(htmltable, 'row_groups'), FALSE)
 expect_identical(attr(htmltable, 'second_headers'), FALSE)

})

test_that("attribute col_classes captures factors", {

 a <- tableHTML(iris)
 expect_identical(attr(a, 'col_classes')[5], 'factor')


})

test_that("Escapes work fine", {

 df <- data.frame(a = c('ldsfkj>hfdasdf'))
 df2 <- data.frame(a = c('ldsfkj<hfd<asdf'))
 expect_true(grepl('&#62', tableHTML(df)))
 expect_true(grepl('&#60', tableHTML(df2)))
 expect_true(grepl('<', tableHTML(df, escape = FALSE)))
 expect_true(grepl('>', tableHTML(df, escape = FALSE)))

})

test_that("Round works fine", {

 df <- data.frame(a = c(5.04867))
 expect_true(grepl('5.05', tableHTML(df, round = 2)))

})

test_that("replace_NA works fine", {

 df <- data.frame(a = c(NA, 'abc', 'abd'))
 expect_true(grepl('this', tableHTML(df, replace_NA = 'this')))

 df <- data.frame(a = c(NA, 'abc', 'abd'), stringsAsFactors = FALSE)
 expect_true(grepl('this', tableHTML(df, replace_NA = 'this')))

})

test_that("theme is deprecated", {

 expect_error(mtcars %>%
               tableHTML(theme = 'scientific'),
              'Deprecated')

})

test_that("add_data works", {
 expect_error(mtcars %>%
               tableHTML(add_data = c(TRUE, FALSE)),
              "add_data")
 expect_error(mtcars %>%
               tableHTML(add_data = "TRUE"),
              "add_data")

 with_data <- mtcars %>%
  tableHTML(add_data = TRUE)

 expect_identical(mtcars, attributes(with_data)[["data"]])

 without_data <- mtcars %>%
  tableHTML(add_data = FALSE)

 expect_null(attributes(without_data)[["data"]])

})

test_that("spacing starts with a number", {

  expect_error(mtcars %>%
                 tableHTML(collapse = 'separate', spacing = 'abc'),
               'distances in spacing')

})

test_that("round is numeric", {

  expect_error(mtcars %>%
                 tableHTML(round = 'abc'),
               'round needs')

})

test_that("separate works", {

  expect_true(grepl('border-collapse:separate',
              mtcars %>% tableHTML(collapse = 'separate')))

})

test_that("separate_shiny works", {

  expect_true(grepl('border-collapse:separate !important',
                    mtcars %>% tableHTML(collapse = 'separate_shiny')))

})

test_that("print works", {

  out <- capture.output(mtcars %>%
                          tableHTML() %>%
                          print(viewer = FALSE)
                        )
  expect_true(any(grepl('tableHTML_column_1', out)))
  
})


test_that("escape argument works when there are factor columns", {
  
  df <- data.frame(a = as.factor(c('<abc>', '<def>')))
  expect_true(grepl('&#62;', tableHTML(df, escape = TRUE)))
  
})


test_that("border does not work with non-numeric", {
  
  expect_error(tableHTML(mtcars, border = 'a' ), 'border needs ')
  
})
