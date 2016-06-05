context("tableHTML testing")

test_that("Function fails if obj not a data.frame", {
 #no data.frame
 expect_error(tableHTML(letters), 'obj needs to be either')
 #data.frame here
 expect_error(tableHTML(mtcars), NA)
 #matrix here
 expect_error(tableHTML(as.matrix(mtcars)), NA)
})

test_that("rows are ok", {
 #number of trs is ok
 expect_identical(length(gregexpr('<tr>', tableHTML(mtcars))[[1]]), nrow(mtcars) + 1L)
})

test_that("columns and ids are ok", {
 #number of tds is ok
 expect_identical(length(gregexpr('<td id="mpg"', tableHTML(mtcars))[[1]]), nrow(mtcars))
 expect_identical(length(gregexpr('<td id="cyl"', tableHTML(mtcars))[[1]]), nrow(mtcars))
 expect_identical(length(gregexpr('<td id="rownames"', tableHTML(mtcars))[[1]]), nrow(mtcars))
})

test_that("widths are ok", {
 #number of widths is ok
 expect_identical(
  length(gregexpr('<col width="100">', tableHTML(mtcars, widths = rep(100, 12)))[[1]]), 
  ncol(mtcars) + 1L
 )
})
 
test_that("headers are ok", {
 #number of headers is ok
 expect_identical(length(gregexpr('<th id="header_', tableHTML(mtcars))[[1]]), ncol(mtcars) + 1L)
 expect_true(grepl('<th id="header_8"', tableHTML(mtcars))) 
})
 
test_that("second headers are ok", {
 #number of second headers is ok
 expect_identical(
  length(gregexpr('id="overheader_', 
                  tableHTML(mtcars, 
                            second_header = list(c(3, 4, 5), 
                                                 c('col1', 'col2', 'col3'))))[[1]]), 3L
 )
 expect_true(grepl('id="overheader_1"', 
                   tableHTML(mtcars, second_header = list(c(3, 4, 5), c('col1', 'col2', 'col3'))))) 
})




