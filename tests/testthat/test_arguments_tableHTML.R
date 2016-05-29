context("Argument testing")

test_that("Function fails if obj not a data.frame", {
 #no data.frame
 expect_error(tableHTML(letters), 'obj needs to be either')
 #data.frame here
 expect_error(tableHTML(mtcars), NA)
 #matrix here
 expect_error(tableHTML(as.matrix(mtcars)), NA)
})