context("test odd_even")

test_that("odd works fine", {
 #no tableHTML
 expect_error(odd(1:10), NA)

})

test_that("odd outputs correct", {
 #no tableHTML
 expect_equal(odd(1:10), c(1, 3, 5, 7, 9))

})

test_that("even works fine", {
 #no tableHTML
 expect_error(even(1:10), NA)

})

test_that("even outputs correct", {
 #no tableHTML
 expect_equal(even(1:10), c(2, 4, 6, 8, 10))

})
