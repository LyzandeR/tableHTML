context("extract_column_data testing")

test_that("Function fails for wrong inputs", {
  #no tableHTML
  expect_error(extract_column_data(mtcars, 1),
               'tableHTML needs to be')
  
  # input not numeric
  expect_error(tableHTML(mtcars) %>% 
                 extract_column_data("character"),
               'indices have to be numeric')
  
  #all checks ok
  expect_error(tableHTML(mtcars) %>% 
                 extract_column_data(indices = 1), NA)

})

test_that("Correct values are extracted", {
  
  #check correct values extracted
  for (n in names(mtcars)) {
    expect_equal(unname(unlist(tableHTML(mtcars) %>% 
                   extract_column_data(indices = which(names(mtcars) == n)))),
                 mtcars[, n])
  }
  #check rownames extraction
  expect_equal(unname(unlist(tableHTML(mtcars) %>% 
                 extract_column_data(indices = 0))),
               rownames(mtcars))
  
})

