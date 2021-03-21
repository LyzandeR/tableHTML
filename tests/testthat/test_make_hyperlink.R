context("make_hyperlink testing")

test_that("hyperlink is created", {
  
  mylinks <- c('www.abc.com', 'www.bca.com', 'https://www.def.com')
  mymessages <- c('abc', 'bca', 'def')
  
  #no tableHTML
  expect_true(all(grepl('<a href', make_hyperlink(mylinks))))

  
  #check css list has same lengths
  expect_true(all(grepl('<a href', make_hyperlink(mylinks, mymessages))))
  
})
