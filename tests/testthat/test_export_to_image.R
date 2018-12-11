context("testing of export to image")

test_that("Function fails for wrong inputs", {

 #check argument type is picked correctly
 expect_error(
  mtcars %>%
   tableHTML(theme = 'scientific') %>%
   tableHTML_to_image(type = 'abc'),
  'should be one of'
 )

 #check argumrnt add is logical
 expect_error(
  mtcars %>%
   tableHTML() %>%
   tableHTML_to_image(add = 2),
  "add must be TRUE or FALSE"
 )



 #check jpeg prints a file
 expect_true({
  myfile <- tempfile(fileext = '.jpeg')
  mtcars %>%
   tableHTML() %>%
   tableHTML_to_image(type = 'jpeg', file = myfile)
  out <- file.size(myfile) > 1
  file.remove(myfile)
  out
 })

 #check png prints a file
 expect_true({
  myfile <- tempfile(fileext = '.png')
  mtcars %>%
   tableHTML() %>%
   tableHTML_to_image(type = 'png', file = myfile)
  out <- file.size(myfile) > 1
  file.remove(myfile)
  out
 })

 #check png prints a file even with a theme
 expect_true({
  myfile <- tempfile(fileext = '.png')
  mtcars %>%
   tableHTML(theme = 'rshiny-blue') %>%
   tableHTML_to_image(type = 'png', file = myfile)
  out <- file.size(myfile) > 1
  file.remove(myfile)
  out
 })


})


