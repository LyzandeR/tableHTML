context("replace_html testing")

test_that("Function fails for wrong inputs", {
 #no tableHTML
 expect_error(replace_html(mtcars), 'tableHTML needs to be')
 
 expect_true(
   grepl('border-collapse:separated;',
         replace_html(tableHTML(mtcars), 'border-collapse:collapse;', 'border-collapse:separated;')
 ))
})