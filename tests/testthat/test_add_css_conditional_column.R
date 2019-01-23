context("add_css_conditional_column testing")

test_that("Function fails for wrong inputs", {

  # no tableHTML
  expect_error(add_css_conditional_column(mtcars, 1),
               'tableHTML needs to be')

  # no columns specified
  expect_error(tableHTML(mtcars) %>% add_css_conditional_column(conditional = "==", value = 1,
                                                                css = list('background-color', 'lightgray')),
               'argument "columns" is missing, with no default')

  # wrong conditional argument
  expect_error(tableHTML(mtcars) %>% add_css_conditional_column(columns = 1,
                                                                conditional = "blubb"),
               "arg' should be one of ")

  # no css provided
  expect_error(tableHTML(mtcars) %>% add_css_conditional_column(conditional = "==", value = 1, column = 1),
               'css needs to be provided')



  # typeof columns numeric and character
  expect_error(tableHTML(mtcars) %>%
                 add_css_conditional_column(conditional = "==", value = 1,
                                            columns = c("rownames", 1),
                                            css = list('background-color', 'lightgray')),
               'columns must either be numeric or text')

  # no rownames in tableHTML, but css to be applied
  expect_error(tableHTML(mtcars, rownames = FALSE) %>%
                 add_css_conditional_column(conditional = "==", value = 1,
                                            columns = c("rownames"),
                                            css = list('background-color', 'lightgray')),
               'tableHTML does not have rownames')

  # no rowgroups in tableHTML, but css to be applied
  expect_error(tableHTML(mtcars, rownames = FALSE) %>%
                 add_css_conditional_column(conditional = "==", value = 1,
                                            columns = c("row_groups"),
                                            css = list('background-color', 'lightgray')),
               'tableHTML does not have row_groups')

  #begin and end values missing
  expect_error(tableHTML(mtcars) %>%
                 add_css_conditional_column(conditional = "between",
                                            columns = c(1),
                                            css = list('background-color', 'lightgray')),
               'begin and end values for between need to be provided')

  # length of between not 2
  expect_error(tableHTML(mtcars) %>%
                 add_css_conditional_column(conditional = "between", between = 1,
                                            columns = c(1),
                                            css = list('background-color', 'lightgray')),
               'between needs to be a vector')

  # between not a numeric vector
  expect_error(tableHTML(mtcars) %>%
                 add_css_conditional_column(conditional = "between", between = list(1, 2),
                                            columns = c(1),
                                            css = list('background-color', 'lightgray')),
               'between needs to be a vector')

  # begin ! <= end
  expect_error(tableHTML(mtcars) %>%
                 add_css_conditional_column(conditional = "between", between = c(2, 1),
                                            columns = c(1),
                                            css = list('background-color', 'lightgray')),
               'begin must be smaller than end in between')

  # top_n warning
  expect_warning(tableHTML(mtcars) %>%
                   add_css_conditional_column(conditional = "top_n",
                                              columns = c(1),
                                              css = list('background-color', 'lightgray')),
                 'n not provided')

  # bottom_n warning
  expect_warning(tableHTML(mtcars) %>%
                   add_css_conditional_column(conditional = "bottom_n",
                                              columns = c(1),
                                              css = list('background-color', 'lightgray')),
                 'n not provided')



})

test_that("equations and inequations work", {

  #test that css is applied to column 1, row 4 + 32
  expect_equal(
    {
      tableHTML <- tableHTML(mtcars) %>%
        add_css_conditional_column(conditional = "==", value = 21.4, css = list('background-color', "green"), columns = c("mpg"))

      starts <- gregexpr("<td", tableHTML)[[1]]

      ends <- gregexpr("</td>", tableHTML)[[1]]

      which(vapply(seq_along(starts), function(i) {
        grepl('style="background-color:green;"', substr(tableHTML, starts[i], ends[i] + 4))
      }, FUN.VALUE = logical(1)))
    },
    expected = c(38, 374)
  )
  #test that css is NOT applied to column 1, row 4 + 32
  expect_equal(
    {
      tableHTML <- tableHTML(mtcars) %>%
        add_css_conditional_column(conditional = "!=", value = 21.4, css = list('background-color', "green"), columns = c("mpg"))

      starts <- gregexpr("<td", tableHTML)[[1]]

      ends <- gregexpr("</td>", tableHTML)[[1]]

      which(vapply(seq_along(starts), function(i) {
        grepl('style="background-color:green;"', substr(tableHTML, starts[i], ends[i] + 4))
      }, FUN.VALUE = logical(1)))
    },
    expected = c(2, 14, 26, 50, 62, 74, 86, 98, 110, 122, 134, 146, 158, 170, 182, 194,
                 206, 218, 230, 242, 254, 266, 278, 290, 302, 314, 326, 338, 350, 362)
  )

  #test inequations
  expect_equal(
    {
      tableHTML <- tableHTML(mtcars) %>%
        add_css_conditional_column(conditional = "<", value = 14.7, css = list('background-color', "green"), columns = c("mpg")) %>%
        add_css_conditional_column(conditional = "<=", value = 75.7, css = list('background-color', "blue"), columns = c("disp")) %>%
        add_css_conditional_column(conditional = ">", value = 4, css = list('background-color', "red"), columns = c("gear")) %>%
        add_css_conditional_column(conditional = ">=", value = 6, css = list('background-color', "orange"), columns = c("carb"))

      starts <- gregexpr("<td", tableHTML)[[1]]

      ends <- gregexpr("</td>", tableHTML)[[1]]
      c(
        which(vapply(seq_along(starts), function(i) {
          grepl('style="background-color:green;"', substr(tableHTML, starts[i], ends[i] + 4))
        }, FUN.VALUE = logical(1))),
        which(vapply(seq_along(starts), function(i) {
          grepl('style="background-color:blue;"', substr(tableHTML, starts[i], ends[i] + 4))
        }, FUN.VALUE = logical(1))),
        which(vapply(seq_along(starts), function(i) {
          grepl('style="background-color:red;"', substr(tableHTML, starts[i], ends[i] + 4))
        }, FUN.VALUE = logical(1))),
        which(vapply(seq_along(starts), function(i) {
          grepl('style="background-color:orange;"', substr(tableHTML, starts[i], ends[i] + 4))
        }, FUN.VALUE = logical(1)))
      )
    },
    expected = c(74, 170, 182, 278, 220, 232, 323, 335, 347, 359, 371, 360, 372)
  )

  expect_equal(
    {
      tableHTML <- tableHTML(mtcars) %>%
        add_css_conditional_column(conditional = ">", value = 4.43, css = list('background-color', "orange"), columns = c("drat", "wt"))


      starts <- gregexpr("<td", tableHTML)[[1]]

      ends <- gregexpr("</td>", tableHTML)[[1]]
      which(vapply(seq_along(starts), function(i) {
        grepl('style="background-color:orange;"', substr(tableHTML, starts[i], ends[i] + 4))
      }, FUN.VALUE = logical(1)))
    },
    expected = c(175, 187, 199, 222)
  )

})

test_that("min max work", {

  expect_equal(
    {
      tableHTML <- tableHTML(mtcars) %>%
        add_css_conditional_column(conditional = "min", css = list('background-color', "orange"), columns = c("qsec"))

      starts <- gregexpr("<td", tableHTML)[[1]]

      ends <- gregexpr("</td>", tableHTML)[[1]]

      which(vapply(seq_along(starts), function(i) {
        grepl('style="background-color:orange;"', substr(tableHTML, starts[i], ends[i] + 4))
      }, FUN.VALUE = logical(1)))
    },
    expected = 344
  )

  expect_equal(
    {
      tableHTML <- tableHTML(mtcars) %>%
        add_css_conditional_column(conditional = "max", css = list('background-color', "orange"), columns = c("qsec"))

      starts <- gregexpr("<td", tableHTML)[[1]]

      ends <- gregexpr("</td>", tableHTML)[[1]]

      which(vapply(seq_along(starts), function(i) {
        grepl('style="background-color:orange;"', substr(tableHTML, starts[i], ends[i] + 4))
      }, FUN.VALUE = logical(1)))
    },
    expected = 104
  )

  #test that only 1 highlighted
  expect_equal(
    {
      tableHTML <- tableHTML(mtcars) %>%
        add_css_conditional_column(conditional = "min",css = list('background-color', "orange"),
                                   columns = c("gear", "carb"), same_scale = TRUE)

      starts <- gregexpr("<td", tableHTML)[[1]]

      ends <- gregexpr("</td>", tableHTML)[[1]]

      which(vapply(seq_along(starts), function(i) {
        grepl('style="background-color:orange;"', substr(tableHTML, starts[i], ends[i] + 4))
      }, FUN.VALUE = logical(1)))
    },
    expected = c(36, 48, 72, 216, 240, 252, 312)
  )

  #test 1 in carb and 3 in gear highlighted
  expect_equal(
    {
      tableHTML <- tableHTML(mtcars) %>%
        add_css_conditional_column(conditional = "min",css = list('background-color', "orange"),
                                   columns = c("gear", "carb"), same_scale = FALSE)

      starts <- gregexpr("<td", tableHTML)[[1]]

      ends <- gregexpr("</td>", tableHTML)[[1]]

      which(vapply(seq_along(starts), function(i) {
        grepl('style="background-color:orange;"', substr(tableHTML, starts[i], ends[i] + 4))
      }, FUN.VALUE = logical(1)))
    },
    expected = c(36, 47, 48, 59, 71, 72, 83, 143, 155, 167, 179, 191, 203, 216, 240, 251, 252, 263, 275, 287, 299, 312)
  )

})

test_that("top_n and bottom_n work", {

  #top 5 in drat, top in wt
  expect_equal(
    {
      tableHTML <- tableHTML(mtcars) %>%
        add_css_conditional_column(conditional = "top_n", n = 5, css = list('background-color', "orange"),
                                   columns = c("drat", "wt"), same_scale = FALSE)

      starts <- gregexpr("<td", tableHTML)[[1]]

      ends <- gregexpr("</td>", tableHTML)[[1]]

      which(vapply(seq_along(starts), function(i) {
        grepl('style="background-color:orange;"', substr(tableHTML, starts[i], ends[i] + 4))
      }, FUN.VALUE = logical(1)))
    },
    expected = c(139, 175, 187, 199, 222, 234, 295, 318, 342, 378)
  )

  #top 5 in drat AND wt
  expect_equal(
    {
      tableHTML <- tableHTML(mtcars) %>%
        add_css_conditional_column(conditional = "top_n", n = 5, css = list('background-color', "orange"),
                                   columns = c("drat", "wt"), same_scale = TRUE)

      starts <- gregexpr("<td", tableHTML)[[1]]

      ends <- gregexpr("</td>", tableHTML)[[1]]

      which(vapply(seq_along(starts), function(i) {
        grepl('style="background-color:orange;"', substr(tableHTML, starts[i], ends[i] + 4))
      }, FUN.VALUE = logical(1)))
    },
    expected = c(175, 187, 199, 222, 318)
  )

  #bottom_1 == min
  expect_equal(
    {
      tableHTML <- tableHTML(mtcars) %>%
        add_css_conditional_column(conditional = "bottom_n", n = 1, css = list('background-color', "orange"),
                                   columns = c("drat", "wt"), same_scale = TRUE)

      starts <- gregexpr("<td", tableHTML)[[1]]

      ends <- gregexpr("</td>", tableHTML)[[1]]

      which(vapply(seq_along(starts), function(i) {
        grepl('style="background-color:orange;"', substr(tableHTML, starts[i], ends[i] + 4))
      }, FUN.VALUE = logical(1)))
    },
    expected = {
      tableHTML <- tableHTML(mtcars) %>%
        add_css_conditional_column(conditional = "min", css = list('background-color', "orange"),
                                   columns = c("drat", "wt"), same_scale = TRUE)

      starts <- gregexpr("<td", tableHTML)[[1]]

      ends <- gregexpr("</td>", tableHTML)[[1]]

      which(vapply(seq_along(starts), function(i) {
        grepl('style="background-color:orange;"', substr(tableHTML, starts[i], ends[i] + 4))
      }, FUN.VALUE = logical(1)))
    }
  )

})

test_that("between works", {

  expect_equal(
    {
      tableHTML <- tableHTML(mtcars) %>%
        add_css_conditional_column(conditional = "between", between = c(3.5, 3.7), css = list('background-color', "orange"),
                                   columns = c("drat"))

      starts <- gregexpr("<td", tableHTML)[[1]]

      ends <- gregexpr("</td>", tableHTML)[[1]]

      which(vapply(seq_along(starts), function(i) {
        grepl('style="background-color:orange;"', substr(tableHTML, starts[i], ends[i] + 4))
      }, FUN.VALUE = logical(1)))
    },
    expected = c(90, 246, 354, 366)
  )



})

test_that("color rank works", {
  #check colors are correct
  expect_equal(
    {
      tableHTML <- tableHTML(mtcars) %>%
        add_css_conditional_column(conditional = "color_rank", color_rank_theme = "RAG",
                                   columns = c("carb"))

      starts <- gregexpr("<td", tableHTML)[[1]]

      ends <- gregexpr("</td>", tableHTML)[[1]]
      colors <-
        vapply(seq_along(starts), function(i) {
          td <- substr(tableHTML, starts[i], ends[i] + 4)
          ifelse(grepl('style="', td), substr(td, gregexpr("#", td)[[1]], gregexpr("#", td)[[1]] + 6), NA_character_)
        }, FUN.VALUE = character(1))
      colors <- colors[!is.na(colors)]
    },
    expected = c('#E8E58F', '#E8E58F', '#86C183', '#86C183', '#A3CB87', '#86C183', '#E8E58F', '#A3CB87',
                 '#A3CB87', '#E8E58F', '#E8E58F', '#C2D78B', '#C2D78B', '#C2D78B', '#E8E58F', '#E8E58F',
                 '#E8E58F', '#86C183', '#A3CB87', '#86C183', '#86C183', '#A3CB87', '#A3CB87', '#E8E58F',
                 '#A3CB87', '#86C183', '#A3CB87', '#A3CB87', '#E8E58F', '#F0B681', '#F8696B', '#A3CB87')
  )

  # check colors on same scale
  expect_equal(
    {
      tableHTML <- tableHTML(data.frame(a = 1:10, b = rep(1:5, 2)), rownames = FALSE) %>%
        add_css_conditional_column(conditional = "color_rank", color_rank_theme = "RAG",
                                   columns = c("a", "b"), same_scale = TRUE)
      starts <- gregexpr("<td", tableHTML)[[1]]

      ends <- gregexpr("</td>", tableHTML)[[1]]
      colors <-
        vapply(seq_along(starts), function(i) {
          td <- substr(tableHTML, starts[i], ends[i] + 4)
          ifelse(grepl('style="', td), substr(td, gregexpr("#", td)[[1]], gregexpr("#", td)[[1]] + 6), NA_character_)
        }, FUN.VALUE = character(1))

    },
    expected = c('#86C183', '#86C183', '#9CC986', '#9CC986', '#B3D189',
                 '#B3D189', '#CFDC8C', '#CFDC8C', '#EDE690', '#EDE690',
                 '#F9DE8D', '#86C183', '#F3C285', '#9CC986', '#F0A67C',
                 '#B3D189', '#F38773', '#CFDC8C', '#F8696B', '#EDE690')
  )

  #check independent scales
  expect_equal(
    {
      tableHTML <- tableHTML(data.frame(a = 1:10, b = rep(1:5, 2)), rownames = FALSE) %>%
        add_css_conditional_column(conditional = "color_rank", color_rank_theme = "RAG",
                                   columns = c("a", "b"), same_scale = FALSE)
      starts <- gregexpr("<td", tableHTML)[[1]]

      ends <- gregexpr("</td>", tableHTML)[[1]]
      colors <-
        vapply(seq_along(starts), function(i) {
          td <- substr(tableHTML, starts[i], ends[i] + 4)
          ifelse(grepl('style="', td), substr(td, gregexpr("#", td)[[1]], gregexpr("#", td)[[1]] + 6), NA_character_)
        }, FUN.VALUE = character(1))

    },
    expected = c('#86C183', '#86C183', '#9CC986', '#B9D48A', '#B3D189',
                 '#FCEC92', '#CFDC8C', '#EFAE7F', '#EDE690', '#F8696B',
                 '#F9DE8D', '#86C183', '#F3C285', '#B9D48A', '#F0A67C',
                 '#FCEC92', '#F38773', '#EFAE7F', '#F8696B', '#F8696B')
  )

})
