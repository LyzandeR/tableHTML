## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----example_1-----------------------------------------------------------
library(tableHTML)
tableHTML(mtcars,
          widths = c(140, rep(45, 11))) %>%
  add_css_conditional_column(conditional = '==', 
                             value = 21.4, 
                             css = list('background-color', 'green'), 
                             columns = 'mpg')

## ----example_2-----------------------------------------------------------
tableHTML(mtcars,
          widths = c(140, rep(45, 11))) %>%
  add_css_conditional_column(conditional = '==', 
                             value = 3.15,
                             css = list('background-color', 'steelblue'), 
                             columns = c('drat', 'wt'))

## ----example_3-----------------------------------------------------------
tableHTML(mtcars,
          widths = c(140, rep(45, 11))) %>%
  add_css_conditional_column(conditional = '==', 
                             value = 21.4, 
                             css = list('background-color', 'green'), 
                             columns = 'mpg') %>%
  add_css_conditional_column(conditional = '==',
                             value = 3.15,
                             css = list('background-color', 'steelblue'), 
                             columns = c('drat', 'wt'))

## ----example_4-----------------------------------------------------------
tableHTML(mtcars,
          widths = c(140, rep(45, 11))) %>%
  add_css_conditional_column(conditional = '==', 
                             value = 21.4, 
                             css = list('background-color', 'green'), 
                             columns = 'mpg') %>%
  add_css_conditional_column(conditional = '==', 
                             value = 15.2, 
                             css = list('background-color', 'steelblue'), 
                             columns = 'mpg')

## ----example_5-----------------------------------------------------------
 qu_25_75 <- quantile(mtcars$disp, c(0.25, 0.75))
 
 tableHTML(mtcars,
          widths = c(140, rep(45, 11))) %>%
   add_css_conditional_column(conditional = "<",
                              value = qu_25_75[1], 
                              css = list('background-color', "green"),
                              columns = c("disp")) %>%
   add_css_conditional_column(conditional = "between",
                              between = qu_25_75, 
                              css = list('background-color', "orange"), 
                              columns = c("disp")) %>%
   add_css_conditional_column(conditional = ">", 
                              value = qu_25_75[2], 
                              css = list('background-color', "red"), 
                              columns = c("disp"))

## ----example_6-----------------------------------------------------------
tableHTML(mtcars,
          widths = c(140, rep(45, 11))) %>%
  add_css_conditional_column(conditional = 'min', 
                             css = list('background-color', 'orange'), 
                             columns = c('disp', 'hp'),
                             same_scale = TRUE)

## ----example_7-----------------------------------------------------------
tableHTML(mtcars,
          widths = c(140, rep(45, 11))) %>%
  add_css_conditional_column(conditional = 'min', 
                             css = list('background-color', 'orange'), 
                             columns = c('disp', 'hp'),
                             same_scale = FALSE)

## ----example_8-----------------------------------------------------------
tableHTML(mtcars,
          widths = c(140, rep(45, 11))) %>% 
  add_css_conditional_column(conditional = "min", 
                             css = list('background-color', "green"),
                             columns = seq_along(mtcars),
                             same_scale = FALSE)

## ----example_9-----------------------------------------------------------
tableHTML(mtcars,
          widths = c(140, rep(45, 11))) %>%
  add_css_conditional_column(conditional = "==",
                             value = 14.3, 
                             css = list('background-color', "steelblue"), 
                             columns = 1) %>%
  add_css_conditional_column(conditional = "!=", 
                             value = 8,
                             css = list('background-color', "mediumvioletred"), 
                             columns = 2) %>%
  add_css_conditional_column(conditional = ">",
                             value = 440,
                             css = list('background-color', "orange"),
                             columns = 3) %>%
  add_css_conditional_column(conditional = ">=", 
                             value = 264,
                             css = list('background-color', "green"),
                             columns = 4) %>%
  add_css_conditional_column(conditional = "<",
                             value = 3, 
                             css = list('background-color', "yellow"),
                             columns = 5) %>%
  add_css_conditional_column(conditional = "<=", 
                             value = 2.20, 
                             css = list('background-color', "lightgray"), 
                             columns = 6)
  


## ----example_10----------------------------------------------------------
tableHTML(mtcars,
          widths = c(140, rep(45, 11))) %>%
  add_css_conditional_column(conditional = "min",
                             css = list('background-color', "steelblue"), 
                             columns = 1) %>%
  add_css_conditional_column(conditional = "max", 
                             css = list('background-color', "mediumvioletred"), 
                             columns = 2) %>%
  add_css_conditional_column(conditional = "bottom_n",
                             n = 5, 
                             css = list('background-color', "green"), 
                             columns = c(3, 4), 
                             same_scale = FALSE) %>% 
  add_css_conditional_column(conditional = "top_n",
                             n = 5, 
                             css = list('background-color', "orange"), 
                             columns = c(5, 6)) 

## ----example_11----------------------------------------------------------
tableHTML(mtcars,
          widths = c(140, rep(45, 11))) %>%
  add_css_conditional_column(conditional = "between",
                             between = c(15, 25),
                             css = list('background-color', "steelblue"), 
                             columns = 1)

## ----example_12----------------------------------------------------------
tableHTML(mtcars,
          widths = c(140, rep(45, 11))) %>%
  add_css_conditional_column(conditional = "between", 
                             between = c(20, 22), 
                             css = list('background-color', "steelblue"), 
                             columns = c(1, 7))


## ----example_13----------------------------------------------------------
tableHTML(mtcars,
          widths = c(140, rep(45, 11))) %>%
  add_css_conditional_column(conditional = "contains", 
                             value = "[0-9]", 
                             css = list('background-color', "steelblue"), 
                             columns = "rownames") %>%
  add_css_conditional_column(conditional = "contains", 
                             value = "Honda",
                             css = list('background-color', "silver"), 
                             columns = "rownames")

## ----example_20----------------------------------------------------------
df <- data.frame(factor_alphabetic = c('d', 'a', 'e', 'a', 'd', 'd', 'a', 'c', 'd', 'a'),
                 factor_ordered = c('D', 'A', 'E', 'A', 'D', 'D', 'A', 'C', 'D', 'A'),
                 stringsAsFactors = TRUE)
tableHTML(df, 
          rownames = FALSE) %>%
  add_css_conditional_column(colour_rank_theme = 'White-Green',
                             columns = 1) %>%
  add_css_conditional_column(colour_rank_theme = 'White-Green', 
                             columns = 2,
                             levels = c('B', 'D', 'A', 'E', 'C'))

## ----example_14----------------------------------------------------------
tableHTML(mtcars,
          widths = c(140, rep(45, 11))) %>%
  add_css_conditional_column(colour_rank_theme = "RAG", columns = 1) %>%
  add_css_conditional_column(colour_rank_theme = "Spectral", columns = 2) %>%
  add_css_conditional_column(colour_rank_theme = "Rainbow", columns = 3) %>%
  add_css_conditional_column(colour_rank_theme = "White-Green", columns = 4) %>%
  add_css_conditional_column(colour_rank_theme = "White-Blue", columns = 5) %>%
  add_css_conditional_column(colour_rank_theme = "White-Red", columns = 6) 

## ----example_15----------------------------------------------------------
tableHTML(mtcars,
          widths = rep(100, 12)) %>%
  add_css_conditional_column(colour_rank_theme = "RAG", 
                             columns = 1, 
                             decreasing = TRUE) 

## ----example_16----------------------------------------------------------
tableHTML(data.frame(a = 1:20, b = rep(1:5, 4), c = 1:20, d = rep(1:5, 4)), 
          width = rep(80, 4),
          second_headers = list(c(2, 2), 
                                c("same_scale = TRUE",
                                  "same_scale = FALSE")),
          rownames = FALSE) %>%
  add_css_conditional_column(colour_rank_theme = "RAG",
                             columns = c(1, 2), 
                             decreasing = FALSE, 
                             same_scale = TRUE) %>%
  add_css_conditional_column(colour_rank_theme = "RAG", 
                             columns = c(3, 4), 
                             decreasing = FALSE, 
                             same_scale = FALSE)

## ----example_17----------------------------------------------------------
colour_rank_css <- 
  make_css_colour_rank_theme(list(qsec = mtcars$qsec),
                             colors = RColorBrewer::brewer.pal(9, "Set1"))

tableHTML(mtcars,
          widths = c(140, rep(45, 11))) %>%
  add_css_conditional_column(colour_rank_theme =  "Custom", 
                             colour_rank_css = colour_rank_css, 
                             columns = 7)

## ----example_18----------------------------------------------------------
tableHTML(mtcars,
          widths = c(120, 200, rep(100, 11)),
          row_groups = list(c(10, 10, 12), c('Group 1', 'Group 2', 'Group 3')),
          theme = "rshiny-blue") %>%
 add_css_column(css = list('border', '1px solid'), columns = 1) %>%
 add_css_conditional_column(colour_rank_theme = "RAG", columns = 1) %>%
 add_css_conditional_column(conditional = "contains",
                            value = "1", 
                            css = list(c('color', 'font-size', 'border'), 
                                       c('steelblue', '20', '1px solid steelblue')),
                            columns = "row_groups") %>%
 add_css_conditional_column(conditional = "contains",
                            value = "2", 
                            css = list(c('color', 'font-size', 'border'),
                                       c('royalblue', '30', '1px solid royalblue')),
                            columns = "row_groups") %>%
 add_css_conditional_column(conditional = "contains",
                            value = "3", 
                            css = list(c('color', 'border'),
                                       c('navy', '1px solid navy')),
                            columns = "row_groups")

## ----example_19----------------------------------------------------------
tableHTML(mtcars, 
          border = 2,
          rownames = TRUE, 
          widths = c(80, 140, rep(50, 11)),
          row_groups = list(c(10, 10, 12), c('Group 1', 'Group 2', 'Group 3')),
          second_headers = list(c(3, 4, 6), c('col1', 'col2', 'col3'))) %>%
  add_css_second_header(css = list(c('background-color', 'color', 'height'),
                                   c('#2E5894', 'white', '50px')),
                        second_headers = 1:3) %>%
  add_css_header(css = list(c('transform', 'height'),
                            c('rotate(-45deg)', '50px')),
                 headers = 3:13) %>%
  add_css_row(css = list('background-color', '#f2f2f2'), 
              rows = even(3:34)) %>%
  add_css_conditional_column(colour_rank_css = 
                               make_css_colour_rank_theme(list(row_groups = 1:3),
                                                          colors = c('#00b200',
                                                                     '#007f00',
                                                                     '#004c00'),
                                                          css_property = 'color'),
                             columns = 'row_groups') %>%
  add_css_conditional_column(conditional = "contains",
                             value = "1", 
                             css = list('background-color', '#F5F5F5'),
                             columns = "row_groups") %>%
  add_css_conditional_column(conditional = "contains",
                             value = "2", 
                             css = list('background-color', '#D0D0D0'),
                             columns = "row_groups") %>%
  add_css_conditional_column(conditional = "contains",
                             value = "3", 
                             css = list('background-color', '#A9A9A9'),
                             columns = "row_groups") %>%
  add_css_conditional_column(colour_rank_theme = 'RAG', 
                             columns = 4)

