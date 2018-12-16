#' Generate package's logo
#'
#' \code{create_logo} will create and generate the package's logo.
#'
#' \code{create_logo} will create and generate the package's logo.
#'
#' @param save A boolean when set to TRUE the logo will be saved in
#' the specified format.
#'
#' @param format A character string to specify the format of the output,
#' it accepts \code{'html'}, \code{'png'}, or \code{'jpeg'}.
#' Default is \code{'html'}.
#'
#' @param file A character string to specify the name and path to the new file.
#' Should end with \code{'.html'}, \code{'.png'}, or \code{'.jpeg'},
#' depending on the selected format.
#'
#' @param ... Further parameters to pass to webshot.
#'
#' @inheritParams write_tableHTML
#' 
#' @return The logo of the tableHTML package as a tableHTML object.
#' The output will be shown in the Viewer pane, and can be saved
#' either as an image or as an HTML.
#'
#' @examples
#' create_logo(save = FALSE)
#'
#' create_logo(format = 'png', file = '~/exported_logo.png')
#'
#' @importFrom stats setNames
#'
#' @export


create_logo <- function(save = TRUE, format = 'html',
                        file = 'tableHTML_logo.html',
                        complete_html = FALSE,
                        ...){
  if(save){
    if(! format %in% c('html', 'png', 'jpeg')){
      stop("format should be 'html', 'png', or 'jpeg'")
    }}
  if(!endsWith(file, format)){
  stop("file extension should be the same as the the format")
 }

  # Create Data
  df_t <- do.call(rbind, strsplit(c('...t...', '..ttt..', '...t...', '...t...',
                                    '...t...', '...t...','...t...', '...tt..'), ''))
  df_a <- do.call(rbind, strsplit(c('..aa...', '....a..', '..aaa..', '.a..a..',
                                    '..aaa..'), ''))
  df_b <- do.call(rbind, strsplit(c('..b....', '..b....', '..b....', '..b....',
                                    '..bbb..', '..b..b.', '..b..b.', '..bbb..'), ''))
  df_l <- do.call(rbind, strsplit(c('..l....', '..l....', '..l....', '..l....',
                                    '..l....', '..l....','..l....', '..ll...'), ''))
  df_e <- do.call(rbind, strsplit(c('..ee...', '.e..e..', '.eee...', '.e.....',
                                    '..ee...'), ''))

  df_H <- do.call(rbind, strsplit(c('.H...H.', '.H...H.', '.H...H.', '.H...H.',
                                    '.HHHHH.','.H...H.', '.H...H.', '.H...H.', '.H...H.'), ''))
  df_T <- do.call(rbind, strsplit(c('.TTTTT.', '...T...', '...T...', '...T...',
                                    '...T...', '...T...','...T...', '...T...', '...T...'), ''))
  df_M <- do.call(rbind, strsplit(c('.M...M.', '.M...M.', '.MM.MM.', '.M.M.M.',
                                    '.M.M.M.','.M...M.', '.M...M.', '.M...M.', '.M...M.'), ''))
  df_L <- do.call(rbind, strsplit(c('.L.....', '.L.....', '.L.....', '.L.....',
                                    '.L.....', '.L.....','.L.....', '.L.....', '.LLLL..'), ''))

  tablehtml_raw <- list(df_t, df_a, df_b, df_l, df_e, df_H, df_T, df_M, df_L)
  rows <- max(unlist(lapply(tablehtml_raw, nrow)))

  df <- do.call(cbind,
                lapply(tablehtml_raw,
                       function(x, n){
                         if (nrow(x) < n){
                           x <- rbind(matrix(rep(rep('.', ncol(x)), n - nrow(x)),
                                             nrow = n - nrow(x)), x)}
                         x <- x[, which(colSums(x == '.') != rows)]
                         return(x)
                       }, rows))

  df <- rbind(matrix(rep('.', ncol(df)*3), nrow = 3),
              df,
              matrix(rep('.', ncol(df)*6), nrow = 6))
  df <- gsub('\\.', ' ', df)

  # second header
  header_2 = unlist(strsplit('tableHTML', ''))

  # colors
  colors = c('#ffe100', '#e8693b', '#681bb5', '#2b912d',
             '#b2ff9b', '#4442ff', '#3ae8df', '#f7c0e6', '#f4429e')

  # create the logo
  logo <- tableHTML(df,
            headers = rep('..', ncol(df)),
            rownames = FALSE,
            widths = rep(20, ncol(df)),
            class = '\"hexagon inner\"',
            second_headers = list(c(3, 4, 4, 2, 4, 5, 5, 5, 4), header_2)) %>%
    add_theme('scientific') %>%
    add_css_second_header(css = list('background-color', colors[1]), second_headers = 1) %>%
    add_css_second_header(css = list('background-color', colors[2]), second_headers = 2) %>%
    add_css_second_header(css = list('background-color', colors[3]), second_headers = 3) %>%
    add_css_second_header(css = list('background-color', colors[4]), second_headers = 4) %>%
    add_css_second_header(css = list('background-color', colors[5]), second_headers = 5) %>%
    add_css_second_header(css = list('background-color', colors[6]), second_headers = 6) %>%
    add_css_second_header(css = list('background-color', colors[7]), second_headers = 7) %>%
    add_css_second_header(css = list('background-color', colors[8]), second_headers = 8) %>%
    add_css_second_header(css = list('background-color', colors[9]), second_headers = 9) %>%
    add_css_conditional_column(conditional = '==', value ='t',
                               css = list(c('background-color', 'border-top', 'border-left', 'border-right', 'border-bottom'),
                                          rep(colors[1]), 5),
                               columns = 1:ncol(df)) %>%
    add_css_conditional_column(conditional = '==', value ='a',
                               css = list(c('background-color', 'border-top', 'border-left', 'border-right', 'border-bottom'),
                                          rep(colors[2]), 5),
                               columns = 1:ncol(df)) %>%
    add_css_conditional_column(conditional = '==', value ='b',
                               css = list(c('background-color', 'border-top', 'border-left', 'border-right', 'border-bottom'),
                                          rep(colors[3]), 5),
                               columns = 1:ncol(df)) %>%
    add_css_conditional_column(conditional = '==', value ='l',
                               css = list(c('background-color', 'border-top', 'border-left', 'border-right', 'border-bottom'),
                                          rep(colors[4]), 5),
                               columns = 1:ncol(df)) %>%
    add_css_conditional_column(conditional = '==', value ='e',
                               css = list(c('background-color', 'border-top', 'border-left', 'border-right', 'border-bottom'),
                                          rep(colors[5]), 5),
                               columns = 1:ncol(df),
                               same_scale = TRUE) %>%
    add_css_conditional_column(conditional = '==', value ='H',
                               css = list(c('background-color', 'border-top', 'border-left', 'border-right', 'border-bottom'),
                                          rep(colors[6]), 5),
                               columns = 1:ncol(df),
                               same_scale = TRUE) %>%
    add_css_conditional_column(conditional = '==', value ='T',
                               css = list(c('background-color', 'border-top', 'border-left', 'border-right', 'border-bottom'),
                                          rep(colors[7]), 5),
                               columns = 1:ncol(df),
                               same_scale = TRUE) %>%
    add_css_conditional_column(conditional = '==', value ='M',
                               css = list(c('background-color', 'border-top', 'border-left', 'border-right', 'border-bottom'),
                                          rep(colors[8]), 5),
                               columns = 1:ncol(df),
                               same_scale = TRUE) %>%
    add_css_conditional_column(conditional = '==', value ='L',
                               css = list(c('background-color', 'border-top', 'border-left', 'border-right', 'border-bottom'),
                                          rep(colors[9]), 5),
                               columns = 1:ncol(df),
                               same_scale = TRUE) %>%
    add_css_row(rows = 1:(nrow(df)+2),
                css = list(c('text-align', 'height'), c('center', '25px')))

  # add attributes needed for testing
  attr(logo, 'letters_cnt') <- c('t' = sum(df_t != '.'), 'a' = sum(df_a != '.'), 'b' = sum(df_b != '.'), 'l' = sum(df_l != '.'), 'e' = sum(df_e != '.'),
                                 'H' = sum(df_H != '.'), 'T' = sum(df_T != '.'), 'M' = sum(df_M != '.'), 'L' = sum(df_L != '.'))

  attr(logo, 'colors') <- c(colors) %>% setNames(header_2)

  # save the logo
  if (save){
    if (format != 'html'){
     tableHTML_to_image(logo,
                       file = file,
                       type = format,
                       zoom = 5,
                       ...)
    }else{
     write_tableHTML(logo,
                      file = file,
                      complete_html = complete_html)
    }
  }
  logo
}

