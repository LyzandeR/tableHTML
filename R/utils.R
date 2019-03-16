is.fachar <- function(x) is.character(x) | is.factor(x)


replace_style<- function(tableHTML, split, style, condition = NULL){

  if (is.null(condition)) {
    condition <- rep(TRUE, attr(tableHTML, "nrows"))
  }

  tableHTML <- gsub(paste(split, "style="),
                    paste0(split), tableHTML)

  splits <- strsplit(tableHTML, split)

  splits[[1]][2:length(splits[[1]])] <-
    vapply(2:length(splits[[1]]), function(i) {
      x <- splits[[1]][i]
      #check if style should be applied
      if (condition[i - 1]) {
        paste0(split,' ', style[i - 1], x)
      } else {
        #if not, check if style was applied from previous functions
        # or if rowspan attribute present
        if (substr(x, 1, 1) == ">" | substr(x, 1, 8) == " rowspan") {
          paste0(split, x)
        } else {
          paste0(split, ' style=', x)
        }
      }

    }, FUN.VALUE = character(1))

  gsub(';""', ';', paste(splits[[1]], collapse = ''))

}

fix_header_dupes <- function(h) {
 dup_positions <- which(duplicated(h))
 empty_strings <- sapply(dup_positions, function(x) paste0(rep(' ', x), collapse = ''))
 h[dup_positions] <- paste0(h[dup_positions], empty_strings)
 h
}

#taken from webshot
phantom_paths <- function() {
  if (is_windows()) {
    path <- Sys.getenv('APPDATA', '')
    path <- if (dir_exists(path)) file.path(path, 'PhantomJS')
  } else if (is_osx()) {
    path <- '~/Library/Application Support'
    path <- if (dir_exists(path)) file.path(path, 'PhantomJS')
  } else {
    path <- '~/bin'
  }
  path <- c(path, system.file('PhantomJS', package = 'webshot'))
  path
}

find_phantom <- function() {
  path <- Sys.which( "phantomjs" )
  if (path != "") return(path)
  
  for (d in phantom_paths()) {
    exec <- if (is_windows()) "phantomjs.exe" else "phantomjs"
    path <- file.path(d, exec)
    if (utils::file_test("-x", path)) break else path <- ""
  }
  
  if (path == "") {
    # It would make the most sense to throw an error here. However, that would
    # cause problems with CRAN. The CRAN checking systems may not have phantomjs
    # and may not be capable of installing phantomjs (like on Solaris), and any
    # packages which use webshot in their R CMD check (in examples or vignettes)
    # will get an ERROR. We'll issue a message and return NULL; other
    message(
      "PhantomJS not found. You can install it with webshot::install_phantomjs(). ",
      "If it is installed, please make sure the phantomjs executable ",
      "can be found via the PATH variable."
    )
    return(NULL)
  }
  path.expand(path)
}

#is windows
is_windows <- function() .Platform$OS.type == "windows"
is_osx     <- function() Sys.info()[['sysname']] == 'Darwin'
is_linux   <- function() Sys.info()[['sysname']] == 'Linux'
is_solaris <- function() Sys.info()[['sysname']] == 'SunOS'
dir_exists <- function(path) utils::file_test('-d', path)
