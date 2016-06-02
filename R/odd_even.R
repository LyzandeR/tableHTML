#' Get the odd or even numbers from a numeric vector
#'
#' Get the odd or even numbers from a numeric vector
#' 
#' \code{odd} will extract the odd numbers from a vector.\cr \code{even} will extract the 
#'   even numbers from a vector
#'
#' @param vec A numeric atomic vector.
#'
#' @return A numeric atomic vector with the odd / even numbers 
#'         
#' @examples
#' odd(1:10)
#' even(1:10)
#' 
#' @rdname odd_even
#' @export
odd <- function(vec) {
 
 if(!is.numeric(vec)) stop('vec needs to be numeric')
 
 vec[vec %% 2 == 1]
}

#' @rdname odd_even
#' @export
even <- function(vec) {
 
 if(!is.numeric(vec)) stop('vec needs to be numeric')
 
 vec[vec %% 2 == 0]
}