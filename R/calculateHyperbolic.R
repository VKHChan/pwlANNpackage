#' A function that returns the hyperbolic value of the given input.
#' @param data The input data
#' @return The hyperboic value of the given data

calculateHyperbolic <- function(data){

  hyperbolicdata <-  (exp(data)-exp(-data))/(exp(data)+exp(-data))

  hyperbolicdata
}
