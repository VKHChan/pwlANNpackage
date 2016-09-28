#' A function that returns the sigmoid value of the given input.
#' @param data The input data
#' @return The sigmoid value of the given data

calculateSigmoid <- function(data){

  sigmoiddata <-  1/(1+exp(-data))

  sigmoiddata
}
