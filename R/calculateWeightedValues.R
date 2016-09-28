#' A function that takes an input matrix and a weight matrix and return a weighted input matrix.
#' @param Data The input data
#' @param WeightMatrix The weight values
#' @return The weighted Input

calculateWeightedValues <- function(Data, WeightMatrix){

  #the last row of the weight matrix is the biases
  #add a row of 1's to multiply with the bias
  Data <- cbind(Data, rep(1, nrow(Data)))


  Data <- as.matrix(Data)
  WeightMatrix <- as.matrix(WeightMatrix)
  WeightedInput <- Data%*%WeightMatrix

  WeightedInput
}
