#' A function that takes a sample of the given dataset
#' @param data The data that is being sampled
#' @param samplesize The desired sample size
#' @return A subset with the specified sample size of the given dataset


takesample <- function(data, samplesize){

  i <- round(nrow(data)/samplesize)
  sample <- data[seq(1, nrow(data), i),]

  sample
}
