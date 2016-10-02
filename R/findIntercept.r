#' A function that determines the x-intercept of two given lines.
#' @param intercept1 The intercept of line 1
#' @param gradient1 The gradient of line 1
#' @param intercept2 The intercept of line 2
#' @param gradient2 The gradient of line 2
#' @return The x-intercept of the two given lines

findIntercept <- function(intercept1, gradient1, intercept2, gradient2){

  interceptx <- (intercept2-intercept1)/(gradient1-gradient2)
  intercepty <- intercept1+ gradient1*interceptx
  interceptx

}
