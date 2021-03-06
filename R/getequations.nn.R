#' A function that gets the equations when given the breakpoints.
#' @param data The data being approximated
#' @param BPs The list of breakpoints
#' @return Coefficients of the equations, fitted values, and residuals.

getequations.nn <- function(data, BPs){
  noOfBP <- length(BPs)
  piecewise <- list(coeffs = matrix, BreakPoints = c(), fitted = matrix, residuals = matrix)
  piecewise$BreakPoints <- BPs


  if(noOfBP == 1){
    # if there is only one breakpoints
    # find both lines
    x1 <- as.matrix(data[data[,1] <= BPs[1], 1, drop = FALSE])
    x2 <- as.matrix(data[data[,1] >= BPs[1], 1, drop = FALSE])
    y1 <- as.matrix(data[data[,1] <= BPs[1], 2, drop = FALSE])
    y2 <- as.matrix(data[data[,1] >= BPs[1], 2, drop = FALSE])

    #line1 <- lm(y1~x1+0)
    #line1 <- lm(y1~x1)
    #line2 <- lm(y2~x2)
    if(ismidsection(x1)==TRUE){
      line1 <- lm(y1~x1+0)
      line2 <- lm(y2~x2)
      coeffs <- cbind(rbind(0, as.matrix(line1$coefficients)), line2$coefficients)
    }else{
      line1 <- lm(y1~x1)
      line2 <- lm(y2~x2+0)
      coeffs <- cbind(line1$coefficients, rbind(0, as.matrix(line2$coefficients)))
    }

    #piecewise$coeffs <- cbind(rbind(0, line1$coefficients), line2$coefficients)
    #piecewise$coeffs <- cbind(line1$coefficients, line2$coefficients)
    piecewise$coeffs <- coeffs
    piecewise$fitted <- rbind(line1$fitted.values, line2$fitted.values)
    piecewise$residuals <- rbind(line1$residuals, line2$residuals)

  }else{
    # otherwise, find one segment at a time
    i <- 0
    repeat{
      if(i == 0){
        # the first segment
        x1 <- as.matrix(data[data[,1] <= BPs[i+1], 1, drop = FALSE])
        y1 <- as.matrix(data[data[,1] <= BPs[i+1], 2, drop = FALSE])
        #line1 <- lm(y1~x1+0)
        #line1 <- lm(y1~x1)
      }else if(i == noOfBP){
        # finding the last segment
        x1 <- as.matrix(data[data[,1] >= BPs[i], 1, drop = FALSE])
        y1 <- as.matrix(data[data[,1] >= BPs[i], 2, drop = FALSE])
        #line1 <- lm(y1~x1)
      }else{
        # finding all the other segments in between
        x1 <- as.matrix(data[data[,1] >= BPs[i] & data[,1] <= BPs[i+1], 1, drop = FALSE])
        y1 <- as.matrix(data[data[,1] >= BPs[i] & data[,1] <= BPs[i+1], 2, drop = FALSE])
        #line1 <- lm(y1~x1)
      }

      if(ismidsection(x1)==TRUE){
        line1 <- lm(y1~x1+0)
        coeffs <- rbind(0, as.matrix(line1$coefficients))
      }else{
        line1 <- lm(y1~x1)
        coeffs <- as.matrix(line1$coefficients)
      }

      if(i == 0){
        #piecewise$coeffs <- rbind(0, as.matrix(line1$coefficients))
        #piecewise$coeffs <- as.matrix(line1$coefficients)
        piecewise$coeffs <- coeffs
        piecewise$fitted <- as.matrix(line1$fitted.values)
        piecewise$residuals <- as.matrix(line1$residuals)
      }else{
        piecewise$coeffs <- cbind(piecewise$coeffs, coeffs)
        piecewise$fitted <- rbind(piecewise$fitted, as.matrix(line1$fitted.values))
        piecewise$residuals <- rbind(piecewise$residuals, as.matrix(line1$residuals))
      }

      i <- i+1
      if(i>noOfBP) break()
    }
  }
  rownames(piecewise$coeffs) <- c("(Intercept)", "x1")
  piecewise
}
