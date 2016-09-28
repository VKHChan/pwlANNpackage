#' A function that finds the breakpoints (BP) with the minimum ssr, given the number of desired BP, by searching through the given ssr matrix.
#' @param SSRMatrix The SSR matrix
#' @param noOfBP The desired number of breakpoint
#' @param l The minimum distance between breakpoints
#' @param index The index of the to start the search from
#' @param dataSize The data size for the data that is being approximated
#' @return The list of breakpoints and the SSR

findBP <- function(SSRMatrix, noOfBP, l, index, dataSize){

  result <- list(minssr=(max(SSRMatrix, na.rm=TRUE)*1000), BP = c())
  #If more than one breakpoint is desired
  if(noOfBP > 1){
    j <- index+l+1
    repeat{
      ssr1<- SSRMatrix[index, j]

      if(ssr1 > result$minssr) break()

      result2 <- findBP(SSRMatrix, noOfBP-1, l, j, dataSize)
      totssr <- ssr1 + result2$minssr

      if(totssr < result$minssr){
        result$minssr <- totssr
        result$BP <- c(j,result2$BP)
      }
      j <- j+1
      if(j>=(dataSize-(l+1)*noOfBP)) break()
    }
  }else if(noOfBP==1){
    i <- index+l+1
    repeat{
      ssr<- SSRMatrix[index, i]+ SSRMatrix[i, dataSize]

      if(ssr < result$minssr){
        result$minssr<- ssr
        result$BP <- c(i)
      }
      i<- i+l
      if(i> (dataSize-l-1)) break()
    }
  }
  result
}
