#' A function that prints and saves all the plots for the hidden nodes with the pwl functions. Plots will be saved as PNG format.
#' @param Inputdata The input data used to trained the ANN model.
#' @param ANNOutput The output predicted by the ANN model.
#' @param ActualOutput The target output used to train the ANN model.
#' @param hiddenW The weights of the hidden neuron in matrix form, columns are the hidden nodes, rows are each of the weights to the hidden node. Only one hidden layer is supported.
#' @param outputW The output weights of the output layer. The output activation function is assumed to be linear.
#' @param actfun The activation function of the hidden layer. It can be either "sigmoid" or "hyperbolic". Default function is "sigmoid".
#' @param allpwlfull The full set of pwl equations for all hidden nodes.
#' @return plots saved in the work directory in PNG format.
#' @export

plots.pwlnn.nodes <- function(Inputdata, ANNOutput, ActualOutput, hiddenW, outputW, allpwlfull, actfun){
  noOfnode <- ncol(hiddenW)

  # check what activation was given
  if(actfun=="sigmoid"){
    constraint <- 0.5
    activation <- "calculateSigmoid"
  }else if(actfun=="hypoberlic"){
    constraint <- 0
    activation <- "calculateHyperbolic"
  }else{
    stop("Activation function can be either be sigmoid or hypobolic.\n Acticaation function by default is sigmoid function")
  }

  # get the activation function
  activationfunction <- get(activation)
  # calculate weighted input
  weightedInput <- as.data.frame(calculateWeightedValues(Inputdata, hiddenW))
  sigmoid <- activationfunction(weightedInput)

  # print the pwl graphs for each node
  i <- 1
  repeat{
    node <- paste0("node", i)
    data <- cbind(weightedInput[,i], sigmoid[,i])

    title <- paste0("Node ", i)
    png(paste0(title,".png"), width = 900, height = 600)
    plots.pwl(allpwlfull[[node]], data)
    title(main = title, xlab = "Weighted Input", ylab = "Activated Values")

    dev.off()

    i <- i+1
    if(i>noOfnode) break()

  }
}
