% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.pwlnn.results.R
\name{plots.pwlnn.results}
\alias{plots.pwlnn.results}
\title{A function that prinst and saves the predicted output Vs actual output graphs.
Three graphs will be given: Actual output Vs PWL output, Actual output Vs ANN output, and Actual output Vs PWL and ANN output.
Graphs will be saved in png format.}
\usage{
plots.pwlnn.results(Inputdata, ANNOutput, ActualOutput, hiddenW, outputW,
  allpwlfull, actfun)
}
\arguments{
\item{Inputdata}{The input data used to trained the ANN model.}

\item{ANNOutput}{The output predicted by the ANN model.}

\item{ActualOutput}{The target output used to train the ANN model.}

\item{hiddenW}{The weights of the hidden neuron in matrix form, columns are the hidden nodes, rows are each of the weights to the hidden node. Only one hidden layer is supported.}

\item{outputW}{The output weights of the output layer. The output activation function is assumed to be linear.}

\item{allpwlfull}{The full set of pwl equations for all hidden nodes.}

\item{actfun}{The activation function of the hidden layer. It can be either "sigmoid" or "hyperbolic". Default function is "sigmoid".}
}
\value{
plots saved in the work directory in PNG format.
}
\description{
A function that prinst and saves the predicted output Vs actual output graphs.
Three graphs will be given: Actual output Vs PWL output, Actual output Vs ANN output, and Actual output Vs PWL and ANN output.
Graphs will be saved in png format.
}

