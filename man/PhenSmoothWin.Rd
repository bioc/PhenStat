\name{PhenSmoothWin}
\alias{PhenSmoothWin}

\title{
Complementary function for "testDataset" that allows soft windowing
}
\description{
  Running this function allows a range of windowing on the analysis method.  The bandwidth and sharpness of the window are controlled by two parameters l and k respectively.
}
\usage{
PhenSmoothWin(
  testDatasetObj,
  l = 4:5,
  k = 1:2,
  min.obs   = 50,
  criteria  = 'AICc',
  method    = 'enet',
  threshold = 10 ^ -18,
  plot      = FALSE
)
}
\arguments{
  \item{testDatasetObj}{
  the output of testDataset function
}
  \item{l}{
  a positive vector of at least one value for the bandwidth parameter. The function searches for the best window among these values. For example, l=1:10 searches for the optimal windows in the radius of 10 (20 units since the l is symmetric).
}
  \item{k}{
  a positive vector of at least one value for the shape parameter. The function searches for the optimal sharpness among these values.
}
  \item{min.obs}{
  the minimum number of observations that must be included in the window. The default is 50
}
  \item{criteria}{
  select among 'AICc', 'BIC', 'GCV' and 'Cp' for the penalized change point selection criteria. The default is 'AICc'
  }
  \item{method}{
  select between 'enet' and 'alasso' for the lasso and adaptive-lasso respectively in the penalized change point detection algorithm. The default is 'enet'
  }
  \item{threshold}{
  a single value. The threshold for the weighting function that pushes the weights to zero. Default is 10^-18
}
  \item{plot}{
  logical flag. Setting to TRUE shows some plots. Data must be sorted on Batch to get neat plots.
}
}

\value{
  \item{finalk }{The optimal value for k }
  \item{finall }{The optimal value for l }
  \item{finalModel }{a list of the single element that contains the final model }
  \item{model.l }{a list that contains all models for the different values of l }
  \item{model.k }{a list that contains all models for the different values of k }
  \item{x }{a data frame that contains the explanatory variables (data) }
  \item{y }{response data }
  \item{t }{batch data that is converted to UNIX time stamp}
  \item{k }{similar to the input parameter k}
  \item{l }{similar to the input parameter l }
  \item{min.obs }{similar to the input parameter min.obs}
  \item{mutInd }{The index of the mutant animals }
  \item{threshold }{similar to the input parameter threshold}
  \item{plot }{similar to the input parameter plot}
}

\author{
  Hamed Haselimashhadi <hamedhm@eb.ac.uk>
}

\note{
	*** Hard windowing can be achieved by fixing "l"" to the bandwidth of interest and setting "k" to a big value such as 20.
}

\seealso{
 \code{\link{testDataset}}
}
\examples{
	\dontrun{
	    set.seed(123456)
	    par(ask=FALSE)
	    # Mixed Model framework
	    file    <- system.file("extdata", "test1.csv", package = "PhenStat")
	    csvFile <- read.csv(file, na.strings = '-')
	    data    <- csvFile[order(as.Date(csvFile$Assay.Date)), ]
	    test    <- PhenList(dataset = data,
	                        testGenotype = "Sparc/Sparc",
	                        outputMessages = FALSE)
	    result <- testDataset(test,
	                          depVariable = "Lean.Mass",
	                          outputMessages = FALSE)
	    SofWinresult = PhenSmoothWin(
	      testDatasetObj = result,
	      plot = TRUE,
	      l = 10,
	      k = 5
	    )
	    SofWinresult$finalModel$models[[1]]
	}
}
\keyword{SoftWindowing }
\keyword{HardWindowing }
