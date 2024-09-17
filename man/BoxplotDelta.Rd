\name{BoxplotDelta}
\alias{BoxplotDelta}
\title{
  Boxplots of Jacquard Coefficients 
}
\description{
  Function \code{BoxplotDelta} produces boxplots of the Jacquard coefficients from
  a list structure containing nine matrices with the pairwise coefficients. The
  diagonals of J1 and J7 are plotted in separate boxplots.
}
\usage{
BoxplotDelta(J, ind.sub = 1:nrow(J[[1]]), ...)
}

\arguments{
  \item{J}{The list structure with nine fields consisting of nine matrices}
  \item{ind.sub}{Index for subsetting the individuals}
  \item{\dots}{Additional arguments to pass on to \code{boxplot}}
}

\value{NULL}

\author{Jan Graffelman (jan.graffelman@upc.edu)}

\examples{
  data(DeltaSimulatedPedigree)
  BoxplotDelta(DeltaSimulatedPedigree)
}

\keyword{graphics}
