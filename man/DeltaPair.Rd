\name{DeltaPair}
\alias{DeltaPair}

\title{Extract estimates of Jacquard coefficients for a given pair}

\description{
Function \code{DeltaPair} extracts from the list object of all pairwise Jacquard coefficients the
set of coefficients of a given pair (i,j).
}

\usage{
DeltaPair(Delta, i, j, digits = 7)
}
\arguments{
  \item{Delta}{A list with nine matrices of pairswise coefficients.}
  \item{i}{Index of the first individual.}
  \item{j}{Index of the second individual.}
  \item{digits}{Number of digits to which the coefficients are rounded.}
}

\value{A vector with nine elements}

\author{Jan Graffelman (jan.graffelman@upc.edu)}

\seealso{
   \code{\link{PairwiseList}},\code{\link{Jacquard.cls}}
}

\examples{
 data(DeltaSimulatedPedigree)
 DeltaPair(DeltaSimulatedPedigree,1,2)
}

\keyword{manip}

