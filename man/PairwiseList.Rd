\name{PairwiseList}
\alias{PairwiseList}
\title{
Create a Pairwise List of Jacquard Coefficients
}
\description{
  Function \code{PairwiseList} processes the list structure of Jacquard coefficients
  and converts it into to table of pairs of individuauls with their Jacquard coefficients.
}
\usage{
PairwiseList(X, digits = 3)
}
\arguments{
  \item{X}{A list structure of nine matrices of Jacquard coefficients.}
  \item{digits}{Jacquard coefficients are rounded to the given number of digits.}
}
\value{A matrix}

\author{Jan Graffelman (jan.graffelman@upc.edu)}

\seealso{
   \code{\link{Jacquard.cls}}
}
\examples{
data(DeltaSimulatedPedigree)

ii <- 1:3

SubSet <- list(length = 9)
for (k in 1:9) {
  SubSet[[k]] <- matrix(numeric(3^2), ncol = 3)
  SubSet[[k]] <- DeltaSimulatedPedigree[[k]][ii,ii]
}

List <- PairwiseList(SubSet)

print(List)
}

\keyword{manip}
