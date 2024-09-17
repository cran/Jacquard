\name{mafvector}
\alias{mafvector}
\title{
Calculate the Minor Allele Frequency (MAF) for a matrix with (0,1,2) genotype data
}
\description{
  Function \code{mafvector} calculates genotype counts columnwise and determines
  the minor allele frequency for each column.
}
\usage{
mafvector(X)
}
\arguments{
  \item{X}{A matrix of with (0,1,2) genotype data, individuals in rows, markers in columns.}
}

\details{
  \code{mafvector} calculates the frequency of the minor allele irrespective of the coding; i.e.,
  irrespective of whether the genotype data represent major or minor allele counts. Missing values
  are discarded for the calculation of the MAF.
}
\value{a vector}

\author{Jan Graffelman (jan.graffelman@upc.edu)}

\examples{
  data(SimulatedPedigree)
  p <- mafvector(SimulatedPedigree[,1:10])
  print(p)
}

\keyword{misc}

