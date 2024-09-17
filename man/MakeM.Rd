\name{MakeM}
\alias{MakeM}
\title{Create matrix of conditional joint genotype probabilities}
\description{
   Function \code{MakeM} creates the matrix of conditional joint genotype probabilities for biallelic markers for a given allele frequency.
}
\usage{
MakeM(p)
}
\arguments{
  \item{p}{the allele frequency}
}
\value{a 9 by 9 matrix}
\references{
Csuros, M. (2014) Non-identifiability of identity coefficients at biallelic loci. Theoretical Population Biology 92, pp. 22--29.
doi: 10.1016/j.tpb.2013.11.001

Graffelman, J., Weir, B.S. and Goudet, J. (2024) Estimation of Jacquard's genetic identity coefficients with bi-allelic variants by constrained least-squares. Under review.
}
\author{Jan Graffelman (jan.graffelman@upc.edu)}

\examples{

set.seed(123)
p <- runif(1)
M <- MakeM(p)

}
\keyword{misc}
