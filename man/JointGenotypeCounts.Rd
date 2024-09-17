\name{JointGenotypeCounts}
\alias{JointGenotypeCounts}
\title{
Calculate joint pairwise Genotype Counts
}
\description{
Function \code{JointGenotypeCounts} counts for each pair of individuals in the database their
nine joint genotype counts
}

\usage{
JointGenotypeCounts(X.gen, one.is.minor = TRUE)
}

\arguments{
  \item{X.gen}{A matrix with genotype data coded in (0,1,2) format}
  \item{one.is.minor}{If \code{TRUE} the genotype data represent the count
  of the minor allele for each marker, if \code{FALSE} the major allele.}
}

\value{A list object with nine fields containing:
 \item{f0000}{Matrix of (0/0,0/0) counts for all pairs}
 \item{f1111}{Matrix of (1/1,1/1) counts for all pairs}
 \item{f1101}{Matrix of (1/1,0/1) counts for all pairs}
 \item{f0111}{Matrix of (0/1,1/1) counts for all pairs}
 \item{f0101}{Matrix of (0/1,0/1) counts for all pairs}
 \item{f1100}{Matrix of (1/1,0/0) counts for all pairs}
 \item{f0011}{Matrix of (0/0,1/1) counts for all pairs}
 \item{f0100}{Matrix of (0/1,0/0) counts for all pairs}
 \item{f0001}{Matrix of (0/0,0/1) counts for all pairs}
}

\references{
  Graffelman, J., Weir, B.S. and Goudet, J. (2024) Estimation of Jacquard's genetic identity coefficients
  with bi-allelic variants by constrained least-squares. Preprint at bioRxiv. \doi{10.1101/2024.03.25.586682}
}

\author{Jan Graffelman (jan.graffelman@upc.edu)}

\examples{
data(SimulatedPedigree)
JC <- JointGenotypeCounts(SimulatedPedigree[1:3,1:100])
print(JC)
}
\keyword{manip}
