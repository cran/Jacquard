\name{BoxplotTheta}
\alias{BoxplotTheta}
\title{
Boxplots of Relateness parameters
}
\description{
 Function \code{BoxplotTheta} makes boxplots of relatedess parameters from a
 list object containing estimates of pairwise relatedness parameters.
}
\usage{
BoxplotTheta(KS, ind.sub = 1:nrow(KS[[1]]), ...)
}

\arguments{
  \item{KS}{A list object with four fields of matrices: kinship, inbreeding, T3 and T4.}
  \item{ind.sub}{Index for subsetting the individuals}
  \item{\dots}{Additional arguments passed on to the function \code{boxplot}.}
}

\details{
  For individual inbreeding coefficients, two boxplots are made, the first based on the diagonal of T2,
  the second on the row means of T2. The remaining boxplots (kinship, T3 and T4) are all pairwise, and
  exclude self-pairs.
}

\value{NULL}

\references{
Csuros, M. (2014) Non-identifiability of identity coefficients at biallelic loci. Theoretical Population
Biology 92, pp. 22-29. \doi{10.1016/j.tpb.2013.11.001}.

Graffelman, J., Weir, B.S. and Goudet, J. (2024) Estimation of Jacquard's genetic identity coefficients with
bi-allelic variants by constrained least-squares. Preprint at bioRxiv \doi{10.1101/2024.03.25.586682}.
}

\author{Jan Graffelman (jan.graffelman@upc.edu)}

\seealso{
\code{\link{CalculateTheta}}
}
\examples{
data(DeltaSimulatedPedigree)
Theta <- CalculateTheta(DeltaSimulatedPedigree)
BoxplotTheta(Theta)
}

\keyword{graphics}
