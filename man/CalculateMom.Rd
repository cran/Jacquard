\name{CalculateMom}
\alias{CalculateMom}
\title{
  Calculate Moment Estimators for Relatedness Parameters
}
\description{
  Function \code{CalculateMom} computes moment estimators for a set of
  relatedness parameters (kinship, inbreeding, least one IBD out of three
  and T4) using the genotype data and the allele frequencies.
}
\usage{
CalculateMom(Xgen, mafvec, ind.sub = 1:nrow(Xgen), verbose = TRUE)
}
\arguments{
  \item{Xgen}{Genotype data coded in (0,1,2) format}
  \item{mafvec}{A vector of minor allele frequencies}
  \item{ind.sub}{Index for subsetting individuals}
  \item{verbose}{Print output on the progress of the algorithm if \code{verbose=TRUE}.}
}

\value{A list object with four fields:

\item{T1}{The pairwise coancestry or kinship coefficients (symmetric)}
\item{T2}{The pairwise inbreeding coefficients (non-symmetric)}
\item{T3}{The pairwise Least one IBD out of three (symmetric)}
\item{T4}{T4 (skew-symmetric)}

}

\references{
Csuros, M. (2014) Non-identifiability of identity coefficients at biallelic loci. Theoretical Population
Biology 92, pp. 22-29. \doi{10.1016/j.tpb.2013.11.001}.
}

\author{Jan Graffelman (jan.graffelman@upc.edu)}

\seealso{
\code{\link{BoxplotTheta}}
}

\examples{
data(SimulatedPedigree)
Xgen <- as.matrix(SimulatedPedigree[,6:ncol(SimulatedPedigree)])
mafvec <- mafvector(Xgen)
Theta.mom <- CalculateMom(Xgen[1:10,],mafvec)
}

\keyword{misc}
