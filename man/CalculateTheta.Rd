\name{CalculateTheta}
\alias{CalculateTheta}
\title{
Calculate Relatedness Paramenters from Jacquard Coefficients tA Capitalized Title (ideally limited to 65 characters)
}
\description{
  Function \code{CalculateTheta} calculates five identifiable relatedness parameters from a
  list structure containing nine matrices of Jacquard coefficients.
}
\usage{
CalculateTheta(J, ind.sub = 1:nrow(J[[1]]))
}

\arguments{
  \item{J}{A list structure with nine matrices of pairwise Jacquard coefficients}
  \item{ind.sub}{Index for subsetting the individuals}
}

\details{
   \code{CalculateTheta} produces four matrices according to the expressions in
   Graffelman et al. (2024).
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

Graffelman, J., Weir, B.S. and Goudet, J. (2024) Estimation of Jacquard's genetic identity coefficients with
bi-allelic variants by constrained least-squares. Preprint at bioRxiv \doi{10.1101/2024.03.25.586682}.

}

\author{Jan Graffelman (jan.graffelman@upc.edu)}

\seealso{
  \code{\link{Jacquard.cls}}
}

\examples{
data(DeltaSimulatedPedigree)
Theta <- CalculateTheta(DeltaSimulatedPedigree)
}

\keyword{misc}
