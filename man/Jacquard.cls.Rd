\name{Jacquard.cls}
\alias{Jacquard.cls}
\title{
Estimation of Jacquard coefficients by Constrained Least Squares
}
\description{
Function \code{Jacquard.cls} estimates the nine condensed Jacquard coefficients of a pair of individuals
using their joint genotype probabilities and the allele frequencies using constrained least squares.
}
\usage{
Jacquard.cls(Xlist, mafvec = NULL, eps = 1e-06, delta.init = runif(9), Mavg = NULL,
             inner.iter = 1000, outer.iter = 1000, verbose = TRUE)
}
\arguments{
  \item{Xlist}{A list object with nine fields containing the matrices with joint genotype counts.}
  \item{mafvec}{A vector with the minor allele frequencies for all genetic variants.}
  \item{eps}{Tolerance criterion for the solver (\code{eps=1e-06} by default).}
  \item{delta.init}{Initial vector of estimates for the nine condensed Jacquard coefficients.}
  \item{Mavg}{A nine by nine matrix of conditional probabilities, allele frequency dependent. This matrix
  is calculated by \code{Jacquard.cls} if it is not specified.}
  \item{inner.iter}{Maximum number of inner iterations for the solver (1000 by default).}
  \item{outer.iter}{Maximum number of outer iterations for the solver (1000 by default).}
  \item{verbose}{Print output on the progress of the algorithm if \code{verbose=TRUE}.}
}
\details{
  Function \code{Jacquard.cls} relies on the solver \code{solnp} from the Rsolnp package.
}

\value{A list object with fields:
  \item{delta}{A list with nine matrices of estimates of pairwise Jacquard coefficients.}
  \item{convergence}{A matrix with the convergence status for each pair (0 = converged; 1 = not converged).}
}

\references{
Graffelman, J., Weir, B.S. and Goudet, J. (2024) Estimation of Jacquard's genetic identity coefficients with
bi-allelic variants by constrained least-squares. Preprint at bioRxiv \doi{10.1101/2024.03.25.586682}.

Ghalanos, A. and Theussl, S. (2015) Rsolnp: General Non-linear Optimization Using Augmented Lagrange Multiplier Method.
R package version 1.16. \href{https://cran.r-project.org/package=Rsolnp}{https://cran.r-project.org/package=Rsolnp}
}

\author{Jan Graffelman (jan.graffelman@upc.edu)}

\seealso{
   \code{\link{JointGenotypeCounts}}
}
\examples{
data(SimulatedPedigree)
Xgen <- as.matrix(SimulatedPedigree[,6:ncol(SimulatedPedigree)])

data(GTC)

mafvec <- mafvector(Xgen)

ii <- 1:3

GTCsubset <- list(length = 9)
for (k in 1:9) {
  GTCsubset[[k]] <- matrix(numeric(3^2), ncol = 3)
  GTCsubset[[k]] <- GTC[[k]][ii,ii]
}

output <- Jacquard.cls(GTCsubset,mafvec=mafvec,
                       eps=1e-06)
Delta.cls <- output$delta
print(Delta.cls)
print(output$convergence)

# A particular estimate of a Jacquard coefficient for a particular pair can
# be extracted from Delta.cls
#
# E.g., Delta\_9 of the first pair of individuals (1,2) can be extracted by
#

D9_12 <- Delta.cls[[9]][1,2]

}

\keyword{optimize}
