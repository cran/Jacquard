\name{SimulatedPedigree}
\alias{SimulatedPedigree}
\docType{data}
\title{
Simulated SNP data according to a Pedigree
}
\description{
  A matrix containing 111 individuals (rows), pedigree information (first five columns) and 20.000 single
  nucleotide polymorphisms (remaining columns) coded in (0,1,2) format and simulated according to a
  pedigree of 20 unrelated founders with six posterior generations. 
}
\usage{data("SimulatedPedigree")}
\format{A data frame containing 111 rows and 20.005 columns.}
\details{
  The SNP data was generated using the JGTeach package. The genotype counts in the object SimulatedPedigree
  represent the counts of the minor allele.
}
\references{
Goudet, J. (2022) JGTeach: JG Teaching material. R package version 0.1.9. https://github.com/jgx65

Graffelman, J., Weir, B.S. and Goudet, J. (2024) Estimation of Jacquard's genetic identity coefficients with
bi-allelic variants by constrained least-squares. Preprint at bioRxiv \doi{10.1101/2024.03.25.586682}.
}
\examples{
data(SimulatedPedigree)
}
\keyword{datasets}
