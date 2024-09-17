mafvector <-
function(X) {
  gcount <- function(x,arg=0) {
    y <- sum(x==arg,na.rm=TRUE)
    return(y)
  }
  n0 <- apply(X,2,gcount,0)
  n1 <- apply(X,2,gcount,1)
  n2 <- apply(X,2,gcount,2)
  n  <- n0 + n1 + n2
  nt <- 2*n
  mafvec <- (n1+2*pmin(n0,n2))/nt  
}
