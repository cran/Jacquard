BoxplotDelta <- function(J, ind.sub=1:nrow(J[[1]]), ...) {
  n <- nrow(J[[1]][ind.sub,ind.sub])
  npairs <- 0.5*n*(n-1)
  M <- matrix(NA,npairs,11)
  for(i in 1:length(J)) {
    Jsub <- J[[i]][ind.sub,ind.sub]
    Jv   <- Jsub[upper.tri(Jsub)]  
    M[,i] <- Jv
  }
  ll <- ncol(J[[1]])
  M[1:ll,10] <- diag(J[[1]])
  M[1:ll,11] <- diag(J[[7]])
  colnames(M) <- c(paste("J",1:9,sep=""),paste("D",c(1,7),sep=""))
  boxplot(M,ylab="Jacquard coefficient",...)
}
