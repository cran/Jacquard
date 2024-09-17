BoxplotTheta <- function(KS,ind.sub=1:nrow(KS[[1]]),...) {
  n <- nrow(KS[[1]][ind.sub,ind.sub])
  npairs <- 0.5*n*(n-1)   
  M <- matrix(NA,npairs,5)
  subset <- KS[[1]][ind.sub,ind.sub]
  subset <- subset[lower.tri(subset)]
  M[,1] <- subset
  
  ks1diag <- diag(KS[[2]][ind.sub,ind.sub])
  nas <- nrow(M) - length(ks1diag)
  navec <- rep(NA,nas)
  ks1diag <- c(ks1diag,navec)
  
  M[,2] <- ks1diag
 
  Copy <- KS[[2]][ind.sub,ind.sub]
  diag(Copy) <- NA
  ks2diag <- rowMeans(Copy,na.rm=TRUE)
  nas <- nrow(M) - length(ks2diag)
  navec <- rep(NA,nas)
  ks2diag <- c(ks2diag,navec)  
  
  M[,3] <- ks2diag
  
  subset <- KS[[3]][ind.sub,ind.sub]
  subset <- subset[lower.tri(subset)]
  M[,4] <- subset

  subset <- KS[[4]][ind.sub,ind.sub]
  subset <- subset[lower.tri(subset)]
  M[,5] <- subset
  
  colnames(M) <- c("T1","T2d","T2","T3","T4")
  nn <- c(expression(theta[1]),expression(theta[2]),expression(theta["2i"]),
          expression(theta[3]),expression(theta[4]))
  boxplot(M,names=nn,ylab="Relatedness parameter",...)
}
