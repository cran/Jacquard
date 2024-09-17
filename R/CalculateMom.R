CalculateMom <- function(Xgen,mafvec,ind.sub=1:nrow(Xgen),verbose=TRUE) {
  
  Tau.mom <- MatrixTau(Xgen,verbose=verbose) 
  
  mu1 <- mean(mafvec)
  mu2 <- mean(mafvec*mafvec)
  mu3 <- mean(mafvec*mafvec*mafvec)
  mu4 <- mean(mafvec*mafvec*mafvec*mafvec)

  v2 <- mu1 - mu2
  v3 <- mu1 - 3*mu2 + 2*mu3
  
  # Kinship coefficient

  tau1 <- (Tau.mom[[1]][ind.sub,ind.sub] 
           + Tau.mom[[2]][ind.sub,ind.sub])/2
  
  th1  <- 1 - tau1/v2
  
  # A's inbreeding
  
  th2a <- 1 - (Tau.mom[[3]][ind.sub,ind.sub])/v2
  
  # B's inbreeding
  
#  th2b <- 1 - (Tau.mom[[4]][ind.sub,ind.sub])/v2
  
  th3  <- 1 - (Tau.mom[[5]][ind.sub,ind.sub])/v3
  
  th4  <- (Tau.mom[[6]][ind.sub,ind.sub])/v3
    
  theta <- list(T1=th1,T2=th2a,T3=th3,T4=th4)
  
}
