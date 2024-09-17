MatrixTau <- function(Xgen,verbose=TRUE) {
  
  CalculateTaus <- function(G) {
    tau1a <- (G[3] + G[8])/2 + G[5]/4 + G[6]
    tau1b <- (G[4] + G[9])/2 + G[5]/4 + G[7]
    tau1  <- (tau1a + tau1b)/2
    tau2a <- 0.5*(G[4] + G[5] + G[8])
    tau2b <- 0.5*(G[3] + G[5] + G[9])
    tau3  <- 0.25*(G[8] - G[4] + G[9] - G[3])
    tau4  <- 0.50*(G[6] - G[7])
    tauvec <- c(tau1a,tau1b,tau2a,tau2b,tau3,tau4)
    names(tauvec) <- c("Tau1a","Tau1b","Tau2a","Tau2b","Tau3","Tau4")
    return(tauvec)
  }  
  
  n <- nrow(Xgen)
  
  Tau.mom <- list(length = 6)

  for (k in 1:6) Tau.mom[[k]] <- matrix(numeric(n^2), ncol = n)

  for(i in 1:n) {
    if(verbose) cat("Processing",i,"out of",n,"\n")
    for(j in 1:n) {
      i1 <- Xgen[i,]
      i2 <- Xgen[j,]
      GC <- MakeTable(i1,i2)
      G  <- Table2Vector(GC,one.is.minor=TRUE)
      G  <- G/sum(G)
      taus <- CalculateTaus(G)
      for(k in 1:6) {
        Tau.mom[[k]][i,j] <- taus[k]
      }
    }
  }
  return(Tau.mom)
}
