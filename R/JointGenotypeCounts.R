JointGenotypeCounts <- function(X.gen, one.is.minor = TRUE) {
  n <- nrow(X.gen)
  if(is.null(rownames(X.gen))) {
    IDs <- paste("I",1:nrow(X.gen),sep="")
  } else {
    IDs <- rownames(X.gen)
  }
  gtc <- list(length = 9)
  for (k in 1:9) {
    gtc[[k]] <- matrix(numeric(n^2), ncol = n)
    rownames(gtc[[k]]) <- IDs
    colnames(gtc[[k]]) <- IDs
  }
  for(i in 1:n) {
    for(j in 1:i) {
      i1 <- X.gen[i,]
      i2 <- X.gen[j,]
      GC <- MakeTable(i1,i2)
      G  <- Table2Vector(GC,one.is.minor)
      for(k in 1:9) {
        gtc[[k]][i,j] <- G[k]
      }
    }
  }
  names(gtc) <- c("f0000", "f1111", "f1101", "f0111", "f0101", 
                  "f1100", "f0011", "f0100", "f0001")
  return(gtc)
}
