DeltaPair <-
function(Delta,i,j,digits=7) {
  ndelta <- length(Delta)
  estimatespair <- numeric(ndelta)
  for(k in 1:ndelta) {
    estimatespair[k] <- Delta[[k]][i,j]
  }
  names(estimatespair) <- paste("J",1:ndelta,sep="")
  estimatespair <- round(estimatespair,digits=digits)
  return(estimatespair)
}
