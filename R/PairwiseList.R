PairwiseList <-
function(X,digits=3) {
  Res <- NULL
  RN <- matrix(rep(rownames(X[[1]]),ncol(X[[1]])),ncol=ncol(X[[1]]))
  CN <- t(RN)
  for(i in 1:length(X)) {
    ss <- X[[i]][lower.tri(X[[i]])]
    Res <- cbind(Res,ss)
  }
  Res <- round(Res,digits)
  I1 <- CN[lower.tri(CN)]
  I2 <- RN[lower.tri(RN)]
  rownames(Res) <- paste(I1,I2,sep="-")
  colnames(Res) <- paste("J",1:9,sep="")
  return(Res)
}
