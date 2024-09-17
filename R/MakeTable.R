MakeTable <- function(i1,i2,coding=c(0,1,2)) {
  c00 <- sum(i1==coding[1] & i2==coding[1])
  c01 <- sum(i1==coding[1] & i2==coding[2])
  c02 <- sum(i1==coding[1] & i2==coding[3])
  c10 <- sum(i1==coding[2] & i2==coding[1])
  c11 <- sum(i1==coding[2] & i2==coding[2])
  c12 <- sum(i1==coding[2] & i2==coding[3])
  c20 <- sum(i1==coding[3] & i2==coding[1])
  c21 <- sum(i1==coding[3] & i2==coding[2])
  c22 <- sum(i1==coding[3] & i2==coding[3])
  TAB <- matrix(c(c00,c01,c02,c10,c11,c12,c20,c21,c22),
                nrow=3,byrow=TRUE)
  colnames(TAB) <- c("0","1","2")
  rownames(TAB) <- colnames(TAB)
  return(TAB)
}
