Table2Vector <- function(TGP,one.is.minor=TRUE) {
  
  if(one.is.minor==TRUE) {
    f0000 <- TGP[1,1]
    f0001 <- TGP[1,2]
    f0011 <- TGP[1,3]
    
    f0100 <- TGP[2,1]
    f0101 <- TGP[2,2]
    f0111 <- TGP[2,3]
    
    f1100 <- TGP[3,1]
    f1101 <- TGP[3,2]
    f1111 <- TGP[3,3]
  } else {
    f1111 <- TGP[1,1]
    f1101 <- TGP[1,2]
    f1100 <- TGP[1,3]
    
    f0111 <- TGP[2,1]
    f0101 <- TGP[2,2]
    f0100 <- TGP[2,3]
    
    f0011 <- TGP[3,1]
    f0001 <- TGP[3,2]
    f0000 <- TGP[3,3]
  }
  
  fvector <- c(f0000,f1111,f1101,
               f0111,f0101,f1100,
               f0011,f0100,f0001)
  names(fvector) <- c("f0000","f1111","f1101",
                      "f0111","f0101","f1100",
                      "f0011","f0100","f0001")
  return(fvector)
}