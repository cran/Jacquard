CalculateTheta <- function(J,ind.sub=1:nrow(J[[1]])) {
  
  # Kinship coefficient
  
  theta1  <- J[[1]][ind.sub,ind.sub]  + 
    0.5*(J[[3]][ind.sub,ind.sub] + 
           J[[5]][ind.sub,ind.sub] + 
           J[[7]][ind.sub,ind.sub]) + 
    0.25*J[[8]][ind.sub,ind.sub]
  
  # A's inbreeding
  
  theta2 <- J[[1]][ind.sub,ind.sub] + 
    J[[2]][ind.sub,ind.sub] + 
    J[[3]][ind.sub,ind.sub] + 
    J[[4]][ind.sub,ind.sub]

  # At least one IBD out-of-three
  
  theta3  <- J[[1]][ind.sub,ind.sub] + 
    J[[2]][ind.sub,ind.sub] + 
    J[[3]][ind.sub,ind.sub] + 
    J[[5]][ind.sub,ind.sub] +
    J[[7]][ind.sub,ind.sub] +
    0.50*J[[4]][ind.sub,ind.sub] + 
    0.50*J[[6]][ind.sub,ind.sub] +
    0.50*J[[8]][ind.sub,ind.sub]
  
  theta4  <- 0.5*(J[[4]][ind.sub,ind.sub] - 
                    J[[6]][ind.sub,ind.sub]) 
  
  theta <- list(T1=theta1,T2=theta2,T3=theta3,T4=theta4)
  
  return(theta)
}
