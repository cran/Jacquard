Jacquard.cls <- function(Xlist,mafvec=NULL,
                         eps=1e-06,
                         delta.init=runif(9),
                         Mavg=NULL,
                         inner.iter=1000,
                         outer.iter=1000,
                         verbose=TRUE) {

  n <- nrow(Xlist[[1]])
  
  p <- 0
  
  for(i in 1:9) {
    p <- p + Xlist[[i]][2,1]
  }
  
  if(verbose) cat(n,"pairs",p,"SNPs\n")
  
  delta.init <- delta.init/sum(delta.init)
  
  if(is.null(mafvec)) stop("mafvec not supplied.")
  
  if(length(mafvec) != p) stop("Incorrect number of allele frequencies.")
  
  if(is.null(Mavg)) { # compute M if it is not specified
    Mtotal <- matrix(0,9,9)
    for(i in 1:p) {
      Mi     <- MakeM(mafvec[i])
      Mtotal <- Mtotal + Mi
    } 
    Mavg <- Mtotal/p
  }
  
  convergence <- matrix(numeric(n^2), ncol = n)
  
  delta <- list(length = 9)
  
  for (k in 1:9) {
    delta[[k]] <- matrix(numeric(n^2), ncol = n)
    rownames(delta[[k]]) <- rownames(Xlist[[1]])
    colnames(delta[[k]]) <- rownames(Xlist[[1]])
  }
  
  for(i in 1:n) {
    if(verbose) cat("Processing",i,"out of",n,"\n")
    for(j in 1:i) {
      G <- c(Xlist[[1]][i,j],
             Xlist[[2]][i,j],
             Xlist[[3]][i,j],
             Xlist[[4]][i,j],
             Xlist[[5]][i,j],
             Xlist[[6]][i,j],
             Xlist[[7]][i,j],
             Xlist[[8]][i,j],
             Xlist[[9]][i,j])
      g <- G/sum(G)
      if(i == j) {
        out <- solnp(delta.init, ls.obj, eqfun=eqn2, eqB = 1, ineqfun = NULL, 
                       ineqLB = NULL, ineqUB = NULL, 
                       LB = c(0,0,0,0,0,0,0,0,0), 
                       UB = c(1,1,1,1,1,1,1,1,1), 
                       control = list(trace = FALSE, tol = eps, 
                                      inner.iter = inner.iter, 
                                      outer.iter = outer.iter), g, Mavg)    
      } else {
        out <- solnp(delta.init, ls.obj, eqfun=eqn1, eqB = 1, ineqfun = NULL, 
                       ineqLB = NULL, ineqUB = NULL, 
                       LB = c(0,0,0,0,0,0,0,0,0), 
                       UB = c(1,1,1,1,1,1,1,1,1), 
                       control = list(trace = FALSE, tol = eps, 
                                      inner.iter = inner.iter, 
                                      outer.iter = outer.iter), g, Mavg)
      }
        
      if(out$convergence!=0) {
        if(verbose) cat("pair",i,j,"did not converge.\n")
        convergence[i,j] <- out$convergence
      }
      
      for(k in 1:9) {
        delta[[k]][i,j] <- out$pars[k]    
      }
      
      } # end for j
      
    } # end for i
      
    D3ct <- t(delta[[3]])
    D4ct <- t(delta[[4]])
    D5ct <- t(delta[[5]])
    D6ct <- t(delta[[6]])
      
    delta[[3]][upper.tri(delta[[3]])] <- D5ct[upper.tri(D5ct)] 
    delta[[5]][upper.tri(delta[[5]])] <- D3ct[upper.tri(D3ct)] 
      
    delta[[4]][upper.tri(delta[[4]])] <- D6ct[upper.tri(D6ct)] 
    delta[[6]][upper.tri(delta[[6]])] <- D4ct[upper.tri(D4ct)]
      
    D1ct <- t(delta[[1]])
    D2ct <- t(delta[[2]])
    D7ct <- t(delta[[7]])
    D8ct <- t(delta[[8]])
    D9ct <- t(delta[[9]])
      
    delta[[1]][upper.tri(delta[[1]])] <- D1ct[upper.tri(D1ct)] 
    delta[[2]][upper.tri(delta[[2]])] <- D2ct[upper.tri(D2ct)] 
    delta[[7]][upper.tri(delta[[7]])] <- D7ct[upper.tri(D7ct)] 
    delta[[8]][upper.tri(delta[[8]])] <- D8ct[upper.tri(D8ct)] 
    delta[[9]][upper.tri(delta[[9]])] <- D9ct[upper.tri(D9ct)] 

    names(delta) <- paste("J",1:9,sep="")

  return(list(delta=delta,convergence=convergence))
}
