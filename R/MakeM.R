MakeM <-
function(p) {
  q <- 1 - p
  pq <- p*q
  p2 <- p*p
  q2 <- q*q
  q3 <- q*q*q
  p3 <- p*p*p
  q4 <- q2*q2
  p4 <- p2*p2 
  c1 <- c(q, p, 0,     0,     0,        0,    0,    0,     0)
  c2 <- c(q2,p2,0,     0,     0,        pq,   pq,   0,     0)
  c3 <- c(q2,p2,pq,    0,     0,        0,    0,    0,     pq)
  c4 <- c(q3,p3,2*p2*q,0,     0,        p*q2, p2*q, 0,     2*p*q2)
  c5 <- c(q2,p2,0,     pq,    0,        0,    0,    pq,    0)
  c6 <- c(q3,p3,0,     2*p2*q,0,        p2*q, p*q2, 2*p*q2,0)
  c7 <- c(q2,p2,0,     0,     2*pq,     0,    0,    0,     0)
  c8 <- c(q3,p3,p2*q,  p2*q,  p2*q+p*q2,0,    0,    p*q2,  p*q2)
  c9 <- c(q4,p4,2*p3*q,2*p3*q,4*p2*q2,  p2*q2,p2*q2,2*p*q3,2*p*q3)  
  M <- cbind(c1,c2,c3,c4,c5,c6,c7,c8,c9)
  return(M)
}
