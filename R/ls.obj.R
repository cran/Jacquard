ls.obj <-
function(delta, g = runif(9), Mavg = matrix(0.1,9,9)) {
  sum(g*g) - 2*as.numeric(g%*%Mavg%*%delta) + as.numeric(delta%*%t(Mavg)%*%Mavg%*%delta)
}
