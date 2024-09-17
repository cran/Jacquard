## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(warnings = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(fig.width = 6, fig.height = 6) 

## ----preinstall---------------------------------------------------------------
#install.packages("Jacquard")
library(Jacquard)

## ----data---------------------------------------------------------------------
data(SimulatedPedigree)
SimulatedPedigree[1:5,1:10]

## -----------------------------------------------------------------------------
Xped <- SimulatedPedigree[,1:5]
Xgen <- as.matrix(SimulatedPedigree[,6:ncol(SimulatedPedigree)])

## -----------------------------------------------------------------------------
#GTC <- JointGenotypeCounts(Xgen)
data(GTC)
names(GTC)

## -----------------------------------------------------------------------------
GTC[[1]][1:5,1:5]

## -----------------------------------------------------------------------------
mafvec <- mafvector(Xgen)
mafvec[1:5]

## ----echo = TRUE--------------------------------------------------------------
ii <- 1:3
Xped[ii,]

GTCsubset <- list(length = 9)
for (k in 1:9) {
  GTCsubset[[k]] <- matrix(numeric(3^2), ncol = 3)
  GTCsubset[[k]] <- GTC[[k]][ii,ii]
}

## -----------------------------------------------------------------------------
set.seed(123)
delta.init <- runif(9)
delta.init <- delta.init/sum(delta.init)

## ----echo = TRUE--------------------------------------------------------------
output <- Jacquard.cls(GTCsubset,mafvec=mafvec,
                       eps=1e-06,
                       delta.init=delta.init)
Delta.cls <- output$delta

## -----------------------------------------------------------------------------
output$convergence

## -----------------------------------------------------------------------------
Delta.cls[[9]][1,2]

## -----------------------------------------------------------------------------
DeltaPair(Delta.cls,1,2)

## ----echo = TRUE--------------------------------------------------------------
PairwiseList(Delta.cls)

## -----------------------------------------------------------------------------
#DeltaSimulatedPedigree <- Jacquard.cls(GTC,mafvec=mafvec,
#                       eps=1e-06,
#                       delta.init=delta.init)$delta
data(DeltaSimulatedPedigree)

## -----------------------------------------------------------------------------
BoxplotDelta(DeltaSimulatedPedigree)

## -----------------------------------------------------------------------------
Theta <- CalculateTheta(DeltaSimulatedPedigree)

## -----------------------------------------------------------------------------
Theta[[1]][1:5,1:5]

## -----------------------------------------------------------------------------
diag(Theta[[2]])[1:5]
diag(DeltaSimulatedPedigree[[1]])[1:5]

## -----------------------------------------------------------------------------
BoxplotTheta(Theta)

## ----mom, echo = FALSE--------------------------------------------------------
KS.mom <- CalculateMom(Xgen[1:10,],mafvec,verbose=FALSE)

## -----------------------------------------------------------------------------
KS.mom[[1]][1:5,1:5]

