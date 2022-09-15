# functions for age-structured yield per recruit & stock recruit analyses

# # herring
# nages <- 8
# maturity <- c(0,	0.0115,	0.6778,	0.9597,	0.9997,	1,	1,	1)
# selex <- c(0.06193189, 0.79728883, 0.56565442, 0.54179750, 0.63757692, 0.79754726, 1.00000000, 0.99324767)
# weight <- c(0.0103,	0.0466,	0.0825,	0.1101,	0.1407,	0.1616,	0.1663,	0.1948)
# M <- 0.35


## Numbers-per-recruit
NPR <- function(F,M,selex) {
  nages <- length(weight)
  N <- rep(0,nages)
  N[1] <- 1
  for (age in 2:nages)
    N[age] <- N[age-1]*exp(-1*(selex[age-1]*F+M))
  N[nages] <- N[nages]/(1-exp(-1*(selex[age]*F+M)))
  return(N)
}
#example usage
#npr <- NPR(F=0,M,selex)


## Yield-per-recruit
YPR <- function(F,M,weight,selex) {
  N <- NPR(F,M,selex)
  Z <- selex*F+M
  yield <- sum(weight*selex*F*N*(1-exp(-Z))/Z)
  return(yield)
}
#YPR(F=0.1,npr,M,weight,selex)
#YPR(F=0.1,M,weight,selex)

## Spawning Biomass per recruit
SBPR <- function(F,M,weight,selex,maturity) {
  N <- NPR(F,M,selex)
  sbpr <- sum(N*weight*maturity)
  return(sbpr)
}
# SBPR(0,M,weight,selex,maturity)
# SBPR(0.4,M,weight,selex,maturity)