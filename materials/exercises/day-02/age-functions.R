#### 
###  Contains functions for age-structured modeling
###  Gavin Fay
###  August 2021

# function does the MSE loop for 1 realization
evaluate <- function(seed,input) {

  # ## read in biol data
  nages <- input$nages
  maturity <- input$maturity
  selex <- input$selex
  weight <- input$weight
  # 
  M <- input$M
  Fmult <- input$Fmult
  SPRtarg <- input$SPRtarg
  Rbar <- input$Rbar
  sigmaR <- input$sigmaR
  cpueCV <- input$cpueCV
  
# # ## read in biol data
# path <- "materials/exercises/day-02/"
# nages <- scan(paste0(path,"floundah_biology.txt"),n=1,skip=9)
# maturity <- scan(paste0(path,"floundah_biology.txt"),n=nages,skip=11)
# selex <- scan(paste0(path,"floundah_biology.txt"),n=nages,skip=13)
# weight <- scan(paste0(path,"floundah_biology.txt"),n=nages,skip=15)
# # 
# M <- 0.4
# Fmult <- 1 #proportion of FSPRtarg during historical period
# SPRtarg <- 0.4
# Rbar <- 1000
# sigmaR <- 0.6
# cpueCV <- 0.3

#set the seed
set.seed(seed)

#counters
histyr <- 50
projyr <- 50
nyr <- histyr + projyr

#set up storage objects
om_N <- matrix(rep(0,nages*nyr), nrow=nyr, ncol = nages)
om_F <- rep(NA,nyr-1)
cpue <- rep(NA,nyr)
tac <- rep(NA,nyr)
om_SSB <- rep(NA,nyr)

#get the OM F reference point
getFSPR <- function (x, SPRtarg, M, weight, selex, maturity) (SPRtarg-SBPR(exp(x),M,weight,selex,maturity)/SBPR(0,M,weight,selex,maturity))^2
Ftarg <- nlminb(exp(0.2),getFSPR, SPRtarg = SPRtarg, M=M, weight=weight, selex=selex, maturity=maturity)
Ftarg <- exp(Ftarg$par)

#initialize OM & project through historical period (pre-HCR)
om_N[1,] <- Rbar*NPR(Fmult*Ftarg,M,selex)*rlnorm(nages,0,sigmaR)
for (iyr in 2:histyr+1) {
  res <- OMupdate(om_N[iyr-1,],-99,M,Rbar,weight,selex,sigmaR,Fmult*Ftarg)
  om_N[iyr,] <- res$N
  om_F[iyr-1] <- res$F
}

#generate historical CPUE index
cpue[1:histyr] <- sapply(1:histyr,function(x) sum(om_N[x,]*weight*selex))*rlnorm(histyr,0,cpueCV)

#loop over projection years
for (iyr in (histyr+1):(nyr-1)) {
  
  # do assessment / control rule  (dummy: 10% of last year's CPUE)
  tac[iyr] <- cpue[iyr-1]*0.1
  
  #update OM
  res <- OMupdate(om_N[iyr,],tac[iyr],M,Rbar,weight,selex,sigmaR)
  om_N[iyr+1,] <- res$N
  om_F[iyr] <- res$F
  om_SSB[iyr+1] <- sum(0.5*om_N[iyr+1,]*weight*maturity)
  #generate new CPUE data
  cpue[iyr] <- sum(om_N[iyr,]*weight*selex)*rlnorm(1,0,cpueCV)
}

result <- list(om_N = om_N,
               om_F = om_F,
               om_SSB = om_SSB,
               cpue = cpue,
               tac = tac,
               ftarg = Ftarg)
 return(result)
}
# # # ## read in biol data
# input <- NULL
# path <- "materials/exercises/day-02/"
# input$nages <- scan(paste0(path,"floundah_biology.txt"),n=1,skip=9)
# input$maturity <- scan(paste0(path,"floundah_biology.txt"),n=nages,skip=11)
# input$selex <- scan(paste0(path,"floundah_biology.txt"),n=nages,skip=13)
# input$weight <- scan(paste0(path,"floundah_biology.txt"),n=nages,skip=15)
# #
# input$M <- 0.4
# input$Fmult <- 1 #proportion of FSPRtarg during historical period
# input$SPRtarg <- 0.4
# input$Rbar <- 1000
# input$sigmaR <- 0.6
# input$cpueCV <- 0.3
# xx <- evaluate(input, seed=24601)

#Annual update
OMupdate <- function(N,C,M,Rbar,weight,selex,sigmaR,Fstart=0) {
  F = Fstart
  if (C!=-99) {
    u <- C/(sum(N*exp(-0.5*M)*weight*selex))
    F <- -1*log(1-u)
  }
  R <- Rbar*rlnorm(1,0,sigmaR)
  N <- Natage(N, F, M, selex, R)
  result <- list(N=N,F=F)
  return(result)
}

## Numbers at age calculation
Natage <- function(N1,F,M,selex,R) {
  nages <- length(selex)
  N <- rep(0,nages)
  N[1] <- R
  for (age in 2:nages)
    N[age] <- N1[age-1]*exp(-1*(selex[age-1]*F+M))
  N[nages] <- N[nages] + N1[nages]*exp(-1*(selex[nages]*F+M))
  return(N)  
}

## Numbers-per-recruit
NPR <- function(F,M,selex) {
  nages <- length(selex)
  N <- rep(0,nages)
  N[1] <- 1
  for (age in 2:nages)
    N[age] <- N[age-1]*exp(-1*(selex[age-1]*F+M))
  N[nages] <- N[nages]/(1-exp(-1*(selex[age]*F+M)))
  return(N)
}
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
#SBPR(0,M,weight,selex,maturity)
#SBPR(0.4,M,weight,selex,maturity)



# F <- seq(0,1,0.02)
# SPR <- sapply(F,SBPR,M=M,weight=weight,selex=selex,maturity=maturity)
# SPR <- SPR/SBPR(0,M,weight,selex,maturity)

# plot(F,SPR,type='l',xlab="F",ylab="SPR",ylim=c(0,1))
# abline(h=0.4,lty=2)
# flag <- ifelse(SPR>0.4,1,0)
# F[which(flag==1)[length(which(flag==1))]]
# 
# F <- seq(0,1,0.02)
# Yield <- sapply(F,YPR,M=M,weight=weight,selex=selex)
# F[which.max(Yield)]
# max(Yield)
# 
# plot(F,Yield,type='l',xlab="F",ylab="YPR",ylim=c(0,1.2*max(Yield)))
# abline(h=Yield[which.max(Yield)],lty=2)
