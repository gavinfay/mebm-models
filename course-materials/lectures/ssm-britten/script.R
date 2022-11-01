library(rstan)
options(mc.cores = parallel::detectCores())

##--SIMULATE DATA--######################################
#Setup
set.seed(123)
x0   <- 0.1
r    <- 0.5
K    <- 1
T    <- 30
x    <- matrix(NA,nrow=2,ncol=T)
x[,1] <- c(x0,x0)

#Timestep
for(i in 2:T){
  x[1,i] <- x[1,i-1] + r*x[1,i-1]*(1 - x[1,i-1]/K) + rnorm(1,sd=0.05)
  x[2,i] <- x[2,i-1] + r*x[2,i-1]*(1 - x[2,i-1]/K)
}

#plot
par(mfrow=c(1,1),mar=c(2,3,2,2),oma=c(3,2,2,2))
plot(1:T,x[1,],type='l')
lines(1:T,x[2,],type='l')
  mtext('Biomass',side=2,line=2.5)
  mtext('Time',side=1,line=2.5)

y <- sapply(x[1,] + rnorm(ncol(x),sd=0.1), function(x) max(x,0))
plot(1:T,x[1,],xlab='Time',type='l')
points(1:T,y,col='red')
  mtext('Biomass',side=2,line=2.5)
  mtext('Time',side=1,line=2.5)

#negative log likelihood function  
ll <- function(theta){
  x0 <- theta[1]
  r  <- theta[2]
  K  <- theta[3]
  
  T    <- 30
  x    <- numeric(T)
  x[1] <- x0
  
  for(i in 2:T){
    x[i] <- x[i-1] + r*x[i-1]*(1 - x[i-1]/K)
  }
  
  return(sum(-log(dnorm(x - y))))
}

#Optimize parameters
fit <- optim(par=c(0.1,0.1,1),fn=ll)

#Extract optimized parameters
pars <- fit$par

x0   <- pars[1]
r    <- pars[2]
K    <- pars[3]
x    <- numeric(T)
x[1] <- x0

#Timestep
for(i in 2:T){
  x[i] <- x[i-1] + r*x[i-1]*(1 - x[i-1]/K)
}

#Plot 'observations' and fitted model
plot(1:T,y,xlab='Time')
  mtext('Biomass',side=2,line=2.5)
  mtext('Time',side=1,line=2.5)
  lines(1:T, x,col='red')

#################################################################
## COMPARE DETERMINISTIC VS STATE SPACE MODEL ###################
#################################################################

#Deterministic model
mod   <- stan_model('~/dropbox/Teaching/UMASS/logistic.stan') #Compile stan code
dat   <- list(y=y,T=T) #Package data
mcmc  <- sampling(mod,data=dat) #Do sampling
post  <- extract(mcmc) #Extract posterior

#State space model
modssm <- stan_model('~/dropbox/Teaching/UMASS/logistic_ssm.stan')
datssm <- list(y=y,T=T,sigma_x=0.05)
mcmcssm <- sampling(modssm,data=datssm)
postssm <- extract(mcmcssm)

#6 panel plot
par(mfrow=c(3,2))
plot(colMeans(post$ypred),ylim=c(0,1.2))
lines(colMeans(post$ypred) - apply(post$ypred,2,sd))
lines(colMeans(post$ypred) + apply(post$ypred,2,sd))
mtext(side=2,'Biomass',line=2.5)
mtext(adj=0,'Deterministic')

plot(colMeans(postssm$ypred),ylim=c(0,1.2))
lines(colMeans(postssm$ypred) - apply(postssm$ypred,2,sd))
lines(colMeans(postssm$ypred) + apply(postssm$ypred,2,sd))
mtext(adj=0,'State space model')

hist(post$r,breaks=seq(0,4,0.1),main='',xlim=c(0,2))
mtext(side=1,line=2.5,'r')
hist(postssm$r,col='red',breaks=seq(0,4,0.1),main='',xlim=c(0,2))
mtext(side=1,line=2.5,'r')

hist(post$K,breaks=seq(0,3,0.05),main='',xlim=c(0.5,1.5))
mtext(side=1,line=2.5,'K')
hist(postssm$K,col='red',breaks=seq(0,3,0.05),xlim=c(0.5,1.5),main='')
mtext(side=1,line=2.5,'K')

#####################################################################
## RATIO OF OBSERVATION AND PROCESS ERROR ###########################
#####################################################################
modsig   <- stan_model('~/dropbox/Teaching/UMASS/logistic_ssm_sigma.stan')
#Different assumptions about ratio of observation to process error
datsig1   <- list(y=y,T=T,sigma_x=0.5,sigma_y=0.1)
datsig2   <- list(y=y,T=T,sigma_x=0.5,sigma_y=0.5)
datsig3   <- list(y=y,T=T,sigma_x=0.5,sigma_y=1.0)

mcmcsig1  <- sampling(modsig,data=datsig1)
postsig1  <- extract(mcmcsig1)
mcmcsig2  <- sampling(modsig,data=datsig2)
postsig2  <- extract(mcmcsig2)
mcmcsig3  <- sampling(modsig,data=datsig3)
postsig3  <- extract(mcmcsig3)

par(mfrow=c(3,1))
plot(colMeans(postsig1$ypred),ylim=c(0,1.2),type='l')
points(1:T,y,col='red')
mtext(adj=0,'sigma_x = 0.5, sigma_y=0.1')
plot(colMeans(postsig2$ypred),ylim=c(0,1.2),type='l')
points(1:T,y,col='red')
mtext(adj=0,'sigma_x = 0.5, sigma_y=0.5')
plot(colMeans(postsig3$ypred),ylim=c(0,1.2),type='l')
points(1:T,y,col='red')
mtext(outer=TRUE,'Biomass',side=2)
mtext(adj=0,'sigma_x = 0.5, sigma_y=1.0')


#################################################################
## TIME VARYING R ###############################################
#################################################################
set.seed(123)
x0   <- 0.01
r    <- as.numeric(arima.sim(
  model=list(ar=0.9),n=T,sd=0.1,mean=0.05))
K    <- 1
T    <- 30
x    <- numeric(T)
x[1] <- x0

#Timestep
for(i in 2:T){
  x[i] <- x[i-1] + r[i]*x[i-1]*(1 - x[i-1]/K) + rnorm(1,sd=0.05)
}
y <- sapply(x + rnorm(T,sd=0.05), function(x) max(x,0))

par(mfrow=c(3,1),mar=c(2,3,2,2),oma=c(2,2,2,2))
plot(1:T,r)
plot(1:T,x)
plot(1:T,y)

#Fit time-varying r state space model
modr   <- stan_model('~/dropbox/Teaching/UMASS/logistic_ssm_sigma_theta.stan')
datr   <- list(y=y,T=T,sigma_x=0.05,sigma_y=0.05,sigma_r=0.1,alpha=0.9)
mcmcr  <- sampling(modr,data=datr)
postr  <- extract(mcmcr)

par(mfrow=c(2,1),mar=c(2,3,2,2),oma=c(2,2,2,2))
plot(colMeans(postr$r),ylim=c(0.1,1.2),type='l')
points(1:T,r,col='red')
mtext(side=2,'r',line=2.5)

plot(colMeans(postr$x),ylim=c(0,1.2),type='l')
points(1:T,x,col='red')
mtext(side=2,'Biomass',line=2.5)



