## first mse functions  
## Gavin Fay
## January 2022
## for sourcing in subsequenet exercises

# logistic production function
schaefer <- function(B,C,K,r) {
  #function schaefer takes the current biomass, a catch, 
  #and the model parameters to compute next year's biomass
  res <- B + B * r * (1 - B/K) - C
  return(max(0.001,res))  # we add a constraint to prevent negative biomass
}

#pella-tomlinson
pellat <- function(B,C,K,r, m) {
  #function PT takes the current biomass, a catch, 
  #and the model parameters to compute next year's biomass
  res <- B + B * r * (1 - ((B/K)^(m-1))) - C
  return(max(0.001,res))  # we add a constraint to prevent negative biomass
}

#biomas projection
dynamics <- function(pars,C,yrs,m) {
  # dynamics takes the model parameters, the time series of catch, 
  # & the yrs to do the projection over
  
  # first extract the parameters from the pars vector (we estimate K in log-space)
  K <- exp(pars[1])
  r <- exp(pars[2])
  
  # find the total number of years
  nyr <- length(C) + 1
  
  # if the vector of years was not supplied we create 
  # a default to stop the program crashing
  if (missing(yrs)) yrs <- 1:nyr
  
  #set up the biomass vector
  B <- numeric(nyr)
  
  #intialize biomass at carrying capacity
  B[1] <- K
  # project the model forward using the schaefer model
  for (y in 2:nyr) {
    B[y] <- pellat(B[y-1],C[y-1],K,r, m)
  }
  
  #return the time series of biomass
  return(B[yrs])
  
  #end function dynamics
}  


# function to calculate the negative log-likelihood
nll <- function(pars,C,U, m) {  #this function takes the parameters, the catches, and the index data
  sigma <- exp(pars[3])  # additional parameter, the standard deviation of the observation error
  B <- dynamics(pars,C, m=m)  #run the biomass dynamics for this set of parameters
  Uhat <- B   #calculate the predicted biomass index - here we assume an unbiased absolute biomass estimate
  output <- -sum(dnorm(log(U),log(Uhat),sigma,log=TRUE),na.rm=TRUE)   #calculate the negative log-likelihood
  return(output)
  #end function nll
}


assess <- function(catch,index,calc.vcov=FALSE,pars.init, m) {
  # assess takes catch and index data, initial values for the parameters,
  # and a flag saying whether to compute uncertainty estimates for the model parameters
  
  #fit model
  # optim runs the function nll() repeatedly with differnt values for the parameters,
  # to find the values that give the best fit to the index data
  res <- optim(pars.init,nll,C=catch,U=index,m=m,hessian=TRUE)
  
  # store the output from the model fit
  output <- list()
  output$pars <- res$par
  output$biomass <- dynamics(res$par,catch, m=m)
  output$convergence <- res$convergence
  output$nll <- res$value
  if (calc.vcov)
    output$vcov <- solve(res$hessian)
  
  return(output)
  #end function assess
}


##### Data generation
observe <- function(biomass, sigma) {
  biomass * exp(rnorm(1, -0.5*sigma^2, sigma))
}



##### Harvest Control Rule

control.pars <- list()
control.pars$Htarg <- 0.1
control <- function(estimated.biomass, control.pars) {
  control.pars$Htarg
}

#Implementation 
implement <- function(TAC,...) {
  TAC
}


# MSE function
evaluate <- function(pars.iter, biomass.iter,
                     control.pars, data.years, proj.years,
                     iterations, m, ...) {
  # function arguments:
  # pars.iter & biomass.iter, the parameters & historical biomass trajectories of the operating model
  # control.pars, the specifications of the harvest control rule
  
  # set up some indexing values
  iyr <- length(data.years)+1
  pyr <- length(proj.years)
  yrs <- c(data.years, proj.years, max(proj.years)+1)
  
  # set up a data frame to store the results
  res <- data.frame()
  
  # loop over the iterations of the MSE, each iteration conducts a 20 year projection with annual generation of biomass    
  # observations and appliations of the control rule.
  for(i in 1:iterations) {
    
    #extract the parameters for this iteration
    K.i <- exp(pars.iter[i,1])
    r.i <- exp(pars.iter[i,2])
    sig.i <- exp(pars.iter[i,3])
    
    #set up vectors for time series of interest.
    biomass.i <- c(subset(biomass.iter, iter==i)$biomass, numeric(pyr))
    index.i <- c(index,numeric(pyr))
    catch.i <- c(harvest, numeric(pyr))
    TAC.i <- numeric(pyr)
    
    # loop over the projection period.
    for (y in iyr:(iyr+pyr-1)) {
      #generate the data for the most recent year
      index.i[y] <- observe(biomass.i[y] , sig.i)
      #calculate the TAC based on the harvest control rule
      # note that the control rule ONLY sees the index data, not the operating model biomass.
      TAC.i [y]  <- control(index.i[y], control.pars) * index.i[y]
      #find the realized catch after implementation error
      catch.i[y] <- implement(TAC.i[y], overshoot=0)
      
      # update the true biomass of the operating model based on the output of the HCR
      biomass.i[y+1] <- pellat(biomass.i[y],catch.i[y],K.i,r.i, m)
      
      #end projection year loop for iteration i  
    }
    
    #store the results for this iteration
    res <- rbind(res, data.frame(year = yrs[-length(yrs)],
                                 value = index.i, type = "index", iter = i),
                 data.frame(year = yrs[-length(yrs)],
                            value = catch.i, type = "catch", iter=i),
                 data.frame(year = yrs, value = biomass.i,
                            type= "biomass", iter=i)) 
    #end loop over iterations
  }
  return(res)
  #end function evaluate()
}

#projection plot
projection.plot <- function(project.results) {
project.results %>% 
    filter(type %in% c("biomass","catch")) %>% 
    group_by(type, year) %>% 
    median_qi(value, .width = c(.5, .8, .95)) %>%
    ggplot() +  
    geom_lineribbon(aes(x = year, y = value, ymin = .lower, ymax = .upper),
                    show.legend = FALSE) +
    scale_fill_brewer() +
    geom_line(aes(y=value,x=year),data = subset(project.results, type != "index" & iter==1 & year %in% proj.years), lty=1,lwd=1,col=gray(0.7)) +
    geom_line(aes(y=value,x=year),data = subset(project.results, type != "index" & iter==2 & year %in% proj.years), lty=1,lwd=1,col=gray(0.7)) +
    geom_line(aes(y=value,x=year),data = subset(project.results, type != "index" & iter==3 & year %in% proj.years), lty=1,lwd=1,col=gray(0.7)) +
    facet_wrap(~type, scale = "free_y") + 
    ylab("Million tonnes") + 
    theme_bw()
}
#### Management using alternative harvest control rules

#HCR
control <- function(estimated.biomass, control.pars) {
  H1 <- control.pars$H1
  H2 <- control.pars$H2
  Bmax <- control.pars$Bmax
  B2 <- control.pars$B2
  B1 <- control.pars$B1
  
  harv <- ifelse(estimated.biomass >= B1, H1,
                 ifelse(estimated.biomass < B2, H2,
                        (H1-H2)/(B1-B2)*(estimated.biomass - B2) + H2))
  
  return(harv)
  
  #end function control
}

#implementation
#TAC with overshoot
implement <- function(TAC, overshoot=0, ...) {
  TAC * (1 + overshoot)
}




