### MEBM Fall 2022
### Homework Assignment #1

library(tidyverse)
library(TMB)

# Atlantic herring life history & fishery parameters
# drawn from most recent assessment (2022)
nages <- 8
maturity <- c(0,	0.0115,	0.6778,	0.9597,	0.9997,	1,	1,	1)
selex <- c(0.06193189, 0.79728883, 0.56565442, 0.54179750, 0.63757692, 0.79754726, 1.00000000, 0.99324767)
weight <- c(0.0103,	0.0466,	0.0825,	0.1101,	0.1407,	0.1616,	0.1663,	0.1948)
M <- 0.35

# Equilibirum per-recruit analyses
source("course-materials/assignments/hw-01/spr_functions.R")
#example function calls
#SBPR(0,M,weight,selex,maturity)
#SBPR(0.4,M,weight,selex,maturity)


# stock assessment output (time series of SSB and Recruitment)
herring <- "herring-SSBRecData.rds"
herring



## Compile and load the TMBmodel
compile("   .cpp")
dyn.load(dynlib("   "))

#set up TMB data object
data <- list(   )
#set up TMB herring parameter object and starting values
parameters <- list(   )


## Make a function object
obj <- MakeADFun(data, parameters, DLL= , 
                 random =   ,
                 control = list(trace=1, iter.max=1000)) 

## Call function minimizer
opt <-nlminb(obj$par, obj$fn, obj$gr)
opt

## Get parameter uncertainties and convergence diagnostics
sdr <- sdreport(obj)
sdr
summary(sdr)
summary(sdr, "fixed")
summary(sdr, "random")
summary(sdr, "report")
