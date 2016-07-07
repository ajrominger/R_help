## package needed for mixed effects
library(lme4)

## function that makes the model matrix for a specific set of conditions (i.e.
## for a specific set of coefficient values and numbers of ferns and plots)
## function arguments are:
## a.t = time effect (length 1, equal to difference between the two times)
## a.trt = treatment effect (length 7, each value is the difference between that treatment and the control)
## a.As = main effect of As in soil on As in ferns
## a.As.trt = interaction of treatments with As relationship (length 7)
## nfern = number of ferns
## nplot = number of plots

make.mm <- function(a.t, a.trt, a.As, a.As.trt, beta.sd, epsilon.sd, nfern, nplot) {
    
}

## function to generate data using matrix multiplication,
## run linear model on that simulated data and extract p-values
## function arguments are:
## mm = the model matrix as returned by `make.mm'
## beta.sd = standard deviation of the random plot effect
## epsilon.sd = standard deviation of the residuals

sim.lm <- function(mm, beta.sd, epsilon.sd) {
    
}

## actually the only thing that needs to get repeated is the beta and epsilon part, so have make.mm return the result of the matrix multiplication