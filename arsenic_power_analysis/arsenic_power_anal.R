## package needed for mixed effects
library(lme4)

## function that makes the model matrix for a specific set of conditions (i.e.
## for a specific set of coefficient values and numbers of ferns and plots)
## function arguments are:
## nfern = number of ferns per plot
## nplot = number of plots

make.mm <- function(nfern, nplot) {
    n <- 2*nfern*nplot
    
    ## matrix of dummy variables indicating treatment group
    treat <- matrix(0, nrow=nfern*8, ncol=7)
    for(i in 1:7) treat[(i)*nfern + 1:nfern, i] <- 1
    treat <- do.call(rbind, replicate(2*nplot/8, treat, simplify=FALSE))
    
    ## recall column 1 is the intercept (i.e. all 1's); also x will be randomly 
    ## generated in `sim.lm', so here we just make a place-holder for it (=1)
    return(cbind(rep(1, n),        # the intercept
                 rep(0:1, each=n/2), # time 1 or time 2
                 treat,            # dummy matrix of indicators
                 rep(1, n),        # place-holder for x
                 treat))           # place-holder for x
}

## function to generate data using matrix multiplication,
## run linear model on that simulated data and extract p-values
## function arguments are:
## mm = the model matrix as returned by `make.mm'
## a.t = time effect (length 1, equal to difference between the two times)
## a.trt = treatment effect (length 5, each value is the difference between that treatment and the control)
## a.As = main effect of As in soil on As in ferns
## a.As.trt = interaction of treatments with As relationship (length 5)
## beta.sd = standard deviation of the random plot effect
## epsilon.sd = standard deviation of the residuals
## nfern = number of ferns per plot
## nplot = number of plots


## a.trt and a.As.trt should only be 5 long

sim.lm <- function(mm, a.t, a.trt, a.As, a.As.trt, beta.sd, epsilon.sd, nfern, nplot) {
    n <- 2*nfern*nplot
    
    ## the model matrix, remember that column 1 is the intercept (i.e. all 1's)
    mm <- make.mm(nfern, nplot)
    
    ## fill in x place-holders in model matrix with simulated x values
    x <- rnorm(n)
    mm[, 10:17] <- mm[, 10:17] * x
    
    ## plot random effect (note each plot is sampled twice--two time points)
    plt <- rep(rep(rnorm(nplot, sd=beta.sd), each=nfern), 2)
    
    ## residuals
    epsilon <- rnorm(n, sd=epsilon.sd)
    
    ## response variable
    y <- mm %*% c(1, a.t, a.trt, a.As, a.As.trt) + plt + epsilon
    
    ## time step, treat and plot number for use in lmer
    time.step <- as.factor(mm[, 2] + 1)
    treat <- as.factor(mm[, 3:9] %*% 1:7 + 1)
    pltID <- as.factor(rep(rep(1:nplot, each=nfern), 2))
    
    ## fit models
    mod.full <- lmer(y ~ time.step + treat*x + (1|pltID), REML=FALSE)
    mod.null <- lmer(y ~ time.step + x + (1|pltID), REML=FALSE)
    
    ## return p-val from likelihood ratio test (null is that `treat' doesn't matter)
    return(anova(mod.null, mod.full)[['Pr(>Chisq)']][2]) # return z-test for diff in coeff
}


## function to run the simulation for 1 set of parameters
run.1sim <- function(nsim, a.t, a.trt, a.As, a.As.trt, beta.sd, epsilon.sd, nfern, nplot) {
    ## make the model matrix.  note, we only need to do this once (i.e. it doesn't change
    ## across simulations) that's why we made a sepparate function for it so that we don't 
    ## waste time in the simulation re-making the same model matrix
    mm <- make.mm(nfern, nplot)
    
    ## use replicate to return many simulations for these parameters
    out <- replicate(nsim, sim.lm(mm, a.t, a.trt, a.As, a.As.trt, beta.sd, epsilon.sd, nfern, nplot))
    
    ## calculate and return power
    return(mean(out <= 0.05))
}

## example use (goal is to put this in a function that will iterate through all parameter combinations)
## here's the case where there is *no* treatment effect (95% of pvals should be > 0.05 if nsim is large)
run.1sim(nsim=10, a.t=1, a.trt=rep(0, 7), a.As=0.5, a.As.trt=rep(0, 7), beta.sd=0.5, epsilon.sd=0.1, nfern=3, nplot=16*3)

## here's the case where there *is* a treatment effect (most pvals should be <= 0.05 if nsim is large)
run.1sim(nsim=10, a.t=1, a.trt=1:7, a.As=0.5, a.As.trt=1:7, beta.sd=0.5, epsilon.sd=0.1, nfern=3, nplot=16*3)


## NEXT STEPS:

## make a matrix containing all the parameter values you want to explore
## we decided on 3 different values for a.t, 7 for a.trt, 7 for a.As, 7 for a.As.trt, 
## 3 for beta.sd, 3 for epsilon.sd.  you have 3 different combinations for nfern and nplot
## but you don't want to look at each combination (you have a fixed number of ferns so the
## number of plots will be fully determined by the number of ferns). 
## you can use expand.grid to make you matrix

## start with this example:
param <- expand.grid(a.t=1:3, a.trt=1:7, nfern=c(2, 3, 6))
param

## you can see we have 3 unique values for a.t, 7 for a.trt and 3 for nfern. expand.grid has 
## generated all unique combinations.  you'll need to add the rest of the parameters (a.As, a.As.trt, etc.)

## now we need to calculate number of plots based on nfern (because one determines the other)
## i can't remember what we figured out for that, but suppose that the total number of ferns is 
## limited to 300, then we could do this

param$nplot <- 300 / param$nfern

## the real calculation will be more complicated (edge effects and all that, but i'll leave it to you)

## now we need a function to loop over param and run the simulation for each combination.
## here's a skeleton of that function with some notes below on helpful things

run.pwrAnal <- function(nsim) {
    ## everything in this function needs to be done for each row of param, so right away we need to enter
    ## some kind of for loop kind of construct.  for loops are slow in R so we'll use one of the apply 
    ## functions (see below for details)
    sapply(1:nrow(param), function(i) {
        ## right now there is just a simple function (sum) that we're applying to each row of param
        this.row <- param[i, ]
        sum(this.row)
        
        ## the real function (which you type below these comments) will need to do the following:
        ## 1: for a.trt and a.As.trt, these need to be vectors of length 5, so make a sequence using
        ##    the a.trt and a.As.trt elements of this.row (you can access them with the $, like this:
        ##    this.row$a.trt)
        ## 2: using the 5 long vectors you made in step 1 and the other elements of this.row, plug
        ##    those into run.1sim, that's it, you're done!
    })
}

## if everything worked right the output of run.pwrAnal should be a vector of equal length to param
## then you can plot that vector against the parameter values in param

## here's a little more info on sapply, basically just using what we started out with in the function
## so you can get an idea of how it works

sapply(1:nrow(param), function(i) {
    print(i)
})

## sapply iterates through the vector 1:nrow(param) and does something (specified by the function) 
## to each element in the vector. in the example above it just prints each element
## in the example below we'll extract each row of param and sum their contents

sapply(1:nrow(param), function(i) {
    this.row <- param[i, ]
    sum(this.row)
})

## compare the first few values from the above command with "doing it by hand":
sum(param[1, ])
sum(param[2, ])
sum(param[3, ])
