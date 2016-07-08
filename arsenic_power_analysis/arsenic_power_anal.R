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
    nfern <- 3
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
## a.trt = treatment effect (length 7, each value is the difference between that treatment and the control)
## a.As = main effect of As in soil on As in ferns
## a.As.trt = interaction of treatments with As relationship (length 7)
## beta.sd = standard deviation of the random plot effect
## epsilon.sd = standard deviation of the residuals
## nfern = number of ferns per plot
## nplot = number of plots

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
    return(anova(mod.null, mod.full)[['Pr(>Chisq)']][2])
}


## function to run the simulation for 1 set of parameters
run.1sim <- function(nsim, a.t, a.trt, a.As, a.As.trt, beta.sd, epsilon.sd, nfern, nplot) {
    ## make the model matrix.  note, we only need to do this once (i.e. it doesn't change
    ## across simulations) that's why we made a sepparate function for it so that we don't 
    ## waste time in the simulation re-making the same model matrix
    mm <- make.mm(nfern, nplot)
    
    ## use replicate to return many simulations for these parameters
    replicate(nsim, sim.lm(mm, a.t, a.trt, a.As, a.As.trt, beta.sd, epsilon.sd, nfern, nplot))
}

## example use (goal is to put this in a function that will iterate through all parameter combinations)
## here's the case where there is *no* treatment effect (95% of pvals should be > 0.05 if nsim is large)
run.1sim(nsim=10, a.t=1, a.trt=rep(0, 7), a.As=0.5, a.As.trt=rep(0, 7), beta.sd=0.5, epsilon.sd=0.1, nfern=3, nplot=16*3)

## here's the case where there *is* a treatment effect (most pvals should be <= 0.05 if nsim is large)
run.1sim(nsim=10, a.t=1, a.trt=1:7, a.As=0.5, a.As.trt=1:7, beta.sd=0.5, epsilon.sd=0.1, nfern=3, nplot=16*3)
