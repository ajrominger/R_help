## package needed for mixed effects
library(lme4)

## package for pretty plotting
library(RColorBrewer)

## function that makes the model matrix for a specific set of conditions (i.e.
## for a specific set of coefficient values and numbers of ferns and plots)
## function arguments are:
## nfern = number of ferns per plot
## nplot = number of plots (total; number of plot replicates is built into calculating nplot from nfern for 6 trt)nfern


make.mm <- function(nfern, nplot) {
    n <- 2*nfern*nplot
    
    ## matrix of dummy variables indicating treatment group
    treat <- matrix(0, nrow=nfern*6, ncol=5) 
    ##nrow=nfern*total # trt incl. control; ncol=# trt not including control
    for(i in 1:5) treat[(i)*nfern + 1:nfern, i] <- 1
    treat <- do.call(rbind, replicate(2*nplot/6, treat, simplify=FALSE))
    
    
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
    mm[, 8:13] <- mm[, 8:13] * x
    
    ## plot random effect (note each plot is sampled twice--two time points)
    plt <- rep(rep(rnorm(nplot, sd=beta.sd), each=nfern), 2)
    
    ## residuals
    epsilon <- rnorm(n, sd=epsilon.sd)
    
    ## response variable
    y <- mm %*% c(1, a.t, a.trt, a.As, a.As.trt) + plt + epsilon
    
    ## time step, treat and plot number for use in lmer
    time.step <- as.factor(mm[, 2] + 1)
    treat <- as.factor(mm[, 3:7] %*% 1:5 + 1)
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
run.1sim(nsim=10, a.t=1, a.trt=rep(0, 5), a.As=0.5, a.As.trt=rep(0, 5), beta.sd=0.5, epsilon.sd=0.1, nfern=3, nplot=16*3)

## here's the case where there *is* a treatment effect (most pvals should be <= 0.05 if nsim is large)
run.1sim(nsim=10, a.t=1, a.trt=1:5, a.As=0.5, a.As.trt=1:5, beta.sd=0.5, epsilon.sd=0.1, nfern=3, nplot=16*3)

## works fine through these examples with nfern changed to nfern (as it should, just a find/replace)

## NEXT STEPS:

##Part 1:
## make a matrix containing all the parameter values you want to explore
## we decided on 3 different values for a.t, 7 for a.trt, 7 for a.As, 7 for a.As.trt, 
## 3 for beta.sd, 3 for epsilon.sd.  you have 3 different combinations for nfern and nplot
## but you don't want to look at each combination (you have a fixed number of ferns so the
## number of plots will be fully determined by the number of ferns). 
## you can use expand.grid to make your matrix

## start with this example:
#param <- expand.grid(a.t=1:3, a.trt=1:7, nfern=c(2, 3, 6))
#param

## you can see we have 3 unique values for a.t, 7 for a.trt and 3 for nfern. expand.grid has 
## generated all unique combinations.  you'll need to add the rest of the parameters (a.As, a.As.trt, etc.)

##adding rest of parameters, these are realistic values:
param <- expand.grid(nfern=c(39, 24, 9), nplot = 0, a.t=1:3, a.trt=seq(0, 30, length=7), 
                     a.As=seq(0, 30, length=7), a.As.trt=seq(0, 30, length=7), 
                     beta.sd=1:3, epsilon.sd=1:3)
param$nplot[param$nfern == 39] <- 12
param$nplot[param$nfern == 24] <- 18
param$nplot[param$nfern == 9] <- 36

#param with small values to troubleshoot size
#param <- expand.grid(nfern=c(39, 24, 9), nplot = 0, a.t=1:3, a.trt=seq(0, 1, length=2), a.As=seq(0, 1, length=2), a.As.trt=seq(0, 1, length=2), beta.sd=1:3, epsilon.sd=1:3)

#param without nplot - see troubleshooting comments below:
# param <- expand.grid(nfern=c(39, 24, 9), a.t=1:2, a.trt=seq(0, 1, length=3), 
#                      a.As=0:1, a.As.trt=0:1, 
#                      beta.sd=1:2, epsilon.sd=1:2)

## a.As.trt (the interaction) must be no greater than the main effect (a.trt)
param <- param[param$a.As.trt <= param$a.trt, ]


##Questions for Part 2: When are we incorporating number of ferns to be sampled, vs. number of replicates? nplot does not depend on nfern to be sampled, but our ability to detect within plot variation does depend on nfern to be sampled?


##Part 3:
## now we need a function to loop over param and run the simulation for each combination.
## here's a skeleton of that function with some notes below on helpful things

run.pwrAnal <- function(nsim) {
    ## everything in this function needs to be done for each row of param, so right away we need to enter
    ## some kind of for loop kind of construct.  for loops are slow in R so we'll use one of the apply 
    ## functions (see below for details)
    out <- mclapply(1:nrow(param), mc.cores = 6, FUN = function(i) {
        ## right now there is just a simple function (sum) that we're applying to each row of param
        this.row <- param[i, ]
        #sum(this.row)
        
        ## the real function (which you type below these comments) will need to do the following:
        ## 1: for a.trt and a.As.trt, these need to be vectors of length 5, so make a sequence using
        ##    the a.trt and a.As.trt elements of this.row (you can access them with the $, like this:
        ##    this.row$a.trt) 
        
        ## Question: why do we need to make these vectors here, when we already have values for a.trt and a.As.trt in param? Answer: because we have 1 number, a difference in the value, and we want to expand that difference into 5 values. Remember, a.As is only a scaler and does not need to be expanded. 
        
        this.a.trt <- seq(from=this.row$a.trt, by=this.row$a.trt, length=5)
        this.a.As.trt <- seq(from=this.row$a.As.trt, by=this.row$a.As.trt, length=5)
        
        ## 2: using the 5 long vectors you made in step 1 and the other elements of this.row, plug
        ##    those into run.1sim, that's it, you're done!
        
        ## indexing the arguments in the fucntion run.1sim to the values in this.row of param vector
        ## param <- expand.grid(a.t=1:3, a.trt=1:5, nfern=c(2, 3, 6), a.As=1:7, a.As.trt=5, beta.sd=1:3, epsilon.sd=1:3)

        
        run.1sim(nsim=nsim, nfern=this.row$nfern, nplot=this.row$nplot, a.t=this.row$a.t, a.trt=this.a.trt, a.As=this.row$a.As, a.As.trt=this.a.As.trt, beta.sd=this.row$beta.sd, epsilon.sd=this.row$epsilon.sd)
      #browser()
    })
    unlist(out)
}
out <- run.pwrAnal(nsim=100)


pwr <- aggregate(list(pwr=out), list(nplot=param$nplot, a.trt=param$a.trt), mean)

pdf('~/Dropbox/Research/R_help/arsenic_power_analysis/fig_powerAnal.pdf', width = 5, height = 5)
palette(hsv(c(0.5, 0, 0.1), c(1, 0, 1), c(1, 0, 1)))
par(mar = c(4, 4, 0, 0) + 0.5)
plot(pwr$a.trt, pwr$pwr, col=as.factor(pwr$nplot), 
     xlab = 'As treatment effect size', ylab = 'Power', pch = 16, cex = 1.5,
     cex.lab = 1.5)
legend('topleft', legend = paste('nplot', levels(as.factor(pwr$nplot)), sep = ' =  '), 
       col = as.factor(levels(as.factor(pwr$nplot))), pch = 16, bty = 'n', cex = 1.5)
dev.off()
