## package needed for mixed effects
library(lme4)

## function that makes the model matrix for a specific set of conditions (i.e.
## for a specific set of coefficient values and numbers of ferns and plots)
## function arguments are:
## a.t = time effect (length 1, equal to difference between the two times)
## a.trt = treatment effect (length 7, each value is the difference between that treatment and the control)
## a.As = main effect of As in soil on As in ferns
## a.As.trt = interaction of treatments with As relationship (length 7)
## nfern = number of ferns per plot
## nplot = number of plots

make.mm <- function(a.t, a.trt, a.As, a.As.trt, nfern, nplot) {
    
}

## function to generate data using matrix multiplication,
## run linear model on that simulated data and extract p-values
## function arguments are:
## shared arguments with make.mm, as `sim.lm' calls `make.mm'
## beta.sd = standard deviation of the random plot effect
## epsilon.sd = standard deviation of the residuals

sim.lm <- function(a.t, a.trt, a.As, a.As.trt, beta.sd, epsilon.sd, nfern, nplot) {
    ## the model matrix, remember that column 1 is the intercept (i.e. all 1's)
    mm <- make.mm(a.t, a.trt, a.As, a.As.trt, nfern, nplot)
    
    ## plot random effect
    plt <- rep(rnorm(nplot, sd=beta.sd), each=nfern)
    
    ## residuals
    epsilon <- rnorm(nfern*nplot, sd=epsilon.sd)
    
    ## response variable
    y <- mm %*% c(a.t, a.trt, a.As, a.As.trt) + plt + epsilon
    
    ## time step, treat and plot number for use in lmer
    time.step <- as.factor(mm[, 2] + 1)
    treat <- as.factor(mm[, 3:9] %*% 1:7 + 1)
    pltID <- as.factor(rep(1:nplot, each=nfern))
    
    ## fit models
    mod.full <- lmer(y ~ time.step + treat*x + (1|pltID), REML=FALSE)
    mod.null <- lmer(y ~ time.step + x + (1|pltID), REML=FALSE)
    
    ## return p-val from likelihood ratio test (null is that `treat' doesn't matter)
    return(anova(mod.null, mod.full)[['Pr(>Chisq)']][2])
}
