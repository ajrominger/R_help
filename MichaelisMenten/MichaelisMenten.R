## ================================================
## functions to fit and plot Michaelis–Menten curve
## ================================================


## function to calculate the curve as a function of 
## substrate concentration, Vmax, and Km

mmcurve <- function(S, Vmax, Km) {
    Vmax * S / (Km + S)
}


## function to find best values for Vmax and Km given
## data on v and S

mmsol <- function(v, S) {
    ## sum of squares function to be minimized
    ss <- function(par) {
        sum((mmcurve(S, par[1], par[2]) - v)^2)
    }
    
    ## use Hanes–Woolf linearization to get initial guess
    SoverV <- S/v
    init <- lm(SoverV ~ S)
    initVmax <- 1/init$coefficients[2]
    initKm <- init$coefficients[1] * initVmax
    
    ## combine initial estimates for use in optimization
    initPar <- c(initVmax, initKm)
    names(initPar) <- c('Vmax', 'Km')
    
    ## minimize the sum of squares funciton
    sol <- optim(initPar, ss, method = 'L-BFGS-B', lower = 0)
    
    ## check optimization convereged
    if(sol$convergence > 0) {
        ## if it didn't, return Hanes–Woolf estimate
        warning('cannot fit Michaelis–Menten curve')
        return(c(NA, NA))
    } else {
        ## if it did, return the optimized estimates
        return(sol$par)
    }
}

## =========================================================
## bootstrap method to get confidence intervals on estimates
## =========================================================

mmboot <- function(v, S, n = 1000) {
    boot <- replicate(n, {
        i <- sample(length(S), replace = TRUE)
        newS <- S[i]
        newv <- v[i]
        
        return(mmsol(newv, newS))
    })
    
    ## estimate from non-bootstrapped data
    est <- mmsol(v, S)
    
    ## remove spuriously large outliers
    boot <- boot[, boot[1, ] <= est + est - min(boot[1, ], na.rm = TRUE)]
    
    plot(density(boot[1, ], na.rm = TRUE))
    
    out <- t(apply(boot, 1, function(x) c(median(x, na.rm = TRUE), 
                                          quantile(x, prob = c(0.025, 0.975), na.rm = TRUE))))
    colnames(out) <- c('est', 'ci_2.5', 'ci_97.5')
    
    ## make est the real estimate
    out[, 1] <- est
    
    return(out)
}


## ====================================
## examples of how to use the functions
## ====================================

# ## first simulate some data
# data <- data.frame(S = seq(0.1, 0.9, length = 10),
#                    v = mmcurve(seq(0.1, 0.9, length = 10), 10, 0.2))
# data$v <- data$v + rnorm(nrow(data), 0, 0.25)
# 
# ## plot it
# plot(data)
# 
# ## find Vmax and Km
# pars <- mmsol(data$v, data$S)
# 
# ## find confidence intervals
# mmboot(data$v, data$S)
# 
# ## add them to plot
# curve(mmcurve(x, Vmax = pars['Vmax'], Km = pars['Km']), add = TRUE)
# 
# ## return the predicted mm values and add to data
# data$pred <- mmcurve(data$S, Vmax = pars['Vmax'], Km = pars['Km'])
